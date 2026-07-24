;;; lr-fedi.el --- Read the @root@lr0.org fediverse -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A read-only client for the @root@lr0.org fediverse.  This is the *reading*
;; counterpart to `salih/add-microblog-to-hugo' (authoring lives in lr-tools.el);
;; this module never posts.  Two rich, evil-friendly views:
;;
;;   `SPC o m'  timeline      — posts/boosts from the accounts you follow
;;                              (${base}/admin/timeline.json)
;;   `SPC o n'  notifications — mentions, replies, likes, boosts, and new
;;                              followers directed at you
;;                              (${base}/admin/notifications.json)
;;   `SPC o p'  post          — compose a fediverse-ONLY post (not the blog),
;;                              published via ${base}/admin/publish
;;
;; Both render into a read-only buffer with per-entry navigation and actions:
;;   n / p        next / previous entry
;;   RET / o      open the entry (post / source) in a browser
;;   a            open the author's profile
;;   y            copy the entry's link
;;   gr           refresh
;;   q            quit
;; The timeline adds:
;;   u            unfollow the author of the entry at point (with confirmation)
;;
;; Only built-ins are used (url.el + json + shr) — no request.el / plz.el,
;; matching the rest of the config.
;;
;; Token: never hardcoded.  Resolved (in order) from
;;   1. auth-source  (machine lr0.org login admin password <TOKEN>)
;;   2. env var LR0_ADMIN_TOKEN
;;   3. defcustom `salih/fedi-admin-token'
;; Preferred: add this line to ~/.authinfo.gpg
;;   machine lr0.org login admin password <TOKEN>
;;
;;; Code:

(require 'url)
(require 'auth-source)
(require 'shr)
(require 'subr-x)
(require 'seq)
(require 'json)
(require 'iso8601)
(require 'browse-url)

(defgroup salih/fedi nil
  "Read the @root@lr0.org fediverse."
  :group 'applications
  :prefix "salih/fedi-")

(defcustom salih/fedi-base-url "https://lr0.org"
  "Base URL of the fediverse instance to read from."
  :type 'string
  :group 'salih/fedi)

(defcustom salih/fedi-admin-token nil
  "Fallback admin bearer token.
Prefer ~/.authinfo.gpg or the LR0_ADMIN_TOKEN environment variable."
  :type '(choice (const :tag "None" nil) string)
  :group 'salih/fedi)

(defcustom salih/fedi-timeline-limit 50
  "Number of timeline items to request from the admin endpoint."
  :type 'integer
  :group 'salih/fedi)

(defcustom salih/fedi-notifications-limit 50
  "Number of notifications to request from the admin endpoint."
  :type 'integer
  :group 'salih/fedi)

(defcustom salih/fedi-excerpt-width 100
  "Maximum characters of a quoted post excerpt shown as context."
  :type 'integer
  :group 'salih/fedi)

;;; --- Token -----------------------------------------------------------------

(defun salih/fedi--host ()
  "Return the bare host of `salih/fedi-base-url' (e.g. \"lr0.org\")."
  (or (url-host (url-generic-parse-url salih/fedi-base-url)) "lr0.org"))

(defun salih/fedi--token ()
  "Return the admin bearer token, or signal a helpful error."
  (let* ((host (salih/fedi--host))
         (token (or (auth-source-pick-first-password :host host :user "admin")
                    (getenv "LR0_ADMIN_TOKEN")
                    salih/fedi-admin-token)))
    (or (and (stringp token) (not (string-empty-p token)) token)
        (user-error
         (concat "No fediverse admin token found.  Add this line to "
                 "~/.authinfo.gpg:\n  machine %s login admin password <TOKEN>\n"
                 "or set the LR0_ADMIN_TOKEN env var, or `salih/fedi-admin-token'.")
         host))))

;;; --- Formatting helpers ----------------------------------------------------

(defun salih/fedi--html-to-text (html)
  "Render HTML content into readable, trimmed plain text via `shr'."
  (if (or (null html) (not (stringp html)) (string-empty-p html))
      ""
    (string-trim
     (with-temp-buffer
       (insert html)
       (let ((shr-use-fonts nil)
             (shr-width most-positive-fixnum)
             (shr-inhibit-images t))
         (shr-render-region (point-min) (point-max)))
       (buffer-substring-no-properties (point-min) (point-max))))))

(defun salih/fedi--shorten-actor (actor)
  "Strip the leading scheme from ACTOR for a compact display."
  (if (stringp actor)
      (replace-regexp-in-string "\\`https?://" "" actor)
    "unknown"))

(defun salih/fedi--format-time (iso)
  "Format ISO-8601 string ISO as local `YYYY-MM-DD HH:MM'; fall back to ISO."
  (or (and (stringp iso) (not (string-empty-p iso))
           (ignore-errors
             (format-time-string "%Y-%m-%d %H:%M" (encode-time (iso8601-parse iso)))))
      (or iso "")))

(defun salih/fedi--excerpt (text)
  "Trim TEXT to `salih/fedi-excerpt-width' chars, adding an ellipsis if cut."
  (let ((s (string-trim (or text ""))))
    (if (> (length s) salih/fedi-excerpt-width)
        (concat (substring s 0 salih/fedi-excerpt-width) "…")
      s)))

(defun salih/fedi--indent (text)
  "Indent every line of TEXT by two spaces."
  (concat "  " (replace-regexp-in-string "\n" "\n  " (string-trim-right text))))

;;; --- Item field helpers ----------------------------------------------------

(defun salih/fedi--item-timestamp (item)
  "Return a timestamp string for ITEM, or the empty string."
  (or (alist-get 'publishedAt item)
      (alist-get 'receivedAt item)
      (alist-get 'published item)
      (alist-get 'createdAt item)
      ""))

(defun salih/fedi--item-boosted-p (item)
  "Return non-nil when ITEM represents a boost."
  (let ((kind (alist-get 'kind item)))
    (and (stringp kind) (string= kind "boost"))))

(defun salih/fedi--item-actor (item)
  "Return the raw actor URI for ITEM."
  (or (alist-get 'actor item)
      (alist-get 'attributedTo item)
      (alist-get 'account item)
      "unknown"))

(defun salih/fedi--item-handle (item)
  "Return a display @user@host handle for ITEM."
  (or (alist-get 'actorHandle item)
      (salih/fedi--shorten-actor (salih/fedi--item-actor item))))

(defun salih/fedi--item-content (item)
  "Return ITEM's content rendered to readable plain text."
  (salih/fedi--html-to-text
   (or (alist-get 'contentHtml item)
       (alist-get 'content item)
       (alist-get 'text item)
       "")))

;;; --- HTTP ------------------------------------------------------------------

(defun salih/fedi--parse-buffer ()
  "Parse the current `url-retrieve-synchronously' buffer.
Return a cons (STATUS . BODY-STRING)."
  (goto-char (point-min))
  (unless (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
    (user-error "Malformed HTTP response from %s" salih/fedi-base-url))
  (let ((status (string-to-number (match-string 1))))
    (goto-char (point-min))
    (if (re-search-forward "\n\r?\n" nil t)
        (cons status (buffer-substring-no-properties (point) (point-max)))
      (cons status ""))))

(defun salih/fedi--fetch-json (url)
  "GET URL with the admin bearer token and return the parsed items list.
Accepts a bare JSON array or an object wrapping the list under
`items'/`timeline'/`orderedItems'."
  (let* ((token (salih/fedi--token))
         (url-request-method "GET")
         (url-request-extra-headers
          (list (cons "Authorization" (concat "Bearer " token))
                (cons "Accept" "application/json")))
         (buf (url-retrieve-synchronously url t t 30)))
    (unless buf (user-error "No response from %s" url))
    (unwind-protect
        (with-current-buffer buf
          (set-buffer-multibyte t)
          (let* ((parsed (salih/fedi--parse-buffer))
                 (status (car parsed))
                 (body   (cdr parsed)))
            (cond
             ((= status 401)
              (user-error
               (concat "Fediverse: 401 Unauthorized — the admin token is missing "
                       "or wrong.  Fix ~/.authinfo.gpg: machine %s login admin "
                       "password <TOKEN>")
               (salih/fedi--host)))
             ((/= status 200)
              (user-error "Fediverse: HTTP %d from %s" status url))
             (t
              (let* ((decoded (decode-coding-string body 'utf-8))
                     (data (json-parse-string decoded
                                              :object-type 'alist
                                              :array-type 'list
                                              :null-object nil
                                              :false-object nil)))
                (cond
                 ((listp data)
                  (or (and (consp (car data))
                           (symbolp (caar data))
                           (or (alist-get 'items data)
                               (alist-get 'timeline data)
                               (alist-get 'orderedItems data)))
                      data))
                 (t nil)))))))
      (kill-buffer buf))))

(defun salih/fedi--post-json (path payload)
  "POST PAYLOAD (an alist) as JSON to PATH with the admin token.
Return t on a 2xx response; signal a `user-error' otherwise."
  (let* ((token (salih/fedi--token))
         (url (concat (string-remove-suffix "/" salih/fedi-base-url) path))
         (url-request-method "POST")
         (url-request-extra-headers
          (list (cons "Authorization" (concat "Bearer " token))
                (cons "Content-Type" "application/json")))
         (url-request-data (encode-coding-string (json-encode payload) 'utf-8))
         (buf (url-retrieve-synchronously url t t 30)))
    (unless buf (user-error "No response from %s" url))
    (unwind-protect
        (with-current-buffer buf
          (let ((status (car (salih/fedi--parse-buffer))))
            (unless (and (>= status 200) (< status 300))
              (user-error "Fediverse: HTTP %d from %s" status url))
            t))
      (kill-buffer buf))))

;;; --- Faces -----------------------------------------------------------------

(defface salih/fedi-mention-face '((t :inherit warning :weight bold))
  "Badge face for mentions." :group 'salih/fedi)
(defface salih/fedi-reply-face '((t :inherit success :weight bold))
  "Badge face for replies." :group 'salih/fedi)
(defface salih/fedi-like-face '((t :inherit error :weight bold))
  "Badge face for likes." :group 'salih/fedi)
(defface salih/fedi-boost-face '((t :inherit font-lock-keyword-face :weight bold))
  "Badge face for boosts." :group 'salih/fedi)
(defface salih/fedi-follow-face '((t :inherit font-lock-function-name-face :weight bold))
  "Badge face for new followers." :group 'salih/fedi)
(defface salih/fedi-actor-face '((t :inherit bold))
  "Face for the actor handle." :group 'salih/fedi)
(defface salih/fedi-time-face '((t :inherit shadow))
  "Face for timestamps." :group 'salih/fedi)
(defface salih/fedi-context-face '((t :inherit font-lock-comment-face))
  "Face for the quoted-post context line." :group 'salih/fedi)

;;; --- Shared entry infrastructure -------------------------------------------

(defvar-local salih/fedi--entry-positions nil
  "Sorted buffer positions where each entry begins, for n/p navigation.")

(defun salih/fedi--context-line (label plain)
  "Return an indented `  > LABEL: \"excerpt\"' context string for PLAIN text."
  (propertize (format "  › %s: \"%s\"\n" label (salih/fedi--excerpt plain))
              'face 'salih/fedi-context-face))

(defun salih/fedi--entry-data ()
  "Return the plist of data for the entry at point, or nil."
  (get-text-property (point) 'fedi-data))

(defun salih/fedi--entry-best-url ()
  "Return the most relevant URL for the entry at point."
  (let ((d (salih/fedi--entry-data)))
    (and d (or (plist-get d :source-url)
               (plist-get d :target-url)
               (plist-get d :author-url)))))

(defun salih/fedi-next ()
  "Move point to the next entry."
  (interactive)
  (let ((next (seq-find (lambda (p) (> p (point))) salih/fedi--entry-positions)))
    (if next (goto-char next) (message "No more entries"))))

(defun salih/fedi-prev ()
  "Move point to the previous entry."
  (interactive)
  (let ((prev (seq-find (lambda (p) (< p (point)))
                        (reverse salih/fedi--entry-positions))))
    (if prev (goto-char prev) (message "At first entry"))))

(defun salih/fedi-open ()
  "Open the most relevant link for the entry at point in a browser."
  (interactive)
  (let ((url (salih/fedi--entry-best-url)))
    (if url (browse-url url) (user-error "No link for this entry"))))

(defun salih/fedi-open-author ()
  "Open the author's profile for the entry at point in a browser."
  (interactive)
  (let* ((d (salih/fedi--entry-data))
         (url (and d (plist-get d :author-url))))
    (if url (browse-url url) (user-error "No author link for this entry"))))

(defun salih/fedi-copy-link ()
  "Copy the most relevant link for the entry at point to the kill ring."
  (interactive)
  (let ((url (salih/fedi--entry-best-url)))
    (if url (progn (kill-new url) (message "Copied: %s" url))
      (user-error "No link for this entry"))))

;;; --- Timeline --------------------------------------------------------------

(defun salih/fedi--timeline-url ()
  "Return the full timeline endpoint URL."
  (format "%s/admin/timeline.json?limit=%d"
          (string-remove-suffix "/" salih/fedi-base-url)
          salih/fedi-timeline-limit))

(defun salih/fedi--insert-timeline-item (item)
  "Insert one timeline ITEM (an alist) at point, richly formatted."
  (let* ((actor    (salih/fedi--item-actor item))
         (handle   (salih/fedi--item-handle item))
         (boosted  (salih/fedi--item-boosted-p item))
         (ts       (salih/fedi--format-time (salih/fedi--item-timestamp item)))
         (content  (salih/fedi--item-content item))
         (in-reply (alist-get 'inReplyTo item))
         (src      (or (alist-get 'url item) (alist-get 'object_uri item)))
         (start    (point)))
    (insert (propertize handle 'face 'salih/fedi-actor-face))
    (when boosted (insert (propertize "  ↻ boosted" 'face 'salih/fedi-boost-face)))
    (unless (string-empty-p ts)
      (insert (propertize (format "  %s" ts) 'face 'salih/fedi-time-face)))
    (insert "\n")
    (when (and (stringp in-reply) (not (string-empty-p in-reply)))
      (insert (salih/fedi--context-line "in reply to" (salih/fedi--shorten-actor in-reply))))
    (if (string-empty-p content)
        (insert (propertize "  (no text)\n" 'face 'shadow))
      (insert (salih/fedi--indent content) "\n"))
    (insert "\n")
    (add-text-properties start (point)
                         (list 'fedi-data (list :source-url src :author-url actor)))
    (push start salih/fedi--entry-positions)))

(defun salih/fedi--timeline-header (items)
  "Return a header-line string for the timeline ITEMS."
  (if (null items)
      " Fedi timeline — empty  ·  gr refresh · q quit "
    (format " %d posts  ·  n/p move · RET open · a author · u unfollow · y copy · gr refresh · q quit "
            (length items))))

(defun salih/fedi--render-timeline (items)
  "Render ITEMS (a list of alists) into the `*fedi-timeline*' buffer."
  (let ((buf (get-buffer-create "*fedi-timeline*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (salih/fedi-timeline-mode)
        (setq salih/fedi--entry-positions nil)
        (if (null items)
            (insert (propertize "  Nothing in the timeline yet.\n" 'face 'shadow))
          (dolist (item items) (salih/fedi--insert-timeline-item item)))
        (setq salih/fedi--entry-positions (nreverse salih/fedi--entry-positions))
        (setq header-line-format (salih/fedi--timeline-header items))
        (goto-char (point-min))))
    (pop-to-buffer buf)))

;;;###autoload
(defun salih/fedi-timeline ()
  "Fetch and display the @root@lr0.org fediverse reading timeline."
  (interactive)
  (salih/fedi--render-timeline (salih/fedi--fetch-json (salih/fedi--timeline-url))))

(defun salih/fedi-unfollow ()
  "Unfollow the author of the timeline entry at point (with confirmation)."
  (interactive)
  (let* ((d (salih/fedi--entry-data))
         (actor (and d (plist-get d :author-url))))
    (unless actor (user-error "No author to unfollow at point"))
    (when (yes-or-no-p (format "Unfollow %s? " (salih/fedi--shorten-actor actor)))
      (salih/fedi--post-json "/admin/unfollow" (list (cons "actor" actor)))
      (message "Unfollowed %s" (salih/fedi--shorten-actor actor))
      (salih/fedi-timeline))))

(defvar salih/fedi-timeline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n")   #'salih/fedi-next)
    (define-key map (kbd "p")   #'salih/fedi-prev)
    (define-key map (kbd "TAB") #'salih/fedi-next)
    (define-key map (kbd "RET") #'salih/fedi-open)
    (define-key map (kbd "o")   #'salih/fedi-open)
    (define-key map (kbd "a")   #'salih/fedi-open-author)
    (define-key map (kbd "u")   #'salih/fedi-unfollow)
    (define-key map (kbd "y")   #'salih/fedi-copy-link)
    (define-key map (kbd "g")   #'salih/fedi-timeline)
    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `salih/fedi-timeline-mode'.")

(define-derived-mode salih/fedi-timeline-mode special-mode "Fedi"
  "Major mode for reading the @root@lr0.org fediverse timeline.
\\{salih/fedi-timeline-mode-map}"
  (setq-local truncate-lines nil)
  (setq-local salih/fedi--entry-positions nil)
  (buffer-disable-undo))

(when (featurep 'evil)
  (evil-set-initial-state 'salih/fedi-timeline-mode 'normal))

;;; --- Notifications ---------------------------------------------------------

(defun salih/fedi--notifications-url ()
  "Return the full notifications endpoint URL."
  (format "%s/admin/notifications.json?limit=%d"
          (string-remove-suffix "/" salih/fedi-base-url)
          salih/fedi-notifications-limit))

(defun salih/fedi--notif-badge (type)
  "Return a cons (LABEL . FACE) for notification TYPE."
  (pcase type
    ("mention"  (cons "MENTION" 'salih/fedi-mention-face))
    ("reply"    (cons "REPLY"   'salih/fedi-reply-face))
    ("like"     (cons "LIKE"    'salih/fedi-like-face))
    ("announce" (cons "BOOST"   'salih/fedi-boost-face))
    ("follow"   (cons "FOLLOW"  'salih/fedi-follow-face))
    (_          (cons (upcase (or type "EVENT")) 'default))))

(defun salih/fedi--insert-notification (item)
  "Insert one notification ITEM (an alist) at point, richly formatted."
  (let* ((type       (alist-get 'type item))
         (badge      (salih/fedi--notif-badge type))
         (handle     (salih/fedi--item-handle item))
         (ts         (salih/fedi--format-time (salih/fedi--item-timestamp item)))
         (content    (salih/fedi--item-content item))
         (target     (alist-get 'target item))
         (target-txt (and target (salih/fedi--html-to-text
                                   (or (alist-get 'contentHtml target) ""))))
         (source-url (alist-get 'url item))
         (target-url (and target (alist-get 'url target)))
         (author-url (salih/fedi--item-actor item))
         (start      (point)))
    (insert (propertize (format " %-7s " (car badge)) 'face (cdr badge))
            " " (propertize handle 'face 'salih/fedi-actor-face)
            (propertize (format "  %s\n" ts) 'face 'salih/fedi-time-face))
    (pcase type
      ("like"
       (when target-txt (insert (salih/fedi--context-line "liked your post" target-txt))))
      ("announce"
       (when target-txt (insert (salih/fedi--context-line "boosted your post" target-txt))))
      ("follow"
       (insert (propertize "  started following you\n" 'face 'italic)))
      ("reply"
       (when target-txt (insert (salih/fedi--context-line "in reply to" target-txt)))
       (unless (string-empty-p content) (insert (salih/fedi--indent content) "\n")))
      ("mention"
       (unless (string-empty-p content) (insert (salih/fedi--indent content) "\n")))
      (_
       (unless (string-empty-p content) (insert (salih/fedi--indent content) "\n"))))
    (insert "\n")
    (add-text-properties start (point)
                         (list 'fedi-data (list :source-url source-url
                                                :target-url target-url
                                                :author-url author-url)))
    (push start salih/fedi--entry-positions)))

(defun salih/fedi--notif-header (items)
  "Return a header-line string summarising notification ITEMS."
  (if (null items)
      " Fedi notifications — empty  ·  gr refresh · q quit "
    (let ((counts (make-hash-table :test 'equal)) parts)
      (dolist (it items)
        (let ((ty (alist-get 'type it)))
          (puthash ty (1+ (gethash ty counts 0)) counts)))
      (maphash (lambda (k v)
                 (push (format "%d %s" v (downcase (car (salih/fedi--notif-badge k)))) parts))
               counts)
      (format " %d notifications  ·  %s  ·  n/p move · RET open · a author · y copy · gr refresh · q quit "
              (length items) (mapconcat #'identity (nreverse parts) ", ")))))

(defun salih/fedi--render-notifications (items)
  "Render ITEMS (a list of alists) into the `*fedi-notifications*' buffer."
  (let ((buf (get-buffer-create "*fedi-notifications*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (salih/fedi-notifications-mode)
        (setq salih/fedi--entry-positions nil)
        (if (null items)
            (insert
             (propertize "  No notifications yet.\n\n" 'face 'shadow)
             "  Mentions, replies, likes, boosts and new followers directed\n"
             "  at you will appear here.  Press `gr' to refresh.\n")
          (dolist (item items) (salih/fedi--insert-notification item)))
        (setq salih/fedi--entry-positions (nreverse salih/fedi--entry-positions))
        (setq header-line-format (salih/fedi--notif-header items))
        (goto-char (point-min))))
    (pop-to-buffer buf)))

;;;###autoload
(defun salih/fedi-notifications ()
  "Fetch and display @root@lr0.org fediverse notifications.
Shows mentions, replies, likes, boosts, and new followers newest-first."
  (interactive)
  (salih/fedi--render-notifications
   (salih/fedi--fetch-json (salih/fedi--notifications-url))))

(defvar salih/fedi-notifications-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n")   #'salih/fedi-next)
    (define-key map (kbd "p")   #'salih/fedi-prev)
    (define-key map (kbd "TAB") #'salih/fedi-next)
    (define-key map (kbd "RET") #'salih/fedi-open)
    (define-key map (kbd "o")   #'salih/fedi-open)
    (define-key map (kbd "a")   #'salih/fedi-open-author)
    (define-key map (kbd "y")   #'salih/fedi-copy-link)
    (define-key map (kbd "g")   #'salih/fedi-notifications)
    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `salih/fedi-notifications-mode'.")

(define-derived-mode salih/fedi-notifications-mode special-mode "FediNotif"
  "Major mode for reading @root@lr0.org fediverse notifications.
\\{salih/fedi-notifications-mode-map}"
  (setq-local truncate-lines nil)
  (setq-local salih/fedi--entry-positions nil)
  (buffer-disable-undo))

(when (featurep 'evil)
  (evil-set-initial-state 'salih/fedi-notifications-mode 'normal))

;;; --- Compose (fediverse-only post) -----------------------------------------
;;
;; Publishes ONLY to the fediverse (via ${base}/admin/publish) — it never
;; touches the Hugo blog.  This is distinct from `salih/add-microblog-to-hugo',
;; which authors a blog micropost that then syndicates to the fediverse.

(defun salih/fedi--escape-html (s)
  "Escape &, <, > in S for safe HTML embedding."
  (replace-regexp-in-string
   "[<>&]"
   (lambda (c) (pcase c ("<" "&lt;") (">" "&gt;") ("&" "&amp;") (_ c)))
   s t t))

(defun salih/fedi--text-to-html (text)
  "Convert plain TEXT to minimal HTML: blank lines split paragraphs, newlines
become <br>. TEXT is HTML-escaped first."
  (mapconcat
   (lambda (para)
     (concat "<p>"
             (replace-regexp-in-string
              "\n" "<br>" (salih/fedi--escape-html (string-trim para)))
             "</p>"))
   (split-string (string-trim text) "\n[ \t]*\n" t)
   ""))

(defvar salih/fedi-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'salih/fedi-compose-send)
    (define-key map (kbd "C-c C-k") #'salih/fedi-compose-cancel)
    map)
  "Keymap for `salih/fedi-compose-mode'.")

(define-derived-mode salih/fedi-compose-mode text-mode "FediCompose"
  "Major mode for composing a fediverse-only post."
  (setq-local header-line-format
              " Fediverse-only post  ·  C-c C-c publish · C-c C-k cancel "))

;;;###autoload
(defun salih/fedi-post ()
  "Compose and publish a post to the fediverse ONLY (not the blog).
Opens a compose buffer; `C-c C-c' publishes via /admin/publish, `C-c C-k'
cancels."
  (interactive)
  (let ((buf (get-buffer-create "*fedi-compose*")))
    (with-current-buffer buf
      (erase-buffer)
      (salih/fedi-compose-mode))
    (pop-to-buffer buf)
    (message "Write your post, then C-c C-c to publish to the fediverse.")))

(defun salih/fedi-compose-send ()
  "Publish the compose buffer to the fediverse and close it."
  (interactive)
  (let ((text (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
    (when (string-empty-p text) (user-error "Nothing to post"))
    (salih/fedi--post-json "/admin/publish"
                           (list (cons "content" (salih/fedi--text-to-html text))))
    (quit-window t)
    (message "Published to the fediverse.")))

(defun salih/fedi-compose-cancel ()
  "Abort composing without publishing."
  (interactive)
  (quit-window t)
  (message "Fediverse post cancelled."))

;;; --- Keybindings -----------------------------------------------------------

(map! :leader
      :desc "Fedi timeline" "o m" #'salih/fedi-timeline
      :desc "Fedi notifications" "o n" #'salih/fedi-notifications
      :desc "Fedi post (fedi-only)" "o p" #'salih/fedi-post)

;; Evil-state bindings so single-key actions win over evil's normal/motion maps.
(map! :map salih/fedi-timeline-mode-map
      :nvm "n"       #'salih/fedi-next
      :nvm "p"       #'salih/fedi-prev
      :nvm "TAB"     #'salih/fedi-next
      :nvm "RET"     #'salih/fedi-open
      :nvm "o"       #'salih/fedi-open
      :nvm "a"       #'salih/fedi-open-author
      :nvm "u"       #'salih/fedi-unfollow
      :nvm "y"       #'salih/fedi-copy-link
      :nvm "gr"      #'salih/fedi-timeline
      :nvm "q"       #'quit-window)

(map! :map salih/fedi-notifications-mode-map
      :nvm "n"       #'salih/fedi-next
      :nvm "p"       #'salih/fedi-prev
      :nvm "TAB"     #'salih/fedi-next
      :nvm "RET"     #'salih/fedi-open
      :nvm "o"       #'salih/fedi-open
      :nvm "a"       #'salih/fedi-open-author
      :nvm "y"       #'salih/fedi-copy-link
      :nvm "gr"      #'salih/fedi-notifications
      :nvm "q"       #'quit-window)

(provide 'lr-fedi)
;;; lr-fedi.el ends here
