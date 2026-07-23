;;; lr-fedi.el --- Read the @root@lr0.org fediverse timeline -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A tiny read-only client for the @root@lr0.org fediverse.  This is the
;; *reading* counterpart to `salih/add-microblog-to-hugo' (authoring lives in
;; lr-tools.el); this module never posts.  Two views:
;;
;;   `SPC o m'  timeline      — posts/boosts from the accounts you follow
;;                              (${base}/admin/timeline.json)
;;   `SPC o n'  notifications — mentions, replies, likes, boosts, and new
;;                              followers directed at you, newest-first
;;                              (${base}/admin/notifications.json)
;;
;; Both hit an admin JSON endpoint with a bearer token and render into a
;; read-only buffer.  Only built-ins are used (url.el + json-parse-string +
;; shr) — no request.el / plz.el, matching the rest of the config.
;;
;; Keybindings live under the existing "open" prefix.  The `o'-prefixed leader
;; keys already in use are a/c/e/f/i/l/o/t/v (see lr-editor.el, lr-elfeed.el,
;; lr-agent.el), so `m' (microblog) and `n' (notifications) are both free.
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
(require 'iso8601)
(require 'browse-url)

(defgroup salih/fedi nil
  "Read the @root@lr0.org fediverse timeline."
  :group 'applications
  :prefix "salih/fedi-")

(defcustom salih/fedi-base-url "https://lr0.org"
  "Base URL of the fediverse instance to read from.
The timeline is fetched from `${salih/fedi-base-url}/admin/timeline.json'."
  :type 'string
  :group 'salih/fedi)

(defcustom salih/fedi-admin-token nil
  "Fallback admin bearer token for the fediverse timeline.
Prefer storing the token in ~/.authinfo.gpg or in the LR0_ADMIN_TOKEN
environment variable; this defcustom is only the last resort.  See
`salih/fedi--token'."
  :type '(choice (const :tag "None" nil) string)
  :group 'salih/fedi)

(defcustom salih/fedi-timeline-limit 50
  "Number of timeline items to request from the admin endpoint."
  :type 'integer
  :group 'salih/fedi)

(defun salih/fedi--host ()
  "Return the bare host of `salih/fedi-base-url' (e.g. \"lr0.org\")."
  (or (url-host (url-generic-parse-url salih/fedi-base-url))
      "lr0.org"))

(defun salih/fedi--token ()
  "Return the admin bearer token, or signal a helpful error.
Resolution order: auth-source, then the LR0_ADMIN_TOKEN environment
variable, then `salih/fedi-admin-token'."
  (let* ((host (salih/fedi--host))
         (token (or (auth-source-pick-first-password :host host :user "admin")
                    (getenv "LR0_ADMIN_TOKEN")
                    salih/fedi-admin-token)))
    (or (and (stringp token) (not (string-empty-p token)) token)
        (user-error
         (concat "No fediverse admin token found.  Add this line to "
                 "~/.authinfo.gpg:\n"
                 "  machine %s login admin password <TOKEN>\n"
                 "or set the LR0_ADMIN_TOKEN env var, "
                 "or `salih/fedi-admin-token'.")
         host))))

;;; --- HTML -> readable text -------------------------------------------------

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

;;; --- Item field helpers ----------------------------------------------------

(defun salih/fedi--shorten-actor (actor)
  "Strip the leading scheme from ACTOR for a compact display."
  (if (stringp actor)
      (replace-regexp-in-string "\\`https?://" "" actor)
    "unknown"))

(defun salih/fedi--item-timestamp (item)
  "Return a human timestamp string for ITEM, or the empty string."
  (or (alist-get 'publishedAt item)
      (alist-get 'published item)
      (alist-get 'createdAt item)
      ""))

(defun salih/fedi--item-boosted-p (item)
  "Return non-nil when ITEM represents a boost."
  (let ((kind (alist-get 'kind item)))
    (and (stringp kind) (string= kind "boost"))))

(defun salih/fedi--item-actor (item)
  "Return the display actor for ITEM."
  (or (alist-get 'actor item)
      (alist-get 'attributedTo item)
      (alist-get 'account item)
      "unknown"))

(defun salih/fedi--item-content (item)
  "Return ITEM's content rendered to readable plain text."
  (salih/fedi--html-to-text
   (or (alist-get 'content item)
       (alist-get 'text item)
       "")))

;;; --- Rendering -------------------------------------------------------------

(defun salih/fedi--render (items)
  "Render ITEMS (a list of alists) into the `*fedi-timeline*' buffer."
  (let ((buf (get-buffer-create "*fedi-timeline*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (salih/fedi-timeline-mode)
        (if (null items)
            (insert "No items in the timeline.\n")
          (dolist (item items)
            (let ((actor   (salih/fedi--shorten-actor
                            (salih/fedi--item-actor item)))
                  (ts      (salih/fedi--item-timestamp item))
                  (boosted (salih/fedi--item-boosted-p item))
                  (content (salih/fedi--item-content item)))
              (insert (propertize actor 'face 'bold))
              (when boosted
                (insert (propertize "  [boosted]" 'face 'shr-strike-through)))
              (unless (string-empty-p ts)
                (insert (propertize (format "  %s" ts) 'face 'shadow)))
              (insert "\n")
              (unless (string-empty-p content)
                (insert content "\n"))
              (insert (make-string 60 ?-) "\n"))))
        (goto-char (point-min))))
    (pop-to-buffer buf)))

;;; --- Fetch -----------------------------------------------------------------

(defun salih/fedi--parse-buffer ()
  "Parse the current `url-retrieve-synchronously' buffer.
Return a cons (STATUS . BODY-STRING) where STATUS is the HTTP status code
as an integer, and BODY-STRING is the raw response body after the headers.
Signals a `user-error' if no HTTP status line is present."
  (goto-char (point-min))
  (unless (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
    (user-error "Malformed HTTP response from %s" salih/fedi-base-url))
  (let ((status (string-to-number (match-string 1))))
    (goto-char (point-min))
    ;; Skip past the header block to the body.
    (if (re-search-forward "\n\r?\n" nil t)
        (cons status (buffer-substring-no-properties (point) (point-max)))
      (cons status ""))))

(defun salih/fedi--timeline-url ()
  "Return the full timeline endpoint URL."
  (format "%s/admin/timeline.json?limit=%d"
          (string-remove-suffix "/" salih/fedi-base-url)
          salih/fedi-timeline-limit))

(defun salih/fedi--fetch-json (url)
  "GET URL with the admin bearer token and return the parsed items list.
Signals a `user-error' on auth failure or a non-200 response.  Accepts either
a bare JSON array or an object wrapping the list under
`items'/`timeline'/`orderedItems'.  Shared by the timeline and notifications
readers."
  (let* ((token (salih/fedi--token))
         (url-request-method "GET")
         (url-request-extra-headers
          (list (cons "Authorization" (concat "Bearer " token))
                (cons "Accept" "application/json")))
         (buf (url-retrieve-synchronously url t t 30)))
    (unless buf
      (user-error "No response from %s" url))
    (unwind-protect
        (with-current-buffer buf
          (set-buffer-multibyte t)
          (let* ((parsed (salih/fedi--parse-buffer))
                 (status (car parsed))
                 (body   (cdr parsed)))
            (cond
             ((= status 401)
              (user-error
               (concat "Fediverse: 401 Unauthorized — the admin token is "
                       "missing or wrong.  Fix the ~/.authinfo.gpg line: "
                       "machine %s login admin password <TOKEN>")
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
                ;; The endpoint may return either a bare array or an object
                ;; wrapping the list under `items'/`timeline'/`orderedItems'.
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

;;;###autoload
(defun salih/fedi-timeline ()
  "Fetch and display the @root@lr0.org fediverse reading timeline.
GETs `${salih/fedi-base-url}/admin/timeline.json' with a bearer token and
renders the result into the read-only `*fedi-timeline*' buffer."
  (interactive)
  (salih/fedi--render (salih/fedi--fetch-json (salih/fedi--timeline-url))))

;;; --- Notifications ---------------------------------------------------------
;;
;; A proper reader for things directed at you — mentions, replies, likes,
;; boosts, and new followers — with per-entry navigation (n/p), open-in-browser
;; (RET/o), open-author (a), and copy-link (y).  Works under evil (bindings are
;; registered for normal/visual/motion states).

(defcustom salih/fedi-notifications-limit 50
  "Number of notifications to request from the admin endpoint."
  :type 'integer
  :group 'salih/fedi)

(defcustom salih/fedi-excerpt-width 100
  "Maximum characters of a quoted post excerpt shown as context."
  :type 'integer
  :group 'salih/fedi)

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

(defvar-local salih/fedi--entry-positions nil
  "Sorted buffer positions where each notification entry begins.")

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

(defun salih/fedi--format-time (iso)
  "Format ISO-8601 string ISO as local `YYYY-MM-DD HH:MM'; fall back to ISO."
  (or (and (stringp iso) (not (string-empty-p iso))
           (ignore-errors
             (format-time-string "%Y-%m-%d %H:%M"
                                 (encode-time (iso8601-parse iso)))))
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

(defun salih/fedi--context-line (label plain)
  "Return an indented `  > LABEL: \"excerpt\"' context string for PLAIN text."
  (propertize (format "  › %s: \"%s\"\n" label (salih/fedi--excerpt plain))
              'face 'salih/fedi-context-face))

(defun salih/fedi--insert-notification (item)
  "Insert one notification ITEM (an alist) at point, richly formatted."
  (let* ((type       (alist-get 'type item))
         (badge      (salih/fedi--notif-badge type))
         (handle     (or (alist-get 'actorHandle item)
                         (salih/fedi--shorten-actor
                          (or (alist-get 'actor item) "unknown"))))
         (ts         (salih/fedi--format-time (alist-get 'receivedAt item)))
         (content    (salih/fedi--html-to-text (or (alist-get 'contentHtml item) "")))
         (target     (alist-get 'target item))
         (target-txt (and target (salih/fedi--html-to-text
                                   (or (alist-get 'contentHtml target) ""))))
         (source-url (alist-get 'url item))
         (target-url (and target (alist-get 'url target)))
         (author-url (alist-get 'actor item))
         (start      (point)))
    ;; Line 1:  [ BADGE ]  handle                              timestamp
    (insert (propertize (format " %-7s " (car badge)) 'face (cdr badge))
            " " (propertize handle 'face 'salih/fedi-actor-face)
            (propertize (format "  %s\n" ts) 'face 'salih/fedi-time-face))
    ;; Line(s) 2+: context and/or content, depending on the kind.
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
    ;; Attach per-entry data across the whole region so point-anywhere works.
    (add-text-properties
     start (point)
     (list 'fedi-data (list :source-url source-url
                            :target-url target-url
                            :author-url author-url)))
    (push start salih/fedi--entry-positions)))

(defun salih/fedi--notif-header (items)
  "Return a header-line string summarising ITEMS."
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
             "  at you will appear here.  Press `g' to refresh.\n")
          (dolist (item items) (salih/fedi--insert-notification item)))
        (setq salih/fedi--entry-positions (nreverse salih/fedi--entry-positions))
        (setq header-line-format (salih/fedi--notif-header items))
        (goto-char (point-min))))
    (pop-to-buffer buf)))

;;;###autoload
(defun salih/fedi-notifications ()
  "Fetch and display @root@lr0.org fediverse notifications.
Shows mentions, replies, likes, boosts, and new followers newest-first in the
read-only `*fedi-notifications*' buffer."
  (interactive)
  (salih/fedi--render-notifications
   (salih/fedi--fetch-json (salih/fedi--notifications-url))))

;;; --- Notifications: navigation & actions -----------------------------------

(defun salih/fedi-notif-next ()
  "Move point to the next notification entry."
  (interactive)
  (let ((next (seq-find (lambda (p) (> p (point))) salih/fedi--entry-positions)))
    (if next (goto-char next) (message "No more notifications"))))

(defun salih/fedi-notif-prev ()
  "Move point to the previous notification entry."
  (interactive)
  (let ((prev (seq-find (lambda (p) (< p (point)))
                        (reverse salih/fedi--entry-positions))))
    (if prev (goto-char prev) (message "At first notification"))))

(defun salih/fedi--entry-data ()
  "Return the plist of data for the notification entry at point, or nil."
  (get-text-property (point) 'fedi-data))

(defun salih/fedi--entry-best-url ()
  "Return the most relevant URL for the entry at point (source > target > author)."
  (let ((d (salih/fedi--entry-data)))
    (and d (or (plist-get d :source-url)
               (plist-get d :target-url)
               (plist-get d :author-url)))))

(defun salih/fedi-notif-open ()
  "Open the most relevant link for the notification at point in a browser."
  (interactive)
  (let ((url (salih/fedi--entry-best-url)))
    (if url (browse-url url) (user-error "No link for this notification"))))

(defun salih/fedi-notif-open-author ()
  "Open the author's profile for the notification at point in a browser."
  (interactive)
  (let* ((d (salih/fedi--entry-data))
         (url (and d (plist-get d :author-url))))
    (if url (browse-url url) (user-error "No author link for this notification"))))

(defun salih/fedi-notif-copy-link ()
  "Copy the most relevant link for the notification at point to the kill ring."
  (interactive)
  (let ((url (salih/fedi--entry-best-url)))
    (if url (progn (kill-new url) (message "Copied: %s" url))
      (user-error "No link for this notification"))))

;;; --- Notifications: mode ---------------------------------------------------

(defvar salih/fedi-notifications-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n")         #'salih/fedi-notif-next)
    (define-key map (kbd "p")         #'salih/fedi-notif-prev)
    (define-key map (kbd "TAB")       #'salih/fedi-notif-next)
    (define-key map (kbd "<backtab>") #'salih/fedi-notif-prev)
    (define-key map (kbd "RET")       #'salih/fedi-notif-open)
    (define-key map (kbd "o")         #'salih/fedi-notif-open)
    (define-key map (kbd "a")         #'salih/fedi-notif-open-author)
    (define-key map (kbd "y")         #'salih/fedi-notif-copy-link)
    (define-key map (kbd "g")         #'salih/fedi-notifications)
    (define-key map (kbd "r")         #'salih/fedi-notifications)
    (define-key map (kbd "q")         #'quit-window)
    map)
  "Keymap for `salih/fedi-notifications-mode'.")

(define-derived-mode salih/fedi-notifications-mode special-mode "FediNotif"
  "Major mode for reading @root@lr0.org fediverse notifications.
\\{salih/fedi-notifications-mode-map}"
  (setq-local truncate-lines nil)
  (setq-local salih/fedi--entry-positions nil)
  (buffer-disable-undo))

;; Evil: keep single-key actions working in normal/visual/motion states, and
;; start the buffer in normal state so the mnemonics are live immediately.
(when (featurep 'evil)
  (evil-set-initial-state 'salih/fedi-notifications-mode 'normal))

;;; --- Mode ------------------------------------------------------------------

(define-derived-mode salih/fedi-timeline-mode special-mode "Fedi"
  "Major mode for reading the @root@lr0.org fediverse timeline.
\\{salih/fedi-timeline-mode-map}"
  (setq-local truncate-lines nil)
  (buffer-disable-undo))

(define-key salih/fedi-timeline-mode-map (kbd "g") #'salih/fedi-timeline)
;; `q' is inherited from `special-mode' (quit-window).

;;; --- Keybinding ------------------------------------------------------------

(map! :leader
      :desc "Fedi timeline" "o m" #'salih/fedi-timeline
      :desc "Fedi notifications" "o n" #'salih/fedi-notifications)

;; Evil-state bindings for the notifications buffer (so the single-key actions
;; win over evil's normal/motion-state maps).  Registered via `evil-define-key'
;; under the hood, which takes precedence in buffers using the mode.
(map! :map salih/fedi-notifications-mode-map
      :nvm "n"       #'salih/fedi-notif-next
      :nvm "p"       #'salih/fedi-notif-prev
      :nvm "TAB"     #'salih/fedi-notif-next
      :nvm [backtab] #'salih/fedi-notif-prev
      :nvm "RET"     #'salih/fedi-notif-open
      :nvm "o"       #'salih/fedi-notif-open
      :nvm "a"       #'salih/fedi-notif-open-author
      :nvm "y"       #'salih/fedi-notif-copy-link
      :nvm "gr"      #'salih/fedi-notifications
      :nvm "q"       #'quit-window)

;; Same treatment for the timeline buffer: `gr' refreshes, `q' quits.
(map! :map salih/fedi-timeline-mode-map
      :nvm "gr" #'salih/fedi-timeline
      :nvm "q"  #'quit-window)

(provide 'lr-fedi)
;;; lr-fedi.el ends here
