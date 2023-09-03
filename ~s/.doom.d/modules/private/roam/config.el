;;; private/roam/config.el -*- lexical-binding: t; -*-





(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/roam"))

  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%H:%M> \n %?"
      :if-new (file+head "%<%Y-%m-%d>.org.gpg" "#+title: %^{daily-title}\n#+DATE: <%<%Y-%m-%d>>\n#+FILETAGS: journal\n- tags :: [[id:fe8618df-c476-44b8-8169-a210bff989d7][Journaling]]\n")
      :unnarrowed t)))

  :config
  (setq org-roam-database-connector 'sqlite)
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  ;; (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))
(defun cm/deft-parse-title (file contents)
  "Parse the given FILE and CONTENTS and determine the title.
  If `deft-use-filename-as-title' is nil, the title is taken to
  be the first non-empty line of the FILE.  Else the base name of the FILE is
  used as title."
  (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
    (if begin
	(string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
      (deft-base-filename file))))

(advice-add 'deft-parse-title :override #'cm/deft-parse-title)

(setq deft-strip-summary-regexp
      (concat "\\("
	      "[\n\t]" ;; blank
	      "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
	      "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
	      "\\)"))


(setq org-roam-dailies-directory "journal/")









(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))



(setq org-roam-capture-templates
      '(("k" "knowledge" plain "%?"
         :if-new
         (file+head "main/${slug}.org" "#+title: ${title}\n#+FILETAGS: permanent")
         :immediate-finish t
         :unnarrowed t)

        ("e" "encrypted knowledge" plain "%?"
         :if-new
         (file+head "main/${slug}.org.gpg" "#+title: ${title}\n#+FILETAGS: permanent")
         :immediate-finish t
         :unnarrowed t)


        ("l" "links" plain "%?"
         :if-new
         (file+head "main/${slug}.org" "#+title: ${title}\n#+FILETAGS: link\n")
         :immediate-finish t
         :unnarrowed t)

        ("f" "fleeting" plain "%?"
         :target
         (file+olp "main/fleet.org" ("${title}"))
         :immediate-finish t
         :unnarrowed nil)

        ("r" "bibliography reference" plain
         (file "~/configs/~s/orb")
         :target
         (file+head "references/${citekey}.org" "#+title: ${title}\n"))

        ("v"
         "video ref"
         entry
         "** ${body}"
         :target
         (file+olp
           "webnotes/yt.org"
           ("yt" "${title}"))
         :immediate-finish t
         :jump-to-captured t
         :unnarrowed t)))

(setq org-roam-capture-ref-templates org-roam-capture-templates)
;; To enable the v (video from youtube) tempplate, uses the following bookmark
;; javascript:(function(){  let v = (new URLSearchParams(window.location.search)).get('v');  if(location.href.indexOf("youtube.com")>=0 && v !== null)  {    let seek = (document.getElementsByClassName('video-stream html5-main-video')[0].currentTime).toFixed();    let ytb = "https://youtu.be/" + v;    location.href='org-protocol://roam-ref?template=v&ref=%27 + encodeURIComponent(ytb) + %27&title=%27 + encodeURIComponent(document.title) + %27&body=%27 + %27(%27 +  %27[%27 + %27[%27 + encodeURIComponent(ytb + %27&t=%27 + seek) + %27]%27  + %27[%27 + (new Date(seek * 1000)).toISOString().substr(11,8) + %27]%27 + %27]%27 + %27)%27;  }  else {    location.href=%27org-protocol://roam-ref?template=r&ref=%27 + encodeURIComponent(location.href) + %27&title=%27 + encodeURIComponent(document.title) + %27&body=%27 + encodeURIComponent(window.getSelection())  }})();
;; make sure to setup org roam protocl too.
;; FIXME Currently there is an issue with this function, when you take a note,
;; you will have manually to move the properties from under the yt section to
;; your new actual node
;; FROMATED:
;; javascript: (function() {
;;             let v = (new URLSearchParams(window.location.search)).get('v');
;;             if (location.href.indexOf("youtube.com") >= 0 && v !== null) {
;;                 let seek = (document.getElementsByClassName('video-stream html5-main-video')[0].currentTime).toFixed();
;;                 let ytb = "https://youtu.be/" + v;
;;                 location.href = 'org-protocol://roam-ref?template=v&ref=%27%20+%20encodeURIComponent(ytb)%20+%20%27&title=%27%20+%20encodeURIComponent(document.title)%20+%20%27&body=%27%20+%20%27(%27%20+%20%20%27[%27%20+%20%27[%27%20+%20encodeURIComponent(ytb%20+%20%27&t=%27%20+%20seek)%20+%20%27]%27%20%20+%20%27[%27%20+%20(new%20Date(seek%20*%201000)).toISOString().substr(11,8)%20+%20%27]%27%20+%20%27]%27%20+%20%27)%27;%20%20}%20%20else%20{%20%20%20%20location.href=%27org-protocol://roam-ref?template=r&ref=%27%20+%20encodeURIComponent(location.href)%20+%20%27&title=%27%20+%20encodeURIComponent(document.title)%20+%20%27&body=%27%20+%20encodeURIComponent(window.getSelection())%20%20}})();

(cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE."
  (condition-case nil
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory
         (file-relative-name (org-roam-node-file node) org-roam-directory))))
    (error "")))

;; (setq org-roam-node-display-template
;;       (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))



(use-package org-roam)







(after! popup
  (set-popup-rule! "\\*org-roam\\*"
    :side 'right
    :width 0.40
    :slot 0
    :parameters '((no-other-window . t)
                  (no-delete-other-windows . t))))


(defvar org-roam-list-most-linked-count 5)
(cl-defmethod org-roam-node-backlinkscount-number ((node org-roam-node)) "Access slot \"backlinks\" of org-roam-node struct CL-X. This is identical toorg-roam-node-backlinkscount' with the difference that it returns a number instead of a fromatted string. This is to be used in `org-roam-node-sort-by-backlinks'" (let* ((count (caar (org-roam-db-query [:select (funcall count source) :from links :where (= dest $s1) :and (= type "id")] (org-roam-node-id node))))) count))
(defun org-roam-node-sort-by-backlinks (completion-a completion-b) "Sorting function for org-roam that sorts the list of nodes by the number of backlinks. This is the sorting function in `org-roam-node-find-by-backlinks'" (let ((node-a (cdr completion-a)) (node-b (cdr completion-b))) (>= (org-roam-node-backlinkscount-number node-a) (org-roam-node-backlinkscount-number node-b))))
(defun org-roam-node-find-by-backlinks () "Essentially works like org-roam-node-find' (although it uses a combination offind-file' and org-roam-node-read' to accomplish that and notorg-roam-node-find' as only org-roam-node-read' can take a sorting function as an argument) but the list of nodes is sorted by the number of backlinks instead of most recent nodes. Sorting is done with org-roam-node-sort-by-backlinks'" (interactive) (find-file (org-roam-node-file (org-roam-node-read nil nil #'org-roam-node-sort-by-backlinks))))


(provide '+roam)

(add-hook 'org-roam-find-file-hook #'git-auto-commit-mode)
