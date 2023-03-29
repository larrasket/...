;;; private/bind/config.el -*- lexical-binding: t; -*-





(when (modulep! :editor evil +everywhere)
  ;; NOTE SPC u replaces C-u as the universal argument.

  ;; Minibuffer
  (map! :map (evil-ex-completion-map evil-ex-search-keymap)
        "C-a" #'evil-beginning-of-line
        "C-b" #'evil-backward-char
        "C-f" #'evil-forward-char
        :gi "C-j" #'next-complete-history-element
        :gi "C-k" #'previous-complete-history-element)

  (define-key! :keymaps +default-minibuffer-maps
    [escape] #'abort-recursive-edit
    "C-a"    #'move-beginning-of-line
    "C-r"    #'evil-paste-from-register
    "C-u"    #'evil-delete-back-to-indentation
    "C-v"    #'yank
    "C-w"    #'doom/delete-backward-word
    "C-z"    (cmd! (ignore-errors (call-interactively #'undo))))

  (define-key! :keymaps +default-minibuffer-maps
    "C-j"    #'next-line
    "C-k"    #'previous-line
    "C-S-j"  #'scroll-up-command
    "C-S-k"  #'scroll-down-command)
  ;; For folks with `evil-collection-setup-minibuffer' enabled
  (define-key! :states 'insert :keymaps +default-minibuffer-maps
    "C-j"    #'next-line
    "C-k"    #'previous-line)
  (define-key! read-expression-map
    "C-j" #'next-line-or-history-element
    "C-k" #'previous-line-or-history-element))


;;
;;; Global keybindings

;; Smart tab, these will only work in GUI Emacs
(map! :i [tab] (cmds! (and (modulep! :editor snippets)
                           (yas-maybe-expand-abbrev-key-filter 'yas-expand))
                      #'yas-expand
                      (and (bound-and-true-p company-mode)
                           (modulep! :completion company +tng))
                      #'company-indent-or-complete-common)
      :m [tab] (cmds! (and (modulep! :editor snippets)
                           (evil-visual-state-p)
                           (or (eq evil-visual-selection 'line)
                               (not (memq (char-after) (list ?\( ?\[ ?\{ ?\} ?\] ?\))))))
                      #'yas-insert-snippet
                      (and (modulep! :editor fold)
                           (save-excursion (end-of-line) (invisible-p (point))))
                      #'+fold/toggle
                      ;; Fixes #4548: without this, this tab keybind overrides
                      ;; mode-local ones for modes that don't have an evil
                      ;; keybinding scheme or users who don't have :editor (evil
                      ;; +everywhere) enabled.
                      (or (doom-lookup-key
                           [tab]
                           (list (evil-get-auxiliary-keymap (current-local-map) evil-state)
                                 (current-local-map)))
                          (doom-lookup-key
                           (kbd "TAB")
                           (list (evil-get-auxiliary-keymap (current-local-map) evil-state)))
                          (doom-lookup-key (kbd "TAB") (list (current-local-map))))
                      it
                      (fboundp 'evil-jump-item)
                      #'evil-jump-item)

      (:after help :map help-mode-map
       :n "o"       #'link-hint-open-link)
      (:after helpful :map helpful-mode-map
       :n "o"       #'link-hint-open-link)
      (:after info :map Info-mode-map
       :n "o"       #'link-hint-open-link)
      (:after apropos :map apropos-mode-map
       :n "o"       #'link-hint-open-link
       :n "TAB"     #'forward-button
       :n [tab]     #'forward-button
       :n [backtab] #'backward-button)
      (:after view :map view-mode-map
       [escape]  #'View-quit-all)
      (:after man :map Man-mode-map
       :n "q"    #'kill-current-buffer)
      (:after geiser-doc :map geiser-doc-mode-map
       :n "o"    #'link-hint-open-link)

      (:unless (modulep! :input layout +bepo)
        (:after (evil-org evil-easymotion)
         :map evil-org-mode-map
         :m "gsh" #'+org/goto-visible))

      (:when (modulep! :editor multiple-cursors)
       :prefix "gz"
       :nv "d" #'evil-mc-make-and-goto-next-match
       :nv "D" #'evil-mc-make-and-goto-prev-match
       :nv "s" #'evil-mc-skip-and-goto-next-match
       :nv "S" #'evil-mc-skip-and-goto-prev-match
       :nv "c" #'evil-mc-skip-and-goto-next-cursor
       :nv "C" #'evil-mc-skip-and-goto-prev-cursor
       :nv "j" #'evil-mc-make-cursor-move-next-line
       :nv "k" #'evil-mc-make-cursor-move-prev-line
       :nv "m" #'evil-mc-make-all-cursors
       :nv "n" #'evil-mc-make-and-goto-next-cursor
       :nv "N" #'evil-mc-make-and-goto-last-cursor
       :nv "p" #'evil-mc-make-and-goto-prev-cursor
       :nv "P" #'evil-mc-make-and-goto-first-cursor
       :nv "q" #'evil-mc-undo-all-cursors
       :nv "t" #'+multiple-cursors/evil-mc-toggle-cursors
       :nv "u" #'+multiple-cursors/evil-mc-undo-cursor
       :nv "z" #'+multiple-cursors/evil-mc-toggle-cursor-here
       :v  "I" #'evil-mc-make-cursor-in-visual-selection-beg
       :v  "A" #'evil-mc-make-cursor-in-visual-selection-end)

      ;; misc
      :n "C-S-f"  #'toggle-frame-fullscreen
      :n "C-+"    #'doom/reset-font-size
      ;; Buffer-local font resizing
      :n "C-="    #'text-scale-increase
      :n "C--"    #'text-scale-decrease
      ;; Frame-local font resizing
      :n "M-C-="  #'doom/increase-font-size
      :n "M-C--"  #'doom/decrease-font-size)


;;
;;; Module keybinds

;;; :completion
(map! (:when (modulep! :completion company)
       :i "C-@"    (cmds! (not (minibufferp)) #'company-complete-common)
       :i "C-SPC"  (cmds! (not (minibufferp)) #'company-complete-common)
       (:after company
        (:map company-active-map
         "C-w"     nil  ; don't interfere with `evil-delete-backward-word'
         "C-n"     #'company-select-next
         "C-p"     #'company-select-previous
         "C-j"     #'company-select-next
         "C-k"     #'company-select-previous
         "C-h"     #'company-show-doc-buffer
         "C-u"     #'company-previous-page
         "C-d"     #'company-next-page
         "C-s"     #'company-filter-candidates
         "C-S-s"   #'+company/completing-read
         "C-SPC"   #'company-complete-common
         "TAB"     #'company-complete-common-or-cycle
         [tab]     #'company-complete-common-or-cycle
         [backtab] #'company-select-previous
         [f1]      nil)
        (:map company-search-map  ; applies to `company-filter-map' too
         "C-n"     #'company-select-next-or-abort
         "C-p"     #'company-select-previous-or-abort
         "C-j"     #'company-select-next-or-abort
         "C-k"     #'company-select-previous-or-abort
         "C-s"     #'company-filter-candidates
         [escape]  #'company-search-abort)))

      (:when (modulep! :completion ivy)
       (:after ivy
        :map ivy-minibuffer-map
        "C-SPC" #'ivy-call-and-recenter  ; preview file
        "C-l"   #'ivy-alt-done
        "C-v"   #'yank)
       (:after counsel
        :map counsel-ag-map
        "C-SPC"    #'ivy-call-and-recenter ; preview
        "C-l"      #'ivy-done
        [C-return] #'+ivy/git-grep-other-window-action))

      (:when (modulep! :completion helm)
       (:after helm :map helm-map
        [remap next-line]     #'helm-next-line
        [remap previous-line] #'helm-previous-line
        [left]     #'left-char
        [right]    #'right-char
        "C-S-f"    #'helm-previous-page
        "C-S-n"    #'helm-next-source
        "C-S-p"    #'helm-previous-source
        (:when (modulep! :editor evil +everywhere)
         "C-j"    #'helm-next-line
         "C-k"    #'helm-previous-line
         "C-S-j"  #'helm-next-source
         "C-S-k"  #'helm-previous-source)
        "C-u"      #'helm-delete-minibuffer-contents
        "C-s"      #'helm-minibuffer-history
        ;; Swap TAB and C-z
        "TAB"      #'helm-execute-persistent-action
        [tab]      #'helm-execute-persistent-action
        "C-z"      #'helm-select-action)
       (:after helm-ag :map helm-ag-map
        "C--"      #'+helm-do-ag-decrease-context
        "C-="      #'+helm-do-ag-increase-context
        [left]     nil
        [right]    nil)
       (:after helm-files :map (helm-find-files-map helm-read-file-map)
        [C-return] #'helm-ff-run-switch-other-window
        "C-w"      #'helm-find-files-up-one-level
        (:when (modulep! :editor evil +everywhere)
          "C-h"    #'helm-find-files-up-one-level
          "C-l"    #'helm-execute-persistent-action))
       (:after helm-locate :map helm-generic-files-map
        [C-return] #'helm-ff-run-switch-other-window)
       (:after helm-buffers :map helm-buffer-map
        [C-return] #'helm-buffer-switch-other-window)
       (:after helm-occur :map helm-occur-map
        [C-return] #'helm-occur-run-goto-line-ow)
       (:after helm-grep :map helm-grep-map
        [C-return] #'helm-grep-run-other-window-action))

      (:when (modulep! :completion vertico)
        (:after vertico
         :map vertico-map
         "M-RET" #'vertico-exit-input
         "C-SPC" #'+vertico/embark-preview
         "C-j"   #'vertico-next
         "C-M-j" #'vertico-next-group
         "C-k"   #'vertico-previous
         "C-M-k" #'vertico-previous-group)))


;;; :ui
(map! (:when (modulep! :ui popup)
       "C-`"   #'+popup/toggle
       "C-~"   #'+popup/raise
       "C-x p" #'+popup/other)

      (:when (modulep! :ui workspaces)
       :n "C-t"   #'+workspace/new
       :n "C-S-t" #'+workspace/display
       :g "M-1"   #'+workspace/switch-to-0
       :g "M-2"   #'+workspace/switch-to-1
       :g "M-3"   #'+workspace/switch-to-2
       :g "M-4"   #'+workspace/switch-to-3
       :g "M-5"   #'+workspace/switch-to-4
       :g "M-6"   #'+workspace/switch-to-5
       :g "M-7"   #'+workspace/switch-to-6
       :g "M-8"   #'+workspace/switch-to-7
       :g "M-9"   #'+workspace/switch-to-8
       :g "M-0"   #'+workspace/switch-to-final
       (:when IS-MAC
        :g "s-t"   #'+workspace/new
        :g "s-T"   #'+workspace/display
        :n "s-1"   #'+workspace/switch-to-0
        :n "s-2"   #'+workspace/switch-to-1
        :n "s-3"   #'+workspace/switch-to-2
        :n "s-4"   #'+workspace/switch-to-3
        :n "s-5"   #'+workspace/switch-to-4
        :n "s-6"   #'+workspace/switch-to-5
        :n "s-7"   #'+workspace/switch-to-6
        :n "s-8"   #'+workspace/switch-to-7
        :n "s-9"   #'+workspace/switch-to-8
        :n "s-0"   #'+workspace/switch-to-final)))

;;; :editor
(map! (:when (modulep! :editor format)
       :n "gQ" #'+format:region)

      (:when (modulep! :editor rotate-text)
       :n "]r"  #'rotate-text
       :n "[r"  #'rotate-text-backward)

      (:when (modulep! :editor multiple-cursors)
       ;; evil-multiedit
       :v  "R"     #'evil-multiedit-match-all
       :n  "M-d"   #'evil-multiedit-match-symbol-and-next
       :n  "M-D"   #'evil-multiedit-match-symbol-and-prev
       :v  "M-d"   #'evil-multiedit-match-and-next
       :v  "M-D"   #'evil-multiedit-match-and-prev
       :nv "C-M-d" #'evil-multiedit-restore
       (:after evil-multiedit
        (:map evil-multiedit-mode-map
         :nv "M-d" #'evil-multiedit-match-and-next
         :nv "M-D" #'evil-multiedit-match-and-prev
         [return]  #'evil-multiedit-toggle-or-restrict-region)))

      (:when (modulep! :editor snippets)
       ;; auto-yasnippet
       :i  [C-tab] #'aya-expand
       :nv [C-tab] #'aya-create))

;;; :tools
(when (modulep! :tools eval)
  (map! "M-r" #'+eval/buffer))
