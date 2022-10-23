;;; ../configs/.doom.d/leaders.el -*- lexical-binding: t; -*-


(map! :leader
      :desc "move to jorunal"
      "j" #'org-journal-new-entry)


(map! :leader
      :desc "play playlist"
      "m p p" #'emms-play-playlist)


(map! :leader
      :desc "play directory"
      "m p d" #'emms-play-directory)


(map! :leader
      :desc "save play directory"
      "m p d" #'emms-play-directory)


(map! :leader
      :desc "save current playlist"
      "m p s" #'emms-playlist-save)


(map! :leader
      :desc "shuffle playlist"
      "m p c" #'emms-shuffle)


(map! :leader
      :desc "repeat"
      "m p r" #'emms-toggle-repeat-track)


(map! :leader
      :desc "run vterm"
      "t t" 'vterm)
(map! :leader
      :desc "run mail"
      "m m" 'mu4e)
(map! :leader
      :desc "watch var"
      "o w" 'dap-ui-expressions-add)
(map! :leader
      :desc "open-ajenda"
      "a" #'org-agenda)

(map! :leader
      :desc "insert date"
      "d" #'org-schedule)


(map! :leader
      :desc "show errors"
      "e e" #'flycheck-list-errors)



(map! :leader
      :desc "emacs shell"
      "e s" #'eshell)

(map! :leader
      :desc "show errors"
      "e l" #'lsp-treemacs-errors-list)

(map! :leader
      :desc "evaluate latex"
      "l e" #'TeX-command-master "LaTex")




(map! :leader
      :desc "open leetcode"
      "l l" #'leetcode)



(map! :leader
      :desc "roam graph"
      "r g" #'org-roam-graph)


(map! :leader
      :desc "add tag"
      "r t" #'org-roam-tag-add)



(map! :leader
      :desc "switch to raom buffer"
      "r b" #'org-roam-buffer-toggle)


(map! :leader
      :desc "capture"
      "r c" #'org-roam-capture)


(map! :leader
      :desc "insert"
      "r i" #'org-roam-node-insert)


(map! :leader
      :desc "find file"
      "r f" #'org-roam-node-find)

(map! :leader
      :desc "roam"
      "r r" #'org-roam-buffer-display-dedicated)


(map! :leader
      :desc "org caputer"
      "x" #'org-capture)


(map! :leader
      :desc "org caputer"
      "r j" #'org-roam-dailies-capture-today)


(provide 'leaders)
