;;; ../configs/.doom.d/leader.el -*- lexical-binding: t; -*-


(map! :leader
      :desc "run vterm"
      "t t" 'vterm)

(map! :leader
      :desc "run mail"
      "m m" 'mu4e)

(map! :leader
      :desc "open-ajenda"
      "a" #'org-agenda)


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

(provide 'leader)
