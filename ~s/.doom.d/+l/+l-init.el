;;; +l/init.el -*- lexical-binding: t; -*-

(require '+l-helpers)
(require '+l-ui-basic)
(require '+l-ui-modeline)
(require '+l-ui-completion)
(require '+l-org-core)

;; Load Org modules (can be disabled individually)
(require '+l-org-capture)
(require '+l-org-roam)
(require '+l-org-fc)
(require '+l-org-noter)


;; Load Programming modules (can be disabled individually)
(require '+l-prog-lsp)
(require '+l-prog-compilation)
(require '+l-prog-format)

;; Load Communication modules (can be disabled individually)
;; (require '+l-email)
(require '+l-irc)
(require '+l-translate)
(require '+l-browse)
(require '+l-prayer)


;; Load Academic modules (can be disabled individually)
(require '+l-academic-bibtex)
(require '+l-academic-citar)



;; apple specific
(require '+l-apple)

(require '+l-common)
(require '+l-pic-log)
(provide '+l-init)


