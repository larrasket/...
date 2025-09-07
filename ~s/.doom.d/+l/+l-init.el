;;; +l/init.el -*- lexical-binding: t; -*-

;; Add the +l directory to load path
(add-to-list 'load-path (file-name-directory load-file-name))

;; Load core modules (always loaded)
(require '+l-helpers)

;; Load UI modules
(require '+l-ui-basic)
(require '+l-ui-modeline)
(require '+l-ui-completion)

;; Load Org modules (can be disabled individually)
;; (require '+l-org-core)
;; (require '+l-org-capture)
;; (require '+l-org-roam)
;; (require '+l-org-fc)
;; (require '+l-org-noter)

;; Load Academic modules (can be disabled individually)
;; (require '+l-academic-bibtex)
;; (require '+l-academic-citar)

;; Load Programming modules (can be disabled individually)
(require '+l-prog-lsp)
(require '+l-prog-compilation)
(require '+l-prog-format)

;; Load Communication modules (can be disabled individually)
;; (require '+l-email)
;; (require '+l-irc)

;; Load remaining modules
(require '+l-common)


;; (require '+l-translate)
;; (require '+l-browse)
(require '+l-prayer)
(provide '+l-init)
