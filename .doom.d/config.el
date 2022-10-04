(add-to-list 'load-path "~/.doom.d/")
(require 'genset)
(require 'orgset)
(require 'packconf)
(require 'languageshelpers)
(require 'keys)
(require 'diredconf)
(require 'emailconf)
(require 'shellrc)
(require 'music)
(require 'feedreader)
(require 'dashboardconf)
(require 'leaders)

;; (setq org-plantuml-jar-path
;;       (expand-file-name "~/.doom.d/bin/plantuml.jar"))
(setq gts-translate-list '(("en" "ar")))
;; (setq gts-default-translator
;;       (gts-translator
;;        :picker (gts-prompt-picker)
;;        :engines (list (gts-bing-engine) (gts-google-engine))
;;        :render (gts-buffer-render)))
    (setq-default org-download-image-dir "./org-media")


