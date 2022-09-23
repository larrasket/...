;;; doom-lighttable-theme.el --- inspired by Lighttable -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Copyright (C) 2016-2022 Gregory Adebesin
;;
;; Author: Gregory Adebesin <https://github.com/kazuwal>
;; Created: January 20, 2022
;; Version: 1.0.0
;; Keywords: custom themes, faces
;; Homepage: https://github.com/ensudo/lighttable
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (doom-themes "2.2.1"))
;;
;;; Commentary:
;;
;; Inspired by Lighttable color scheme.
;;
;;; Code:

(require 'doom-themes)

;;
;;; Variables

(defgroup doom-lighttable-theme nil
  "Options for the `doom-lighttable' theme."
  :group 'doom-themes)

(defcustom doom-lighttable-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-lighttable-theme
  :type 'boolean)

(defcustom doom-lighttable-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-lighttable-theme
  :type 'boolean)

(defcustom doom-lighttable-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-lighttable-theme
  :type '(choice integer boolean))

;;
;;; Theme definition

(def-doom-theme doom-lighttable
  "A dark theme inspired by Lighttable theme."

  ;; name        default   256           16
  ((bg         '("#202020" "#202020"     "#202020"  ))
   (fg         '("#CCCCCC" "#CCCCCC"     "#CCCCCC"  ))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#202020" "#202020" "#202020"        ))
   (fg-alt     '("#CCCCCC" "#CCCCCC" "#CCCCCC"        ))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#202020" "#202020"     "#202020"      ))
   (base1      '("#295E5F" "#295E5F"     "#295E5F"      ))
   (base2      '("#CCCCCC" "#CCCCCC"     "#CCCCCC"      ))
   (base3      '("#262626" "#262626"     "#262626"      ))
   (base4      '("#CCCCCC" "#CCCCCC"     "#CCCCCC"      ))
   (base5      '("#99AACC" "#99AACC"     "#99AACC"      ))
   (base6      '("#CCCCCC" "#CCCCCC"     "#CCCCCC"      ))
   (base7      '("#CCCCCC" "#CCCCCC"     "#CCCCCC"      ))
   (base8      '("#CCCCCC" "#CCCCCC"     "#CCCCCC"      ))

   (grey       base4)
   (red        '("#FFAAB3" "#FFAAB3" "#FFAAB3"      ))
   (orange     '("#FFCCAA" "#FFCCAA" "#FFCCAA"      ))
   (green      '("#AAFFCC" "#AAFFCC" "#AAFFCC"      ))
   (teal       '("#4db5bd" "#4db5bd" "#4db5bd"      ))
   (yellow     '("#DDFFAA" "#DDFFAA" "#DDFFAA"      ))
   (blue       '("#AACCFF" "#AACCFF" "#AACCFF"      ))
   (dark-blue  '("#6A809F" "#6A809F" "#6A809F"      ))
   (magenta    '("#6DFBFD" "#6DFBFD" "#6DFBFD"      ))
   (violet     '("#a096fa" "#a096fa" "#a096fa"      ))
   (cyan       '("#96f0fa" "#96f0fa" "#96f0fa"      ))
   (dark-cyan  '("#3c6064" "#3c6064" "#3c6064"      ))

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.

   (highlight      "#AACCFF")
   (vertical-bar   "#202020")
   (selection      "#383838")
   (builtin        "#CCAAFF")
   (comments       "#99AACC")
   (doc-comments   "#99AACC")
   (constants      "#CCAAFF")
   (functions      "#AACCFF")
   (keywords       "#AAEECC")
   (methods        "#CCAAFF")
   (operators      "#CCCCCC")
   (type           "#CCCCCC")
   (strings        "#AADDDD")
   (variables      "#AACCFF")
   (numbers        "#9DD3D3")
   (region         "#353535")
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; These are extra color variables used only in this theme; i.e. they aren't
   ;; mandatory for derived themes.
   (modeline-fg              fg)
   (modeline-fg-alt          fg)
   (modeline-bg              "#2c2c2c")
   (modeline-bg-alt          "#2c2c2c")
   (modeline-bg-inactive     "#2c2c2c")
   (modeline-bg-inactive-alt "#2c2c2c")

   (-modeline-pad
    (when doom-lighttable-padded-modeline
      (if (integerp doom-lighttable-padded-modeline) doom-one-padded-modeline 4))))

  ;;;; Base theme face overrides
  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   ((font-lock-comment-face &override)
    :background (if doom-lighttable-brighter-comments (doom-lighten bg 0.05)))

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box `(:line-width 3 :color "#393939"))

   (mode-line-inactive
    :background modeline-bg :foreground modeline-fg
    :box `(:line-width 3 :color "#393939"))

   (internal-border :background "#202020")

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if doom-lighttable-brighter-modeline modeline-bg highlight))
   (doom-modeline-buffer-file :foreground "#CCCCCC")
   (doom-modeline-evil-normal-state :foreground "#AAEECC")
   (doom-modeline-evil-insert-state :foreground "#AACCFF")
   (doom-modeline-project-dir :foreground "#AACCFF")
   (doom-modeline-buffer-path :foreground "#AACCFF")
   (doom-modeline-buffer-modified :foreground "#FFAAB3")
   (doom-modeline-buffer-major-mode :foreground "#CCAAFF")
   ;; overrides
   (lazy-highlight :background "#295E5F" :foreground "#6DFBFD")
   (isearch :background "#295E5F" :foreground "#6DFBFD")
   (highlight :background "#295E5F" :foreground "#6DFBFD")
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; ivy
   (ivy-current-match :foreground bg :background fg)
   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ;;;; rjsx-mode
   (rjsx-tag :foreground "#AAEECC")
   (rjsx-attr :foreground "#AACCFF")
   ;; paren
   (paren-face-match :foreground "#37FF00" :weight 'bold :underline '(:color "#6DFBFD" :style line :weight 'bold))
   (paren-face-mismatch :foreground red :weight 'bold :underline '(:color "#ff6666" :style line :weight 'bold))
   (paren-face-no-match :foreground red :weight 'bold :underline '(:color "#ff6666" :style line :weight 'bold))
   ;;;; solaire-mode

   (nav-flash-face :background "#295E5F" :foreground "#6DFBFD")

   (solaire-mode-line-face
    :background modeline-bg :foreground modeline-fg
    :box `(:line-width 3 :color "#393939"))

   (solaire-mode-line-inactive-face
    :background modeline-bg :foreground modeline-fg
    :box `(:line-width 3 :color "#393939"))

   (rainbow-delimiters-depth-1-face :foreground "#aaa")
   (rainbow-delimiters-depth-2-face :foreground "#aaa")
   (rainbow-delimiters-depth-3-face :foreground "#aaa")
   (rainbow-delimiters-depth-4-face :foreground "#aaa")
   (rainbow-delimiters-depth-5-face :foreground "#aaa")
   (rainbow-delimiters-depth-6-face :foreground "#aaa")
   (rainbow-delimiters-depth-7-face :foreground "#aaa")
   (rainbow-delimiters-depth-8-face :foreground "#aaa")
   (rainbow-delimiters-depth-9-face :foreground "#aaa")

   (magit-hash :foreground "#CCCCCC")
   (magit-section-heading :foreground "#CCAAFF")

   (org-level-1 :foreground "#AAEECC")
   (org-level-2 :foreground "#CCAAFF")
   (org-level-3 :foreground "#AACCFF")
   (org-level-4 :foreground "#a096fa")
   (org-level-5 :foreground "#AAEECC")
   (org-level-6 :foreground "#CCAAFF")
   (org-level-7 :foreground "#AACCFF")
   (org-level-8 :foreground "#a096fa")
   (org-todo :foreground "#AAEECC")
   )

  ;;;; Base theme variable overrides-
  ())

;;; doom-lighttable-theme.el ends here
