;;; mine/flyivy/config.el -*- lexical-binding: t; -*-



(require 'ivy-posframe)
(setq ivy-posframe-height-alist '((swiper . 20)
                                 (t      . 40)))
(setq ivy-posframe-display-functions-alist
     '((swiper          . ivy-display-function-fallback)
       (complete-symbol . ivy-posframe-display-at-point)
       (counsel-M-x     . ivy-posframe-display-at-frame-top-center)
       (t               . ivy-posframe-display)))
(ivy-posframe-mode 1)
(setq ivy-posframe-parameters
     '((left-fringe . 8)
       (right-fringe . 8)))
