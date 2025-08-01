;;; ../configs/~s/.doom.d/+l/+l-prayer.el -*- lexical-binding: t; -*-


(use-package awqat
  :config
  (setq calendar-latitude                                 29.392691
      calendar-longitude                                30.828360
      salih/awqat-show-mode-line                        t
      awqat-mode-line-format
      " ${prayer} (${hours}h${minutes}m) "
      awqat-update-interval                             (* 60 5)))


(provide '+l-prayer)
