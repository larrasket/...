;;; ../configs/~s/.doom.d/+l/+l-trasnlate.el -*- lexical-binding: t; -*-

(use-package gt
  :config
  (setq gt-langs                                          `("en" "ar")
      gt-default-translator
      (gt-translator
       :taker   (gt-taker :text 'buffer :pick 'paragraph)
       :engines (list (gt-google-engine))
       :render        (gt-buffer-render))))


(provide '+l-translate)
