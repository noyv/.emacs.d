(setq gc-cons-threshold most-positive-fixnum)

(setq tool-bar-mode nil
      menu-bar-mode nil)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(set-face-attribute 'default nil  :family "Hack" :height 135 :weight 'normal)


(setq frame-inhibit-implied-resize t)
