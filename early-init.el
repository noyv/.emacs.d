;;; package --- Summary
;;; Commentary:
;;; Code:
(setq gc-cons-threshold most-positive-fixnum)

(setq tool-bar-mode nil
      menu-bar-mode nil)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq package-enable-at-startup nil)

; (setq frame-inhibit-implied-resize t)
(provide 'early-init)
;;; early-init.el ends here
