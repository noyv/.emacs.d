;;; package --- Summary
;;; Commentary:
;;; Code:

(setq tool-bar-mode nil)
(setq menu-bar-mode -1)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; (setq package-enable-at-startup nil)

; (setq frame-inhibit-implied-resize t)
(provide 'early-init)
;;; early-init.el ends here
