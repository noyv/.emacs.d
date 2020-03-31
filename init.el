;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'package)

(setq-default package-archives
              '(("melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
                ("gnu"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")))

(load-theme 'spacemacs-light t)
(column-number-mode t)
(line-number-mode t)
(global-hl-line-mode t)

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)

(setq make-backup-files nil)
(setq auto-save-default nil)

(global-auto-revert-mode t)
(fset 'yes-or-no-p'y-or-n-p)

(show-paren-mode t)
(setq split-height-threshold nil)
(setq split-width-threshold 0)

(setq scroll-conservatively 101
      scroll-margin 5
      scroll-preserve-screen-position 't)

(setq c-default-style "linux"
      c-basic-offset 4)
(setq-default tab-width 4
              indent-tabs-mode nil)

(defun my-c-mode-hook ()
  (c-toggle-hungry-state 1)
  (c-toggle-comment-style -1))
(add-hook 'c-mode-hook 'my-c-mode-hook)

(require 'general)
(general-define-key
  :states '(normal insert emacs) 
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  :keymaps 'override
  "ff"    'counsel-find-file
  "fr"    'counsel-recentf
  "fs"    'save-buffer
  "bl"     (lambda () (interactive) (switch-to-buffer nil))
  "bk"    'kill-buffer
  "ee"    'flymake-goto-next-error
  "rr"    'ivy-resume
  "ss"    'swiper-isearch
  "v"     'er/expand-region
  "<SPC>" 'counsel-M-x)

(require 'evil)
(evil-mode 1)
(with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol)
    ;; make evil-search-word look for symbol rather than word boundaries
    (setq-default evil-symbol-word-search t))

(evil-define-key '(normal motion) global-map "s" #'avy-goto-char-timer)
(evil-define-key 'normal global-map "gc" 'comment-line)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)
(setq recentf-max-saved-items 40)
(add-to-list 'recentf-exclude "\\.el\\'")

(require 'undo-tree)
(add-hook 'after-init-hook 'global-undo-tree-mode)
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff nil)

(require 'company)
(setq company-show-numbers t)
(setq company-dabbrev-downcase nil)
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-limit 20)
(setq company-backends (delete 'company-clang company-backends))

(require 'diminish)
(eval-after-load 'undo-tree
  '(diminish 'undo-tree-mode))
(eval-after-load 'company
  '(diminish 'company-mode))
(eval-after-load 'yasnippet
  '(diminish 'yas-minor-mode))
(diminish 'eldoc-mode)
(diminish 'abbrev-mode)

(require 'eglot)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(setq-default flymake-diagnostic-functions nil)

;; common settings for prog mode
(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'prog-mode-hook 'yas-minor-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#d2ceda" "#f2241f" "#67b11d" "#b1951d" "#3a81c3" "#a31db1" "#21b8c7" "#655370"])
 '(custom-safe-themes
   '("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default))
 '(evil-escape-mode t)
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#3a81c3")
     ("OKAY" . "#3a81c3")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#42ae2c")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(package-selected-packages
   '(general evil ranger racket-mode sudo-edit which-key spacemacs-theme expand-region smex counsel swiper ivy avy elpa-mirror company eglot yasnippet diminish undo-tree))
 '(pdf-view-midnight-colors '("#eeeeee" . "#000000")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
