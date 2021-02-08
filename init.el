;;; package --- Summary
;;; Commentary:
;;; Code:

(setq package-archives
      '(("melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
        ("gnu"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")))

;; when use offline emacs, use local elpa
;; (setq package-archives '(("elpa-local" . "~/.emacs.d/elpa-local/")))

(defvar my/package-list "Custom packagesa")
(setq my/package-list '(company counsel spacemacs-theme eglot general evil diminish bind-key keyfreq go-mode rust-mode flycheck yasnippet sudo-edit avy smex expand-region))

(require 'package)
(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      my/package-list)

(menu-bar-mode -1)
;; make it match the terminal's transparent background
(set-face-background 'default "unspecified-bg" )
(load-theme 'spacemacs-light t)

(setq inhibit-startup-screen t)

(column-number-mode t)
(line-number-mode t)
(delete-selection-mode 1)

(global-hl-line-mode t)

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

(setq custom-safe-themes t)
(setq custom-file (make-temp-file ""))

(global-auto-revert-mode t)
(fset 'yes-or-no-p'y-or-n-p)

(electric-pair-mode 1)
(setq electric-pair-inhibit-predicate
      'electric-pair-conservative-inhibit)

(show-paren-mode t)
(setq split-height-threshold nil)
(setq split-width-threshold 0)

(setq-default tab-width 4
              indent-tabs-mode nil)

(require 'bind-key)

(require 'diminish)

(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)
(setq keyfreq-excluded-commands
      '(self-insert-command
        forward-char
        backward-char
        previous-line
        next-line))

;; (require 'which-key)
;; (diminish 'which-key-mode)
;; (which-key-mode)
;; (setq which-key-idle-delay 4)

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
 "ee"    'flycheck-next-error
 "r"     'ivy-resume
 "/"     'swiper-isearch
 "v"     'er/expand-region
 "<SPC>" 'counsel-M-x
 "="     '(:keymap vc-prefix-map :which-key "vc")
 "p"     '(:keymap project-prefix-map :which-key "project"))

(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-set-undo-system 'undo-redo)
(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (setq-default evil-symbol-word-search t))
(evil-mode 1)

(require 'recentf)
(recentf-mode 1)
(add-to-list 'recentf-exclude "\\elpa")
(setq recentf-max-menu-items 64)
(setq recentf-max-saved-items 64)

(require 'company)
(diminish 'company-mode)
(global-company-mode)
(setq company-show-numbers t)
(setq company-dabbrev-downcase nil)
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-limit 20)

(require 'counsel)
(bind-key "M-x" 'counsel-M-x)
(bind-key "C-s" 'swiper-isearch)

(require 'avy)

(require 'smex)

(require 'expand-region)

(require 'yasnippet)

(require 'sudo-edit)

(require 'org)
(setq org-export-with-toc nil)
(setq org-html-head-include-default-style nil)
(setq org-agenda-files (list "~/org/work.org"
                             "~/org/home.org"))

;; (require 'tree-sitter)

;; (require 'tree-sitter-langs)

(require 'eglot)
(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'eglot-managed-mode-hook (lambda () (flymake-mode -1)))

(require 'flycheck)
(add-hook 'prog-mode-hook 'flycheck-mode)

(require 'cc-mode)
(setq c-default-style "linux")
(setq-default c-basic-offset 4)
(setq-default c-basic-indent 2)
(defun my/c-mode-hook()
  (c-toggle-hungry-state 1)
  (c-toggle-comment-style -1))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'my/c-mode-hook)

(require 'rust-mode)
(add-hook 'rust-mode-hook 'eglot-ensure)

(require 'go-mode)
(add-hook 'go-mode-hook 'eglot-ensure)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(pdf-view-midnight-colors '("#eeeeee" . "#000000")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 
(provide 'init)
;;; init.el ends here
