;;; package --- Summary
;;; Commentary:
;;; Code:

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

(global-auto-revert-mode t)
(delete-selection-mode 1)

(setq-default tab-width 4
              indent-tabs-mode nil)

(setq-default mode-line-buffer-identification
              '((:eval (list (abbreviate-file-name
                              (expand-file-name buffer-file-name))))))

(setq inhibit-startup-screen t)
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)

(setq custom-safe-themes t)
(setq split-height-threshold nil)
(setq split-width-threshold 0)

(menu-bar-mode -1)
(column-number-mode t)
(line-number-mode t)
(show-paren-mode t)
(global-hl-line-mode)
(add-to-list 'default-frame-alist '(font . "Monaco-16"))

(setq custom-file (make-temp-file ""))
(fset 'yes-or-no-p'y-or-n-p)

(recentf-mode 1)
(add-to-list 'recentf-exclude "\\elpa")
(setq recentf-max-menu-items 64)
(setq recentf-max-saved-items 64)

(when (memq window-system '(mac ns x))
  (straight-use-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(defun local-graphic-config()
  (add-to-list 'default-frame-alist '(font . "FiraCode-16"))
  (load-theme 'modus-operandi t))

(defun local-terminal-config()
  (straight-use-package 'evil-terminal-cursor-changer)
  (evil-terminal-cursor-changer-activate))

(if (display-graphic-p)
    (local-graphic-config)
  (local-terminal-config))

(straight-use-package 'bind-key)
(straight-use-package 'avy)
(straight-use-package 'expand-region)
(straight-use-package 'sudo-edit)

(straight-use-package 'smartparens)
(add-hook 'prog-mode-hook 'smartparens-mode)

(straight-use-package 'markdown-mode)
(straight-use-package 'yaml-mode)

(straight-use-package 'which-key)
(which-key-mode)
(setq-default which-key-idle-delay 4)

(straight-use-package 'orderless)
(setq completion-styles '(orderless)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

(straight-use-package 'vertico)
(vertico-mode)
(savehist-mode)
(defun crm-indicator (args)
  (cons (concat "[CRM] " (car args)) (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(setq completion-styles '(substring orderless))

(straight-use-package 'consult)
(require 'consult)
(consult-customize
 consult-ripgrep consult-git-grep consult-grep
 consult-bookmark consult-recent-file consult-xref
 consult--source-file consult--source-project-file consult--source-bookmark
 :preview-key (kbd "M-."))

(straight-use-package 'embark)
(bind-key "M-o" 'embark-act)
(bind-key "C-h B" 'embark-bindings)
(setq prefix-help-command #'embark-prefix-help-command)
;; Hide the mode line of the Embark live/completions buffers
(add-to-list 'display-buffer-alist
             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
               nil
               (window-parameters (mode-line-format . none))))

(straight-use-package 'embark-consult)
(add-hook 'embark-collect-mode-hook 'embark-consult-preview-minor-mode)

(straight-use-package 'general)
(general-define-key
 :states '(normal insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 :keymaps 'override
 "<SPC>" 'execute-extended-command
 ;; "<SPC>" 'counsel-M-x
 ;; "ff"    'counsel-find-file
 "ff"    'find-file
 "fr"    'consult-buffer
 "fe"    'consult-flymake
 "fl"    'consult-line
 "v"     'er/expand-region
 "="     '(:keymap vc-prefix-map :which-key "vc")
 "p"     '(:keymap project-prefix-map :which-key "project"))

(straight-use-package 'evil)
(setq-default evil-want-C-u-scroll t)
(setq evil-disable-insert-state-bindings t)
(setq evil-undo-system 'undo-redo)
(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (setq-default evil-symbol-word-search t))
(evil-mode 1)

(straight-use-package 'company)
(global-company-mode)
(setq company-show-numbers t)
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-limit 20)

(straight-use-package 'valign)
(add-hook 'org-mode-hook 'valign-mode)

(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)
(add-hook 'prog-mode-hook 'yas-minor-mode)

;; (straight-use-package 'flycheck)
;; (add-hook 'prog-mode-hook 'flycheck-mode)

(straight-use-package 'eglot)
(setq-default eglot-ignored-server-capabilites '(:documentHighlightProvider))
;; (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
(setq eldoc-idle-dealy 2)

;; when lang c/c++
(setq c-default-style "linux")
(setq c-basic-offset 4)
(defun my/c-mode-hook()
  (c-toggle-hungry-state 1)
  (c-toggle-comment-style -1))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'my/c-mode-hook)

;; when lang python
(add-hook 'python-mode-hook 'eglot-ensure)

;; when lang js
(add-hook 'js-mode-hook 'eglot-ensure)

;; when lang golang
(straight-use-package 'go-mode)
(add-hook 'go-mode-hook 'eglot-ensure)
(setq-default eglot-workspace-configuration
              '((gopls
                 (usePlaceholders . t))))
(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))
(cl-defmethod project-root ((project (head go-module)))
  (cdr project))
(add-hook 'project-find-functions #'project-find-go-module)

;; when lang rust
(straight-use-package 'rust-mode)
(add-hook 'rust-mode-hook 'eglot-ensure)
(require 'eglot)
(add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))

;; when lang org
(straight-use-package '(org :type built-in))
(setq-default org-export-with-toc nil)
(setq-default org-export-with-section-numbers nil)
(setq-default org-html-head-include-default-style nil)
(setq-default org-startup-folded 'content)

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
