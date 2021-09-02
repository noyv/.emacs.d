;;; package --- Summary
;;; Commentary:
;;; Code:

(setq package-archives
      '(("melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
        ("gnu"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")))

;; when use offline emacs, use local elpa
;; (setq package-archives '(("elpa-local" . "~/.emacs.d/elpa-local/")))

(defvar my/package-list "Custom packages")
(setq my/package-list '(company eglot general evil bind-key keyfreq go-mode rust-mode))

;; (mapc #'(lambda (package)
;;         (unless (package-installed-p package)
;;            (package-install package)))
;;      my/package-list)

(exec-path-from-shell-initialize)

(require 'which-key)
(which-key-mode)
(setq which-key-idle-delay 4)

(require 'general)
(general-define-key
 :states '(normal insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 :keymaps 'override
 "ff"    'find-file
 "fr"    'consult-buffer
 "fl"    'consult-line
 "fe"    'consult-flymake
 "v"     'er/expand-region
 "<SPC>" 'execute-extended-command
 "="     '(:keymap vc-prefix-map :which-key "vc")
 "p"     '(:keymap project-prefix-map :which-key "project"))

(setq-default evil-want-C-u-scroll t)
(require 'evil)
(evil-set-undo-system 'undo-redo)
(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (setq-default evil-symbol-word-search t))
(evil-mode 1)

;; (sis-ism-lazyman-config "1" "2" 'fcitx5)
(sis-ism-lazyman-config
 "com.apple.keylayout.ABC"
 "com.apple.keylayout.SCIM.ITABC")
(sis-global-cursor-color-mode t)
(sis-global-respect-mode t)
(sis-global-context-mode t)
(sis-global-inline-mode t)
(setq sis-do-set
      (lambda(source) (start-process "set-input-source" nil "macism" source "50000")))

;; (defun my/after-make-frame-functions-hook (frame)
;;  (with-selected-frame frame
;;    (load-theme 'spacemacs-light t)))

;; (if (daemonp)
;;     (add-hook 'after-make-frame-functions 'my/after-make-frame-functions-hook)
;;   (load-theme 'spacemacs-light t))

;; (set-face-background 'default "unspecified-bg")

(setq inhibit-startup-screen t)
(setq custom-safe-themes t)
(setq split-height-threshold nil)
(setq split-width-threshold 0)

(menu-bar-mode -1)
(column-number-mode t)
(line-number-mode t)
(show-paren-mode t)
(global-hl-line-mode t)

(set-face-attribute 'default nil
                    :font (font-spec :name   "Monaco"
                                     :weight 'normal
                                     :size   14))

;; (dolist (charset '(kana han cjk-misc bopomofo))
;;  (set-fontset-font (frame-parameter nil 'font)
;;                    charset
;;                    (font-spec :name   "Noto Sans CJK SC"
;;                               :weight 'normal
;;                               :size   24)))

(require-theme 'modus-themes)
(setq-default modus-themes-headings '((1 . line)
                                      (2 . rainbow-line-no-bold)
                                      (t . no-bold))
              modus-themes-links 'neutral-underline
              modus-themes-org-blocks 'gray-background
              ;; modus-themes-scale-headings t
              ;; modus-themes-scale-1 1.1
              ;; modus-themes-scale-2 1.15
              ;; modus-themes-scale-3 1.21
              ;; modus-themes-scale-4 1.27
              ;; modus-themes-scale-5 1.33)
              modus-themes-success-deuteranopia t)
(modus-themes-load-themes)
(modus-themes-load-operandi)

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

(global-auto-revert-mode t)
(delete-selection-mode 1)

(electric-pair-mode 1)
(setq-default electric-pair-inhibit-predicate
              'electric-pair-conservative-inhibit)

(setq-default tab-width 4
              indent-tabs-mode nil)

(require 'expand-region)

(require 'sudo-edit)

(setq custom-file (make-temp-file ""))
(fset 'yes-or-no-p'y-or-n-p)

(require 'recentf)
(recentf-mode 1)
(add-to-list 'recentf-exclude "\\elpa")
(setq recentf-max-menu-items 256)
(setq recentf-max-saved-items 256)

(require 'savehist)
(savehist-mode)

(require 'company)
(global-company-mode)
(setq company-show-numbers t)
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-limit 20)

(require 'avy)

(require 'vertico)
(vertico-mode)
(setq completion-styles '(orderless)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

(require 'embark)
(bind-key "M-o" 'embark-act)
(bind-key "C-h B" 'embark-bindings)
(setq prefix-help-command #'embark-prefix-help-command)

(require 'embark-consult)
(add-hook 'embark-collect-mode-hook 'embark-consult-preview-minor-mode)

(require 'org)
(setq-default org-export-with-toc nil)
(setq-default org-export-with-section-numbers nil)
(setq-default org-html-head-include-default-style nil)
(setq-default org-startup-folded 'content)

(require 'valign)
(add-hook 'org-mode-hook #'valign-mode)

(require 'org-journal)
(setq-default org-journal-dir "~/org/journal/")
(setq-default org-journal-file-type 'weekly)
(setq-default org-journal-file-format "%Y-%m-%d.org")
(setq-default org-journal-date-format "%Y-%m-%d %A")
(setq org-journal-file-header "#+TITLE: Weekly Journal\n#+STARTUP: folded")

(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  (unless (eq org-journal-file-type 'daily)
    (org-narrow-to-subtree))
  (goto-char (point-max)))

(setq org-capture-templates '(("j" "Journal entry" plain (function org-journal-find-location)
                               "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
                               :jump-to-captured t :immediate-finish t)))

(require 'yasnippet)
(add-hook 'prog-mode-hook 'yas-minor-mode)

(require 'eglot)
(setq eglot-ignored-server-capabilites '(:documentHighlightProvider))
(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)

;; (require 'flycheck)
;; (add-hook 'prog-mode-hook 'flycheck-mode)

(require 'cc-mode)
(setq c-default-style "linux")
(setq c-basic-offset 4)
(defun my/c-mode-hook()
  (c-toggle-hungry-state 1)
  (c-toggle-comment-style -1))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'my/c-mode-hook)

(require 'rust-mode)
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))

(require 'go-mode)
(add-hook 'go-mode-hook 'eglot-ensure)
(setq-default eglot-workspace-configuration
              '((gopls
                 (usePlaceholders . t))))
(with-eval-after-load "go-mode"
  (with-eval-after-load "project"
    (defun project-find-go-module (dir)
      (when-let ((root (locate-dominating-file dir "go.mod")))
        (cons 'go-module root)))
    (cl-defmethod project-root ((project (head go-module)))
      (cdr project))
    (add-hook 'project-find-functions #'project-find-go-module)))


(setq go-translate-token-current (cons 430675 2721866130))

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
