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
(setq straight-vc-git-default-clone-depth 1)

(straight-use-package 'bind-key)
(straight-use-package 'avy)
(straight-use-package 'consult)
(straight-use-package 'eglot)
(straight-use-package 'markdown-mode)
(straight-use-package 'protobuf-mode)
(straight-use-package 'yaml-mode)

(straight-use-package 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(setq-default mode-line-buffer-identification
              '((:eval (list (abbreviate-file-name
                              (expand-file-name buffer-file-name))))))

(straight-use-package 'which-key)
(which-key-mode)
(setq which-key-idle-delay 4)

(straight-use-package 'general)
(general-define-key
 :states '(normal insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 :keymaps 'override
 "ff"    'find-file
 "fr"    'consult-buffer
 "fl"    'consult-line
 "fe"    'consult-flymake
 "gg"    'consult-ripgrep
 "v"     'er/expand-region
 "<SPC>" 'execute-extended-command
 "="     '(:keymap vc-prefix-map :which-key "vc")
 "p"     '(:keymap project-prefix-map :which-key "project"))

(setq-default evil-want-C-u-scroll t)
(setq evil-disable-insert-state-bindings t)
(straight-use-package 'evil)
(setq evil-undo-system 'undo-redo)
(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (setq-default evil-symbol-word-search t))
(evil-mode 1)

(straight-use-package 'sis)
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
;; (global-hl-line-mode t)
(add-to-list 'default-frame-alist '(font . "Monaco-14"))

;; (dolist (charset '(kana han cjk-misc bopomofo))
;;  (set-fontset-font (frame-parameter nil 'font)
;;                    charset
;;                    (font-spec :name   "Noto Sans CJK SC"
;;                               :weight 'normal
;;                               :size   24)))

(require-theme 'modus-themes)
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

(straight-use-package 'expand-region)

(straight-use-package 'sudo-edit)

(setq custom-file (make-temp-file ""))
(fset 'yes-or-no-p'y-or-n-p)

(require 'recentf)
(recentf-mode 1)
(add-to-list 'recentf-exclude "\\elpa")
(setq recentf-max-menu-items 64)
(setq recentf-max-saved-items 64)

(require 'savehist)
(savehist-mode)

(straight-use-package 'company)
(global-company-mode)
(setq company-show-numbers t)
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-limit 20)

;; (straight-use-package '( vertico :files (:defaults "extensions/*")
;;                          :includes (vertico-buffer
;;                                     vertico-directory
;;                                     vertico-flat
;;                                     vertico-indexed
;;                                     vertico-mouse
;;                                     vertico-quick
;;                                     vertico-repeat
;;                                     vertico-reverse)))
(straight-use-package 'vertico)
(vertico-mode)

(straight-use-package 'orderless)
;; (icomplete-vertical-mode)
(setq completion-styles '(orderless)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

(straight-use-package 'embark)
(bind-key "M-o" 'embark-act)
(bind-key "C-h B" 'embark-bindings)
(setq prefix-help-command #'embark-prefix-help-command)

(straight-use-package 'embark-consult)
(add-hook 'embark-collect-mode-hook 'embark-consult-preview-minor-mode)

(require 'org)
(setq-default org-export-with-toc nil)
(setq-default org-export-with-section-numbers nil)
(setq-default org-html-head-include-default-style nil)
(setq-default org-startup-folded 'content)

(straight-use-package 'valign)
(add-hook 'org-mode-hook #'valign-mode)

(straight-use-package 'org-journal)
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

(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)
(add-hook 'prog-mode-hook 'yas-minor-mode)

(straight-use-package 'eglot)
(setq eglot-ignored-server-capabilites '(:documentHighlightProvider))
(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)

(require 'cc-mode)
(setq c-default-style "linux")
(setq c-basic-offset 4)
(defun my/c-mode-hook()
  (c-toggle-hungry-state 1)
  (c-toggle-comment-style -1))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'my/c-mode-hook)

(straight-use-package 'rust-mode)
(require 'rust-mode)
(add-hook 'rust-mode-hook 'eglot-ensure)
;; (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))

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
