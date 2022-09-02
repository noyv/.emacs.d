;;; package --- Summary
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-

;; (require 'package)
(setq package-archives '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "http://mirrors.ustc.edu.cn/elpa/nongnu/")))

(defvar package-list '(
                       avy
                       consult
                       consult-flycheck
                       corfu
                       corfu-terminal
                       eglot
                       embark
                       embark-consult
                       evil
                       evil-terminal-cursor-changer
                       flycheck
                       flycheck-golangci-lint
                       general
                       go-impl
                       go-mode
                       go-tag
                       gotest
                       orderless
                       org-roam
                       plantuml-mode
                       popon
                       protobuf-mode
                       rust-mode
                       sudo-edit
                       valign
                       vertico
                       vterm
                       vterm-toggle
                       yaml-mode
                       yasnippet
                       ))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(setq max-specpdl-size 16384)
(menu-bar-mode -1)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-screen t)
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)
(setq custom-safe-themes t)
(setq split-height-threshold nil)
(setq split-width-threshold 0)
(fset 'yes-or-no-p'y-or-n-p)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq exec-path (append exec-path '("/usr/local/go/bin/" "~/go/bin/")))
(setenv "PATH" (concat "/usr/local/go/bin/" ":" "~/go/bin/" ":" (getenv "PATH")))
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

(column-number-mode t)
(global-hl-line-mode)

(add-to-list 'default-frame-alist '(font . "FiraCode-11"))
(load-theme 'modus-operandi t)

;; (require 'elec-pair)
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(add-hook 'prog-mode-hook #'electric-pair-mode)

;; (require 'recentf)
(recentf-mode t)
(add-to-list 'recentf-exclude "\\elpa")
(setq recentf-max-menu-items 32)
(setq recentf-max-saved-items 32)

;; (require 'dired)
(setq dired-recursive-deletes 'always)
(setq delete-by-moving-to-trash t)

;; (require 'isearch)
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format nil)
(setq lazy-count-suffix-format " [%s/%s]")

;; (require 'general)
(general-create-definer general-leader-def
  :states 'normal
  :keymaps 'override
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  :prefix-command 'leader-prefix-command
  :prefix-map 'leader-prefix-map)
(general-leader-def "<SPC>" 'execute-extended-command)
(general-leader-def "="     '(:keymap vc-prefix-map :which-key "vc"))
(general-leader-def "p"     '(:keymap project-prefix-map :which-key "project"))

(keymap-global-set "s-t" #'vterm-toggle)

;; (require 'evil)
(defalias #'forward-evil-word #'forward-evil-symbol)
(setq evil-symbol-word-search t)
(setq evil-undo-system 'undo-redo)
(evil-mode 1)
(evil-set-initial-state 'vterm-mode 'emacs)

;; (require 'xref)
(general-def 'normal xref--xref-buffer-mode-map "RET" #'xref-goto-xref-and-quit :keymaps 'override)

;; (require 'evil-terminal-cursor-changer)
(unless (display-graphic-p)
  (evil-terminal-cursor-changer-activate))

;; (require 'vertico)
(vertico-mode)

(setq completion-styles '(basic partial-completion orderless)
      completion-category-overrides '((file (styles basic partial-completion))))

;; (require 'consult)
;; (setq consult-buffer-filter '("^ " "\\*straight*"))
(savehist-mode)
;; (consult-customize
;; consult-ripgrep consult-git-grep consult-grep
;; consult-bookmark consult-recent-file consult-xref
;; consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
;; :preview-key (kbd "M-."))

(general-leader-def "ff" 'find-file)
(general-leader-def "fo" 'find-file-other-window)
(general-leader-def "fd" 'dired-jump)
(general-leader-def "fr" 'consult-buffer)
(general-leader-def "fe" 'consult-flycheck)
(general-leader-def "fl" 'consult-line-thing-at-point)

(keymap-global-set "M-o" 'embark-act)
(setq prefix-help-command 'embark-prefix-help-command)

(add-hook 'embark-collect-mode-hook 'embark-consult-preview-minor-mode)

(general-def '(normal motion) global-map "s" #'avy-goto-char-timer)

;; (require 'corfu)
(global-corfu-mode)
(setq corfu-auto t
      corfu-auto-prefix 2
      corfu-quit-no-match 'separator)

;; (require 'corfu-terminal)
(unless (display-graphic-p)
  (corfu-terminal-mode +1))

;; (require 'yasnippet)
(add-hook 'prog-mode-hook 'yas-minor-mode)

;; (require 'flycheck)
(add-hook 'prog-mode-hook 'flycheck-mode)
(eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))

;; (require 'eglot)
(setq eglot-ignored-server-capabilites '(:documentHighlightProvider))
(setq eglot-events-buffer-size 0)
(setq eglot-stay-out-of '(flymake))
(setq eglot-workspace-configuration
      '((gopls
         (usePlaceholders . t))))
(general-leader-def "ca" 'eglot-code-actions)
(general-leader-def "ci" 'eglot-code-action-organize-imports)
(general-leader-def "cr" 'eglot-rename)
(general-leader-def "cf" 'eglot-format)

;; when lang c/c++
(setq c-default-style "linux")
(setq c-basic-offset 4)
(defun my/c-mode-hook()
  "Local config for c mode."
  (c-toggle-hungry-state 1)
  (c-toggle-comment-style -1))
(add-hook 'c-mode-hook 'my/c-mode-hook)

;; (require 'go-mode)
(setq gofmt-command "goimports")
(setq go-test-args "-v -count=1")

;; (require 'rust-mode)
(setq plantuml-exec-mode 'server)
(setq plantuml-server-url "http://172.16.0.201:9000")

;; (require 'org)
(setq org-startup-folded 'content)
(general-def '(normal motion) org-mode-map "TAB" #'org-cycle :keymaps 'override)

;; (require 'org-id)
(setq org-id-locations-file (convert-standard-filename
                             "~/org/.org-id-locations"))

;; (require 'ox)
(setq org-export-with-toc nil)
;; (setq org-export-with-section-numbers nil)
(setq org-html-head-include-default-style nil)

;; (require 'ob-plantuml)
(setq org-plantuml-jar-path "~/org/plantuml.jar")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t))) ; this line activates plantuml

;; (require 'org-roam)
(setq org-roam-directory (file-truename "~/org/"))
(setq org-roam-db-location "~/org/org-roam.db")
(setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-disable-insert-state-bindings t)
 '(evil-want-C-u-scroll t)
 '(package-selected-packages
   '(org-roam yasnippet vterm-toggle vertico valign sudo-edit rust-mode plantuml-mode orderless go-tag general evil-terminal-cursor-changer evil embark-consult eglot corfu-terminal avy))
 '(pdf-view-midnight-colors '("#eeeeee" . "#000000")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
