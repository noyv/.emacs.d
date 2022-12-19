;;; package --- Summary
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-

;; (require 'package)
(setq package-archives '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "http://mirrors.ustc.edu.cn/elpa/nongnu/")))

(menu-bar-mode -1)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(fset 'yes-or-no-p'y-or-n-p)
(setq create-lockfiles nil)
(setq exec-path (append exec-path '("/usr/local/go/bin/" "~/go/bin/")))
(setenv "PATH" (concat "/usr/local/go/bin/" ":" "~/go/bin/" ":" (getenv "PATH")))
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(column-number-mode t)
(global-hl-line-mode)

(custom-set-variables
 '(inhibit-startup-screen t)
 '(initial-major-mode 'fundamental-mode)
 '(initial-scratch-message nil)
 '(split-height-threshold nil)
 '(split-width-threshold 0)
 '(make-backup-files nil)
 '(auto-save-default nil)
 '(custom-safe-themes t))

(add-to-list 'default-frame-alist '(font . "FiraCode-14"))

(load-theme 'modus-operandi t)

;; (require 'elec-pair)
(add-hook 'prog-mode-hook #'electric-pair-mode)
(custom-set-variables
 '(electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; (require 'recentf)
(recentf-mode t)
(add-to-list 'recentf-exclude "\\elpa")
(custom-set-variables
 '(recentf-max-menu-items 128)
 '(recentf-max-saved-items 128))

;; (require 'dired)
(custom-set-variables
 '(dired-recursive-deletes 'always)
 '(delete-by-moving-to-trash t))

;; (require 'isearch)
(custom-set-variables
 '(isearch-lazy-count t)
 '(lazy-count-prefix-format nil)
 '(lazy-count-suffix-format " [%s/%s]"))

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
(custom-set-variables
 '(evil-symbol-word-search t)
 '(evil-undo-system 'undo-redo)
 '(evil-disable-insert-state-bindings t)
 '(evil-want-C-u-scroll t)
 '(evil-want-keybinding nil))
(evil-mode 1)

;; (require 'vertico)
(vertico-mode)

(custom-set-variables
 '(completion-styles '(basic partial-completion orderless))
 '(completion-category-overrides '((file (styles basic partial-completion)))))

(require 'consult)
(savehist-mode)

(general-leader-def "ff" 'find-file)
(general-leader-def "fo" 'find-file-other-window)
(general-leader-def "fd" 'dired-jump)
(general-leader-def "fr" 'consult-buffer)
(general-leader-def "fe" 'consult-flycheck)
(general-leader-def "fl" 'consult-line)
(consult-customize consult-ripgrep
                   :initial (consult--async-split-initial (thing-at-point 'symbol)))

;; (require 'avy)
(general-def '(normal motion) global-map "s" #'avy-goto-char-timer)

;; (require 'corfu)
(global-corfu-mode)
(custom-set-variables
 '(corfu-auto t)
 '(corfu-auto-prefix 2)
 '(corfu-quit-no-match 'separator))

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
(setq eglot-stay-out-of '(flymake))
(setq eglot-workspace-configuration
      '((gopls
         (usePlaceholders . t))))
(custom-set-variables
 '(eglot-events-buffer-size 0))
(general-leader-def "ca" 'eglot-code-actions)
(general-leader-def "ci" 'eglot-code-action-organize-imports)
(general-leader-def "cr" 'eglot-rename)
(general-leader-def "cf" 'eglot-format)

;; (require 'go-mode)
(custom-set-variables
 '(gofmt-command "goimports"))
(setq go-test-args "-v -count=1")

;; (require 'org)
;; (require 'org-id)
;; (require 'ox)
;; (require 'org-roam)
(custom-set-variables
 '(org-export-with-toc nil)
 '(org-html-head-include-default-style nil)
 ;; '(org-export-with-section-numbers nil)
 '(org-id-locations-file (convert-standard-filename "~/org/.org-id-locations"))
 '(org-startup-folded 'content)
 '(org-roam-directory (file-truename "~/org/"))
 '(org-roam-db-location "~/org/org-roam.db")
 '(org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))))
(general-def '(normal motion) org-mode-map "TAB" #'org-cycle :keymaps 'override)

;; (setq plantuml-exec-mode 'server)
;; (setq plantuml-server-url "http://172.16.0.201:9000")
;; (require 'ob-plantuml)
;; (setq org-plantuml-jar-path "~/org/plantuml.jar")
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((plantuml . t))) ; this line activates plantuml

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(blamer org-roam yasnippet vterm-toggle vertico valign sudo-edit rust-mode plantuml-mode orderless go-tag general evil corfu-terminal avy flycheck yaml-mode protobuf-mode gotest go-impl flycheck-golangci-lint consult-flycheck))
 '(pdf-view-midnight-colors '("#eeeeee" . "#000000")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here

