(defun add-paths (&rest paths)
  (mapcar (lambda (x) (add-to-list 'load-path x)) paths))

(setq ring-bell-function 'ignore
      confirm-kill-emacs 'y-or-n-p
      inhibit-startup-message t
      default-indicate-empty-lines t
      redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1
      make-backup-files nil
      auto-save-default nil
      transient-mark-mode nil)

(set-language-environment "utf-8")
(define-coding-system-alias 'UTF-8 'utf-8)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(server-start)
(desktop-save-mode 1)

; make completion buffers disappear after 3 seconds.
(add-hook 'completion-setup-hook
  (lambda () (run-at-time 3 nil
    (lambda () (delete-windows-on "*Completions*")))))

(when (eq system-type 'darwin)
  (setenv "PATH" (concat "/opt/local/bin:/opt/local/sbin:/usr/local/git/bin:/usr/local/bin:" (getenv "PATH")))
  (setq exec-path (append '("/opt/local/bin" "/opt/local/sbin" "/usr/local/bin") exec-path)))

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key (kbd "s-!") 'shell-command)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-x\g" 'rgrep)
(global-set-key (kbd "C-=") '(lambda () (interactive) (revert-buffer t t)))
(global-set-key (kbd "C-c b") '(lambda () (interactive) (message (buffer-file-name))))
(global-set-key [(C tab)] 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "s-m")  '(lambda () (interactive) (message "Have a nice day!")))
(global-set-key (kbd "M-SPC")  '(lambda () (interactive) (just-one-space -1)))
(global-set-key (kbd "s-w")  'kill-buffer)
(global-set-key (kbd "s-s")  'save-buffer)
(global-set-key (kbd "s-0")  'delete-window)
(global-unset-key (kbd "s-k"))

(setq ibuffer-expert t
      ibuffer-show-empty-filter-groups nil
      ibuffer-saved-filter-groups '(("default"
                                     ("BLOG" (filename . "/Documents/documents/notes/for_blog/"))
                                     ("PROJECTS" (filename . "/projects/")))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))
(add-hook 'html-mode-hook
          (lambda () (add-hook 'before-save-hook (lambda () (untabify (point-min) (point-max))) nil t)))
(add-hook 'javascript-mode-hook
          (lambda () (add-hook 'before-save-hook (lambda () (untabify (point-min) (point-max))) nil t)))

(add-hook 'c-mode-hook
	  (lambda ()
	    (unless (or (file-exists-p "makefile")
			(file-exists-p "Makefile"))
	      (set (make-local-variable 'compile-command)
		   (concat "make -k "
			   (file-name-sans-extension buffer-file-name))))))
(add-paths "~/.emacs.d/elisp"
	   "~/projects/gimme-cat")

(require 'orgs)

(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defun package-require (pkg)
  "Install a package only if it's not already installed."
  (when (not (package-installed-p pkg))
    (package-install pkg)))

(package-require 'use-package)
(package-require 's)
(package-require 'f)

(use-package vterm :load-path  "~/code/emacs-libvterm")

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
	 ("s-x" . smex)
         ("M-X" . smex-major-mode-commands)
	 ;; This is your old M-x.
         ("C-c C-c M-x" . execute-extended-command))
  :config (smex-initialize))

;; Alternatives: InputMono 10, Inconsolata 12, SourceCodePro 10, UbuntuMono 12, FiraCode 12
;; IBMPlexMono 10
(let ((font-name-size "IBM Plex Mono 12"))
  (condition-case nil
      (set-face-attribute 'default nil :font font-name-size)
    (error (message
	    (format "%s not available, you'll have to live with the default font" font-name-size)))))

(show-paren-mode 1)
(setq show-paren-style 'parenthesis) ;;or 'expression

(global-hl-line-mode 1)
;; To customize the background color
(package-require 'zenburn-theme)
(load-theme 'zenburn t)


(package-require 'ido)
(require 'ido)
(ido-mode 'both)

(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")
(setq ido-enable-flex-matching t) ;; enable fuzzy matching

(require 'utils)
(require 'gimme-cat)

(use-package abl-mode
  :load-path "~/projects/abl-mode"
  :custom (abl-mode-install-command "pip install -r requirements.txt && python setup.py develop")
  (abl-mode-test-path-module-class-separator ":")
  :hook python-mode)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-completion-addsuffix t)
 '(comint-completion-autolist t)
 '(comint-input-ignoredups t)
 '(comint-move-point-for-output t)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(eshell-scroll-to-bottom-on-input t)
 '(fill-column 80)
 '(js-indent-level 2)
 '(load-home-init-file t t)
 '(package-selected-packages
   '(python-black emmet-mode typescript-mode org-cliplink evil-collection framemove lsp-mode vtrem use-package eglot js2-mode rjsx-mode ob-go fiplr clojure-mode markdown-mode slime bash-completion highlight f csharp-mode rainbow-delimiters inf-mongo dockerfile-mode hcl-mode go-mode turkish evil-magit evil yaml-mode magit zenburn-theme smex s color-theme))
 '(warning-suppress-log-types '((comp))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(fset 'yes-or-no-p 'y-or-n-p)

; Fix copy-paste between emacs and other x programs
(setq x-select-enable-clipboard t)
(if (functionp 'x-cut-buffer-or-selection-value)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))

(use-package recentf
  :ensure t
  :bind (("C-x C-r" . recentf-open-files))
  :config (recentf-mode 1)
  (setq recentf-max-menu-items 25))


(use-package framemove
  :load-path "~/.emacs.d/elisp"
  :config (windmove-default-keybindings)
  (setq framemove-hook-into-windmove t))

(use-package javascript-mode
  :commands javascript
  :mode "\\.js\\'")

(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

; interpret and use ansi color codes in shell output windows
(ansi-color-for-comint-mode-on)

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)
	 ("C-c w" . magit-branch-manager))
  :config (setq magit-default-tracking-name-function (lambda (remote branch) branch))
  (setq magit-push-always-verify nil))

(use-package yaml-mode
  :ensure t
  :mode "\\.yml$")

(use-package evil
  :ensure t
  :custom (evil-want-fine-undo t "Want finer undo")
  :commands (evil-mode)
  :init (evil-mode 1)
  :config
  (evil-set-initial-state 'magit-mode 'emacs)
  (evil-set-initial-state 'magit-status-mode 'emacs)
  (evil-set-initial-state 'magit-diff-mode 'emacs)
  (evil-set-initial-state 'magit-log-mode 'emacs)
  (evil-set-initial-state 'magit-popup-mode 'emacs)
  (evil-set-initial-state 'magit-refs 'emacs)
  (evil-set-initial-state 'vterm-mode 'emacs)
  :bind (:map evil-normal-state-map
	      ("[tab]" . indent-for-tab-command)
	      ("C-r" . isearch-backward)))

(require 'eshell-custom)

(use-package turkish :ensure t)
(use-package go-mode :ensure t)
(use-package hcl-mode :ensure t :mode "\\.tf$")
(use-package dockerfile-mode :ensure t)
(use-package bash-completion :ensure t :config (bash-completion-setup))

(setq explicit-shell-file-name "bash")
(setq sentence-end-double-space nil)

(use-package inf-mongo :ensure t)
(use-package csharp-mode :ensure t)
(use-package markdown-mode :ensure t)

(use-package fiplr :ensure t
  :custom
  (fiplr-ignored-globs
  '((directories (".git" ".svn" ".hg" ".bzr" "node_modules"))
    (files (".#*" "*~" "*.so" "*.jpg" "*.png" "*.gif" "*.pdf" "*.gz" "*.zip")))))


(use-package js2-mode :ensure t)
(use-package rjsx-mode :ensure t :mode "\\.js$")
(use-package org-cliplink :ensure t)

(defun create-etags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (let* ((extension (file-name-extension (buffer-file-name)))
	 (output-file (f-join dir-name "TAGS"))
	 (command (format "find %s -type f -name \"*.%s\" | etags - -o %s"
			  dir-name extension output-file)))
    (message command)
    (message (shell-command-to-string command))))

(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))


(condition-case nil
    (require 'local-custom)
  (error (message "No local customization file")))

(setq gofmt-command "~/go/bin/goimports")

;; slime
(setq inferior-lisp-program "/usr/local/bin/sbcl")

(package-require 'ob-go)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((go . t)))
(put 'downcase-region 'disabled nil)

;; TODO how to make sure
;; npm i -g typescript-language-server; npm i -g typescript
;; has been executed
;; also same thing for go get golang.org/x/tools/gopls
(use-package lsp-mode
  :ensure t
  :init (setq lsp-keymap-prefix "C-c C-l")
  :config (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'typescript-mode-hook #'lsp-deferred))

(use-package typescript-mode :ensure t)

(use-package emmet-mode
  :ensure t
  :config (add-hook 'sgml-mode-hook 'emmet-mode) (add-hook 'css-mode-hook  'emmet-mode))
