(defun starts-with (str1 str2)
  (and (> (length str1) 0)
       (string= str2
                (substring str1 0 (length str2)))))

(defun add-paths (&rest paths)
  (mapcar (lambda (x) (add-to-list 'load-path x)) paths))

(defun trim (str)
  "Trim leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(add-paths "~/.emacs.d/elisp"
           "~/Documents/documents/stuff/elisp"
           "~/Documents/documents/stuff/elisp/evil"
           "~/projects/abl-mode"
           "~/projects/find-here-mode"
           "~/projects/gimme-cat"
           "/usr/local/share/emacs/site-lisp")

(setq ring-bell-function 'ignore)

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
;;(global-set-key (kbd "C-z")  '(lambda () (interactive) (message "Have a nice day!")))
(global-set-key (kbd "s-m")  '(lambda () (interactive) (message "Have a nice day!")))
(global-set-key (kbd "M-SPC")  '(lambda () (interactive) (just-one-space -1)))
(global-set-key (kbd "s-w")  'kill-buffer)
(global-set-key (kbd "s-0")  'delete-window)

(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      '(("default"
         ("BLOG" (filename . "/Documents/documents/notes/for_blog/"))
         ("PROJECTS" (filename . "/projects/")))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))
(add-hook 'html-mode-hook
          (lambda () (add-hook 'before-save-hook (lambda () (untabify (point-min) (point-max))))))
(add-hook 'javascript-mode-hook
          (lambda () (add-hook 'before-save-hook (lambda () (untabify (point-min) (point-max))))))
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(require 'package)
(setq package-archives
      '(("marmalade" . "https://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; smex
(require 'smex)
(smex-initialize)
(global-set-key "\C-x\C-m" 'smex)
(global-set-key "\C-c\C-m" 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "s-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(condition-case nil
    (set-face-attribute 'default nil :font "Source Code Pro 11")
  (error (message "Source code pro not available, you'll have to live with the default font")))
;;(set-default-font "-adobe-courier-medium-r-normal--12-120-75-75-m-70-iso8859-1")

(show-paren-mode 1)
(setq show-paren-style 'parenthesis) ;;or 'expression

(require 'highlight-current-line)
(highlight-current-line-on t)
;; To customize the background color
;;(set-face-background 'hl-line "khaki")  ;; Emacs 22 Only
;;(set-cursor-color "red")
(require 'color-theme)
(require 'zenburn)
(color-theme-zenburn)

(require 'ido)
(ido-mode 'both)
(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")
(setq ido-enable-flex-matching t) ;; enable fuzzy matching



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-completion-addsuffix t)
 '(comint-completion-autolist t)
 '(comint-input-ignoredups t)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(comint-scroll-to-bottom-on-output t)
 '(eshell-scroll-to-bottom-on-input t)
 '(load-home-init-file t t)
 '(abl-mode-install-command "pip install -r requirements.txt && python setup.py develop")
 '(abl-mode-test-path-module-class-separator ":")
 '(js-indent-level 2))

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

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq transient-mark-mode nil)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(setq confirm-kill-emacs 'y-or-n-p)
(setq inhibit-startup-message t)
(setq default-indicate-empty-lines t)

(require 'framemove)
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)

(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
(autoload 'javascript-mode "javascript" nil t)

; interpret and use ansi color codes in shell output windows
(ansi-color-for-comint-mode-on)
;;(add-hook 'comint-mode-hook
;;        (lambda ()
;;          (add-to-list
;;           'comint-preoutput-filter-functions
;;           (lambda (output)
;;             (replace-regexp-in-string "\033\\[[0-9]+[GK]" "" output)
;;             ;(replace-regexp-in-string "\033\\[[0-9]+[A-Z]" "" output)
;;             ))))
;;

; make completion buffers disappear after 3 seconds.
(add-hook 'completion-setup-hook
  (lambda () (run-at-time 3 nil
    (lambda () (delete-windows-on "*Completions*")))))

(require 'utils)
(require 'abl-mode)
(add-hook 'python-mode-hook 'abl-mode-hook)
(require 'find-here-mode)

(require 'camelCase)
(add-hook 'find-file-hook (lambda () (camelCase-mode 1)))

(require 'gimme-cat)
(global-set-key "\C-c\k" 'gimme-cat)

(require 'magit)
(global-set-key "\C-c\g" 'magit-status)
(global-set-key "\C-c\w" 'magit-branch-manager)
(setq magit-default-tracking-name-function (lambda (remote branch) branch))
(setq magit-push-always-verify nil)

(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;;why?
(setq default-directory "/Users/ulas")

(set-language-environment "utf-8")

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; org-mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a t") 'org-todo-list)
(setq org-cycle-separator-lines 1)
(setq org-replace-disputed-keys t)
(setq org-log-done 'time)

(server-start)

(require 'evil)
(setq evil-want-fine-undo t)
(define-key evil-normal-state-map (kbd "TAB") 'indent-for-tab-command)
(define-key evil-normal-state-map (kbd "C-r") 'isearch-backward)
(evil-mode 1)
(evil-set-initial-state 'magit-mode 'emacs)
(evil-set-initial-state 'magit-status-mode 'emacs)
(evil-set-initial-state 'magit-diff-mode 'emacs)
(evil-set-initial-state 'magit-log-mode 'emacs)
(evil-set-initial-state 'magit-popup-mode 'emacs)
(evil-set-initial-state 'magit-refs 'emacs)

(define-coding-system-alias 'UTF-8 'utf-8)

(desktop-save-mode 1)


(load-library "eshell-custom")

(require 'turkish)
