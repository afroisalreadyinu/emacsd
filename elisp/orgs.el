(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a t") 'org-todo-list)

(setq org-cycle-separator-lines 1
      org-replace-disputed-keys t
      org-log-done 'time)

(setq org-agenda-files '("~/Documents/documents/organize/"
			 "~/Documents/documents/organize/work/"
			 "~/Documents/documents/organize/ostkreuz/"))

(add-hook 'org-mode-hook 'turn-on-auto-fill)

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "DELEGATED" "|" "DONE")))

(provide 'orgs)
