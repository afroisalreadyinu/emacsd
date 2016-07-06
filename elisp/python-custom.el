(defun classes-and-methods ()
  (interactive)
  (occur "\\(class .*\\)\\|\\(def .*\\)"))

(add-hook 'python-mode-hook
          (lambda ()
            (progn
              (hs-minor-mode t)
              (local-set-key "\C-c2" 'insert-pdb)
              (local-set-key "\C-c3" 'insert-todo))))

(defun find-import ()
  (interactive)
  (let ((current (point)))
    (re-search-backward "[\n \.\(\)\"\',]" nil t)
    (forward-char)
    (let* ((start (point))
           (end (- (re-search-forward "[\n \.\(\)\"\',]" nil t) 1))
           (entity (buffer-substring-no-properties start end))
           (re (format "import \\(\\(\(\n?\\)[^\)]*\\)?\\(.*\\)?%s" entity)))
      (goto-char (point-min))
      (unless (re-search-forward re nil t)
        (goto-char current)
        (message "import could not be found")))))

(setq python-pdbtrack-do-tracking-p nil)

(provide 'python-custom)
