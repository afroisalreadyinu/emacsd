(defun insert-pdb ()
  (interactive)
  (insert "import pdb;pdb.set_trace()")
  (indent-for-tab-command))

(defun insert-todo ()
  (interactive)
  (insert "# TODO-ulas: ")
  (indent-for-tab-command))

(global-set-key "\C-c2" 'insert-pdb)
(global-set-key "\C-c3" 'insert-todo)

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun delete-this-file ()
  "Delete this file, then close buffer."
  (interactive)
  (let ((filepath (buffer-file-name)))
    (shell-command-to-string (format "rm %s" filepath))
    (kill-buffer)
    (message (format "Deleted %s" filepath))))

(defun copy-output-of-command (command)
  "Ask for a command, run it in shell, copy its output."
  (interactive "sCommand: ")
  (kill-new (shell-command-to-string command)))

(defun find-first-non-ascii-char ()
  "Find the first non-ascii character from point onwards."
  (interactive)
  (let (point)
    (save-excursion
      (setq point
            (catch 'non-ascii
              (while (not (eobp))
                (or (eq (char-charset (following-char))
                        'ascii)
                    (throw 'non-ascii (point)))
                (forward-char 1)))))
    (if point
        (goto-char point)
        (message "No non-ascii characters."))))

(defun git-cmd (cmd)
  (shell-command-to-string (concat "git " cmd)))

(defun browse-github ()
  "Open the github page of a repo."
  (interactive)
  (let* ((origin (git-cmd "config --get remote.origin.url"))
         (branch (trim (git-cmd "rev-parse --abbrev-ref HEAD")))
         (repo-url (replace-regexp-in-string
                    "\.git\n" ""
                    (replace-regexp-in-string
                     "git@github\.com:" "http://github.com/" origin)))
         (branch-url (if (string-equal branch "master")
                         repo-url
                       (concat repo-url "/tree/" branch))))
    (browse-url branch-url)))
(global-set-key (kbd "C-c h") 'browse-github)


(defun close-if-no-file (buffer)
  (let ((filename (buffer-file-name buffer))
        (buffername (buffer-name buffer)))
    (if (and filename (not (file-exists-p filename)))
        (progn
          (kill-buffer buffer)
          (message (format "buffer %s with no file closed"  buffername))))))


(defun delete-nofile-buffers ()
  (interactive)
  (mapcar 'close-if-no-file (buffer-list)))

(provide 'utils)
