(require 's)
(require 'f)

(defun insert-pdb ()
  (interactive)
  (insert "import pdb;pdb.set_trace()")
  (indent-for-tab-command))

(defcustom shell-profiles nil
  "Association list of shell profiles")

(defun save-shell-init-file (file-path file-lines)
  (let ((file-content (s-join "\n" file-lines)))
    (f-write-text file-content 'utf-8 file-path)))

(defun new-shell (name)
  "Opens a new shell buffer with the given name in
asterisks (*name*) in the current directory with and changes the
prompt to name>. If you want shell profiles to work, put the
following in ~/.bashrc:
if [ -f ~/.emacs_shell_profile_will_be_deleted ]; then
   . ~/.emacs_shell_profile_will_be_deleted
fi "
  (interactive "sName: ")
  (let ((profile (alist-get (intern name) shell-profiles))
	(profile-file-path "~/.emacs_shell_profile_will_be_deleted"))
    (if profile (progn
		  (save-shell-init-file profile-file-path profile)
		  (shell (concat "*" name "*"))
		  (sleep-for 1)
		  (f-delete profile-file-path))
      (shell (concat "*" name "*")))))

(global-set-key (kbd "C-c s") 'new-shell)
(global-set-key (kbd "C-c 2") 'insert-pdb)


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

(defun github-urls (repo-url branch filepath)
  (let* ((repo-url (replace-regexp-in-string
		    "\.git\n" ""
		    (replace-regexp-in-string
		     "git@github\.com:" "https://github.com/" origin)))
         (branch-url (if (string-equal branch "master")
                         repo-url
                       (concat repo-url "/tree/" branch)))
	 (file-url (concat repo-url "/blob/" branch "/" filepath)))
    (cons branch-url file-url)))

(defun bitbucket-urls (repo-url branch filepath)
  (let* ((repo-url (replace-regexp-in-string
		    "\.git\n" ""
		    (replace-regexp-in-string
		     "git@bitbucket\.org:" "https://bitbucket.org/" origin)))
         (branch-url (if (string-equal branch "master")
                         repo-url
                       (concat repo-url "/branch/" branch)))
	 (file-url (concat repo-url "/blob/" branch "/" filepath)))
    (cons branch-url branch-url)))


(defun browse-github (arg)
  "Open the Github or Bitbucket page of a repo. With modifier, go
to file."
  (interactive "P")
  (let* ((origin (git-cmd "config --get remote.origin.url"))
         (branch (trim (git-cmd "rev-parse --abbrev-ref HEAD")))
	 (git-base (expand-file-name (locate-dominating-file (buffer-file-name) ".git")))
	 (filepath (file-relative-name (buffer-file-name) git-base))
	 (urls (if (s-contains? "github.com" origin)
		   (github-urls origin branch filepath)
		 (bitbucket-urls origin branch filepath))))
    (browse-url (if arg (cdr urls) (car urls)))))

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

(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(global-set-key (kbd "C-c Q") 'unfill-paragraph)

(defun copy-git-branch ()
  "Show the current branch in the echo-area and add it to the `kill-ring'."
  (interactive)
  (let ((branch (magit-get-current-branch)))
    (if branch
        (progn (kill-new branch)
               (message "%s" branch))
      (user-error "There is not current branch"))))

(defun join-paragraph () (interactive)
       (let ((fill-column 999999)) (fill-paragraph nil)))

(provide 'utils)
