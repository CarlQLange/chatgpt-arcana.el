(defun chatgpt-arcana-commitmsg--git-get-current-diff ()
  "Get the current diff from Git."
  (let ((default-directory (magit-toplevel)))
    (shell-command-to-string "git diff HEAD")))
(defun chatgpt-arcana-commitmsg--git-get-last-diffs-and-commits (count)
  "Get the last COUNT diffs and commits from Git as an alist."
  (let* ((branch (magit-rev-parse "--abbrev-ref" "HEAD"))
         (default-directory (magit-toplevel))
         (log-command (concat "git log " branch " -" (number-to-string count) " -p --pretty=format:'COMMIT:%B%nDIFF:'")))
    (let* ((output (concat (shell-command-to-string log-command) "COMMIT:"))
           (commit-messages)
           (diffs)
           (matched-lines)
           (in-commit)
           (in-diff))
      (dolist (line (split-string output "\n"))
        (when (string-prefix-p "COMMIT:" line)
          (when in-diff
            (push (string-join (reverse matched-lines) "\n") diffs)
            (setq matched-lines nil))
          (setq in-commit t)
          (setq in-diff nil))
        (when (string-prefix-p "DIFF:" line)
          (when in-commit
            (push (string-join (reverse matched-lines) "\n") commit-messages)
            (setq matched-lines nil))
          (setq in-diff t)
          (setq in-commit nil))
        (push (replace-regexp-in-string "^\\(?:COMMIT\\|DIFF\\):" "" line) matched-lines))
      (reverse (cl-mapcar #'list commit-messages diffs)))))

(defcustom chatgpt-arcana-commitmsg-system-prompt
  "Your job is simple: write a commit message based on the provided diff and previous commit messages.
It is crucial that you match the style and tone of previous commit messages as much as possible.
Do NOT pay attention to the content of previous messages, only the style and tone.
Be concise."
  "System prompt for commit message generation"
  :type 'string :group 'chatgpt-arcana)

(defun chatgpt-arcana--build-alist-from-history ()
  (let* ((diffs-commits (chatgpt-arcana-commitmsg--git-get-last-diffs-and-commits 4))
         (diffs (mapcar #'cadr diffs-commits))
         (commits (mapcar #'car diffs-commits))
         (last-commits (mapcar (lambda (commit) (nth 4 (split-string commit "\n"))) (cdr commits)))
         (initial-commit (car commits)))
    (list (list (cons 'role "system")
                (cons 'name "example_user")
                (cons 'content (format "Please write a commit message based on the following diff:\n```\n%s\n```" (nth 1 diffs))))
          (list (cons 'role "system")
                (cons 'name "example_assistant")
                (cons 'content (nth 1 commits)))
          (list (cons 'role "system")
                (cons 'name "example_user")
                (cons 'content (format "Please write a commit message based on the following diff:\n```\n%s\n```" (nth 2 diffs))))
          (list (cons 'role "system")
                (cons 'name "example_assistant")
                (cons 'content (nth 2 commits))))))

(defun chatgpt-arcana-commitmsg-query-api (prompt)
  "Query the API with the given PROMPT for a commit message."
  (chatgpt-arcana--query-api-alist
   `(((role . "system") (content . ,chatgpt-arcana-commitmsg-system-prompt))
     ,@(chatgpt-arcana--build-alist-from-history)
     ((role . "user") (content . ,prompt)))))

(defun chatgpt-arcana-commitmsg ()
  "Get a commit message based on the current diff and previous commit messages."
  (interactive)
  (let* ((diff (chatgpt-arcana-commitmsg--git-get-current-diff))
         (last-commits (chatgpt-arcana-commitmsg--git-get-last-few-commits 4))
         (prompt (format "Please write a commit message in keeping with the previous commit messages.
Here are some previous commit messages in this repo, separated by a hyphen:

```
%s
```

Your message is about the following diff:

```
%s
```"
                         last-commits
                         diff))
         (commit-msg (chatgpt-arcana-commitmsg-query-api prompt)))
    (insert commit-msg)))
