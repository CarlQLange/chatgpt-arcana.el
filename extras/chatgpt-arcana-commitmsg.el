(defun chatgpt-arcana-commitmsg--git-get-current-diff ()
  "Get the current diff from Git."
  (let ((default-directory (magit-toplevel)))
    (shell-command-to-string "git diff HEAD")))

(defun chatgpt-arcana-commitmsg--git-get-last-few-commits (num-commits)
  "Get the last NUM-COMMITS commit messages from Git."
  (let ((num-commits-str (number-to-string num-commits))
        (default-directory (magit-toplevel)))
    (shell-command-to-string (concat "git log -" num-commits-str " --format=%B-"))))

(defcustom chatgpt-arcana-commitmsg-system-prompt
  "You are an advanced program used to generate commit messages.
Your job is simple: generate a commit message based on the provided diff and previous commit messages.
READ THE DIFF CLOSELY. Mention all noteworthy changes.

Attempt to match the style and tone of previous commit messages as much as possible.
The generated message MUST match the style and tone of the previous commit messages.
If they are descriptive, be descriptive; if they are concise, be concise; if they are funny, be funny."
  "System prompt for commit message generation"
  :type 'string :group 'chatgpt-arcana)

(defun chatgpt-arcana-commitmsg-query-api (prompt)
  "Query the API with the given PROMPT for a commit message."
  (chatgpt-arcana--query-api-alist
   `(((role . "system") (content . ,chatgpt-arcana-commitmsg-system-prompt))
     ((role . "system") (name . "example_user")
      (content . "Please generate a commit message based on the following diff:

diff --git a/index.html b/index.html
index b8eac52..9348782 100644
--- a/index.html
+++ b/index.html
@@ -1,5 +1,5 @@
 <html>
-<head><title>Carl's Blog</title></head>
+<head><title>Teh blog of doom!</title></head>
 <body>
-   <h1>My Blog</h1>
+   <h1>Teh blog of doom!</h1>
   <p>This is a sample HTML document.</p>
 </body>
 </html>

Here are the latest commit messages:
Create a sick new index page
-
Initialise my awesome new blog
-"))
     ((role . "system") (name . "example_assistant")
      (content . "Update blog name to be more fun

Updated both the page title and the header."))
     ((role . "user") (content . ,prompt)))))

(defun chatgpt-arcana-commitmsg ()
  "Get a commit message based on the current diff and previous commit messages."
  (interactive)
  (let* ((diff (chatgpt-arcana-commitmsg--git-get-current-diff))
         (last-commits (chatgpt-arcana-commitmsg--git-get-last-few-commits 10))
         (prompt (concat "Please generate a commit message based on the following diff:\n\n"
                         diff "\n\n"
                         "Here are the latest commit messages:\n"
                         last-commits))
         (commit-msg (chatgpt-arcana-commitmsg-query-api prompt)))
    (insert commit-msg)))
