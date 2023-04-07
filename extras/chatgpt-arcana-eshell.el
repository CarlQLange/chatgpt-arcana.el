(defgroup chatgpt-arcana-eshell nil
  "Eshell extension for chatgpt-arcana"
  :group 'chatgpt-arcana)

(defcustom chatgpt-arcana-eshell-system-prompt
  "You are an advanced program used to transform the user's intent from text into shell commands.
Your shell dialect is Emacs' eshell.
Your job is simple: return only the command the user asked for, with no other markup or text.
Your output will be used directly in a shell!

You will be given details about the current directory.
If files were mentioned by name in the input, the first few bytes of the files will be given to you also."
  "System prompt for Eshell integration"
  :type 'string :group 'chatgpt-arcana-eshell)

(defcustom chatgpt-arcana-eshell-user-prompt
  "Please give me an eshell line to do the following: %s

Directory info:

%s

File info, may be nil:

%s"
  "User prompt for Eshell integration.
The first format specifier is the user input from shell.
The second format specifier is for a directory listing.
The third format specifier is for information about files mentioned in prompt."
  :type 'string :group 'chatgpt-arcana-eshell)

(defun chatgpt-arcana-eshell--directory-listing ()
  "Get a list of files in the current directory."
  (directory-files default-directory))

(defun chatgpt-arcana-eshell--filename-p (arg)
  "Check if ARG appears to be a file name."
  (string-match-p "\\S-+\\.\\S-" arg))

(defun chatgpt-arcana-eshell--filename-list (args)
  "Return a list of file names from ARGS."
  (seq-filter #'chatgpt-arcana-eshell--filename-p args))

(defun chatgpt-arcana-eshell--file-info (file)
  "Get information about a file based on its name."
  (let ((full-path (expand-file-name file)))
    (if (file-exists-p full-path)
        (with-output-to-string
          (with-current-buffer standard-output
            (call-process "head" nil t nil "-c" "500" full-path)))
      nil)))

(defun chatgpt-arcana-eshell-query-api (prompt)
  (chatgpt-arcana--query-api-alist
  `(((role . "system") (content . ,chatgpt-arcana-eshell-system-prompt))
    ((role . "system") (name . "example_user")
     (content . "Please give me an eshell line to do the following: total the size on disk of these files

Directory info:

.
..
chatgpt-arcana-eshell.el
chatgpt-arcana-react.el
readme.md

File info, may be nil:
"))
    ((role . "system") (name . "example_assistant")
     (content . "du -ch chatgpt-arcana-eshell.el chatgpt-arcana-react.el readme.md | grep total"))
     ((role . "user") (content . ,prompt)))))

(defun eshell/spell (&rest args)
  "Transform text descriptions of commands into shell commands using ChatGPT-arcana."
  (let* ((directory-info (chatgpt-arcana-eshell--directory-listing))
         (file-names (chatgpt-arcana-eshell--filename-list args))
         (file-info (mapcar #'chatgpt-arcana-eshell--file-info file-names))
         (prompt (format chatgpt-arcana-eshell-user-prompt
                         (s-join " " args)
                         (s-join "\n" directory-info)
                         (s-join "\n" file-info))))
    (let ((output (chatgpt-arcana-eshell-query-api prompt)))
      (if (or (not output) (string-blank-p output))
          (eshell-error "No output found")
        (progn
          (insert output))))))
