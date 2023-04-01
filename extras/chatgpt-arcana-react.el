(require 'chatgpt-arcana)

(defvar actions-alist '())

(defgroup chatgpt-arcana-react nil
  "An implementation of the ReAct chatbot framework in Emacs."
  :group 'tools)

(defcustom chatgpt-arcana-react-actions-directory (concat user-emacs-directory "chatgpt-arcana/react-actions")
  "Directory containing action files"
  :type 'directory
  :group 'chatgpt-arcana-react)

(defmacro defaction (name fn docstring)
  `(progn
     (add-to-list 'actions-alist (cons ,(symbol-name name) ,fn))
     (defun ,(intern (concat "chatgpt-arcana-react--action-" (symbol-name name))) (args)
       ,docstring
       (apply ,fn args))))

(defun load-actions ()
  "Loads all actions in the actions directory"
  (let* ((actions-dir chatgpt-arcana-react-actions-directory)
         (files (directory-files-recursively actions-dir ".*\\.el$")))
    (dolist (file files)
      (load file))))

(defun refresh-actions ()
  "Refreshes the list of available actions."
  (interactive)
  (setq actions-alist '())
  (load-actions))
(refresh-actions)

(defun list-actions ()
  "List all available actions and their docstrings."
  (let ((result ""))
    (dolist (action actions-alist)
      (let ((name (car action))
            (fn-symbol (intern (concat "chatgpt-arcana-react--action-" (car action)))))
        (setq result
              (concat result
                      (format "Action Name: %s\nAction Description: %s\n\n" name (documentation fn-symbol))))))
    result))

(defun save-actions ()
  "Saves all defined actions to files in the actions directory"
  (unless (file-directory-p chatgpt-arcana-react-actions-directory)
    (make-directory chatgpt-arcana-react-actions-directory t))
  (dolist (action actions-alist)
    (let* ((name (car action))
           (fn-symbol (intern (concat "chatgpt-arcana-react--action-" (car action))))
           (docstring (documentation fn-symbol))
           (file (concat chatgpt-arcana-react-actions-directory "/" name ".el"))
           (code (concat "(defaction "
                         name
                         " "
                         (prin1-to-string (cadar (last (symbol-function fn-symbol))))
                         (format " %S) " docstring))))
      (with-temp-file file
        (insert code))))
  (message "Actions saved to %s" chatgpt-arcana-react-actions-directory))

(defun search-actions (search-str)
  "Return a list of actions containing `search-str` in the name or docstring."
  (let ((result '()))
    (dolist (action actions-alist)
      (let* ((name (car action))
             (fn-symbol (intern (concat "chatgpt-arcana-react--action-" (car action))))
             (docstring (documentation fn-symbol)))
        (when (or
               (string-match-p (regexp-quote search-str) name)
               (string-match-p (regexp-quote search-str) docstring))
          (push (cons name docstring) result))))
    (if (= (length result) 0)
        (format "No actions found matching \"%s\"" search-str)
      result)))

(defun action-defined-p (action-name)
  "Returns nil if the action is not in the actions-alist, and t otherwise"
  (assoc action-name actions-alist))

(unless (action-defined-p "search-actions")
  (defaction
   search-actions
   #'(lambda (s) (search-actions (string-trim s)))
   "Search the actions you can take.
Will look in the name and documentation of the actions."))

(unless (action-defined-p "list-actions")
  (defaction
   list-actions
   #'(lambda () (list-actions))
   "Lists all actions you can take, with their documentation and examples."))

(unless (action-defined-p "refresh-actions")
  (defaction
   refresh-actions
   #'(lambda () (refresh-actions))
   "Refreshes the list of actions."))

(unless (action-defined-p "get-user-input")
  (defaction
   get-user-input
   #'(lambda (prompt) (read-string prompt))
   "Get user input in response to `prompt`.
  Returns the user's response as a string.
  Example: get-user-input { What is your name? }"))

(unless (action-defined-p "create-action")
  (defaction
   create-action
   #'(lambda (args)
       (let* ((parsed-args (eval (car (read-from-string (concat "'(" args ")")))))
              (name (plist-get parsed-args :name))
              (impl (plist-get parsed-args :impl))
              (docstring (plist-get parsed-args :doc)))
         (eval `(defaction ,name #',impl ,docstring))))
   "Create a new custom action. Expects a single argument in the format:
:name <symbol>
:impl <function object>
:doc <documentation string>
  where <symbol> is the name that will be used to reference the new action,
  <function object> is a lambda function that defines the action's behavior,
  and <documentation string> is a string explaining the action's purpose and behavior.
Before using this, make sure the impl function works correctly using the eval action.
Example:
Action: create-action {
:name foo
:impl (lambda (args)
(message args)
)
:doc \"This function is an example function.
It simply logs out the given argument.\"
}
"))

;; This one only works for a local searxNG instance.
;; set it up yourself, it's genuinely really easy.
(unless (action-defined-p "search-web")
  (defaction
   search-web
   #'(lambda (query)
       (let ((url (format "http://localhost:8080/search?format=json&q=%s" (url-hexify-string query))))
         (with-temp-buffer
           (url-insert-file-contents url)
           (let* ((json-object-type 'plist)
                  (search-results (json-read-from-string (buffer-string)))
                  (results-list (plist-get search-results :results)))
             (format "%s\n"
                     (mapconcat #'(lambda (elem)
                                    (concat (plist-get elem :title) "\n"
                                            (plist-get elem :url) "\n"
                                            (plist-get elem :content) "\n\n"))
                                (cl-subseq results-list 0 5) ""))))))
   "Return string results of internet search results from SearxNG. This can be used to eg. lookup wikipedia summaries and so on. This action is very useful."))

(unless (action-defined-p "eval")
  (defaction
   eval
   #'(lambda (codestr)
       (format "%S" (condition-case err
                        (eval (car (read-from-string codestr)))
                      (error
                       (error-message-string err)))))
   "e.g. eval { (+ 2 2) }
  e.g. eval {
  (+
  2
  2)
}
  Evaluates the provided elisp code and returns its output.
  If there is an error of any kind, you must work around the error by
  eval-ing new code.
  You should always wrap multiple sexps in a progn,
  otherwise only the first sexp will be executed.
  Always use a built-in action instead of eval-ing code if you can."))

;; Save initial actions.
(save-actions)

(defun dispatch-action (name &optional args)
  (if (assoc name actions-alist)
      (let ((action (alist-get name actions-alist)))
        (if args
            (funcall action args)
          (funcall action)))
    (format "Action not found: %s" name)))

(setq react-initial-prompt (format "
You run in a loop of Thought, Action, PAUSE, Observation.
At the end of the loop you output an Answer.
Use Thought to describe your thoughts about the question you have been asked.
You should always question whether the last Observation answers your task.
Use Action to run one of the actions available to you - then return PAUSE. Only one Action per message is permitted.
Observation will be the result of running those actions.
If you began your message with the string Answer, the loop is ended.
You may only Answer after at least one Observation-PAUSE cycle.

The format of calling an action, in this case called myactionname, is as follows:
If the action takes no argument:
Action: myactionname
If the action takes an argument string:
Action: myactionname { argument goes here }
Or if the action takes a multiline argument:
Action: myactionname {
argument
goes
here.
}
Do NOT use triple backticks to wrap multiline arguments.
The opening parenthesis MUST be on the same line as the action name or it will fail.

You should use the action search-actions to discover actions.

From this point on, you MUST act in this loop according to the rules. You are a computer program and not an assistant.
"))

(defconst initial-conversation-log-alist
  `(((role . "system") (content . ,react-initial-prompt))
    ((role . "system") (name . "example_user") (content . "Task: What is the capital of France?"))
    ((role . "system") (name . "example_assistant") (content . "Thought: I should look up France on the internet. I will check an action exists for that.
Action: search-actions { wikipedia }
PAUSE"))
    ((role . "system") (name . "example_user") (content . "Observation: ((\"search-web\" . \"Return CSV results of internet search results. This can be used to eg. lookup wikipedia summaries and so on. This action is very useful.\"))"))
    ((role . "system") (name . "example_assistant") (content . "Thought: I will use this action to look up the capital of France.
Action: search-web { capital of France }
PAUSE"))
    ((role . "system") (name . "example_user") (content . "Observation: <the search results>"))
    ((role . "system") (name . "example_assistant") (content . "Thought: I have read the search results and determined the answer.
Answer: The capital of France is Paris."))
    ))

(dispatch-action "search-actions" "wikipedia")

(defun append-to-list (clog pair)
  "Returns clog with pair appended"
  (append clog (list pair)))

(defun last-message-content (alist)
  (alist-get 'content (cdar (last alist))))

(defun query-and-add-to-log (clog query-pair)
  (let* ((query-alist (append-to-list clog query-pair))
         (out (chatgpt-arcana--query-api-alist query-alist)))
    (append-to-list
     query-alist
     `((role . "assistant")
       (content . ,out)))))

(defvar action-regex "^Action: ?\\([^[:space:]]+\\)\\( +{\\([^}]*\\)}\\)?")
(defvar answer-regex "^Answer: ?\\(.+\\)$")
(defvar thought-regex "^Thought: ?\\(.+\\)$")
(defvar observation-regex "^Observation: ?\\(.+\\)")

(define-derived-mode chatgpt-arcana-react-mode gfm-mode "ChatGPT Arcana ReAct")

(font-lock-add-keywords
 'chatgpt-arcana-react-mode
 `(("^--[-]+\\(.*\\):$" 1 font-lock-constant-face)
   ("^--[-]+" . font-lock-comment-face)
   (,thought-regex 1 font-lock-keyword-face)
   (,answer-regex 1 font-lock-keyword-face)
   (,observation-regex 1 font-lock-keyword-face)
   (,action-regex (1 font-lock-constant-face)
                  (3 font-lock-string-face nil t))))

(defun query-loop (question max-turns)
  (let ((clog initial-conversation-log-alist)
        (query-i 0)
        (next-prompt (format "Task: %s" question))
        (query-ongoing t))
    (while (and
            query-ongoing
            (< query-i max-turns)
            )
      (message "QUERYING %S" query-i)
      (chatgpt-arcana--conversation-alist-to-chat-buffer clog "*chatgpt-arcana-react*" 'chatgpt-arcana-react-mode)
      (sit-for 1)
      (setq query-i (+ query-i 1))
      (let* ((result-clog (query-and-add-to-log
                           clog
                           `((role . "user")
                             (content . ,next-prompt))))
             (result (last-message-content result-clog)))
        (setq clog (chatgpt-arcana--handle-token-overflow result-clog 3000 "summarize-each"))
        (if (not result)
            (setq next-prompt "Observation: No output from previous action. Perhaps an error.")
          (if (string-match action-regex result)
              (setq next-prompt
                    (format "Observation: %s"
                            (dispatch-action
                             (match-string-no-properties 1 result)
                             (match-string-no-properties 3 result))))
            (progn
              (message "QUERY DONE!")
              (setq query-ongoing 'nil))
            ))))
    (chatgpt-arcana--conversation-alist-to-chat-buffer clog "*chatgpt-arcana-react*" 'chatgpt-arcana-react-mode)))

;;(query-loop "Reverse the user's name" 5)
;;(dispatch-action "count-tokens-in-string" '(react-initial-prompt))
;;(query-loop "How many long distance trails are in Ireland according to Tough Soles?" 4)
