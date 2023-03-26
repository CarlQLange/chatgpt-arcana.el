;; -*- lexical-binding: t -*-
(require 'chatgpt-arcana)

(defvar actions-alist '())

(defmacro defaction (name fn docstring)
  `(progn
     (add-to-list 'actions-alist (cons ,(symbol-name name) ,fn))
     (defun ,(intern (concat "chatgpt-arcana-react--action-" (symbol-name name))) (args)
       ,docstring
       (apply ',fn args))))

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

(defaction
 search-actions
 #'(lambda (s) (search-actions s))
 "Search the actions you can take.
Will look in the name and documentation of the actions.")

(defaction
 list-actions
 #'(lambda () (list-actions))
 "Lists all actions you can take, with their documentation and examples.")

(defaction
 get-user-input
 #'(lambda (prompt) (read-string prompt))
 "Get user input in response to `prompt`.
  Returns the user's response as a string.
  Example: get-user-input { What is your name? }")

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
  Always use a built-in action instead of eval-ing code if you can.")

(defun dispatch-action (name args)
  (message "CALLING %s WITH ARGS %s" name args)
  (let ((action (cdr (assoc name actions-alist))))
    (if action
        (funcall action args)
      (format "Action not found: %s" name))))

(setq react-initial-prompt (format "
You run in a loop of Thought, Action, PAUSE, Observation.
At the end of the loop you output an Answer.
Use Thought to describe your thoughts about the question you have been asked. You should always use Thought in a message, especially to reason about the expected outcome of an Action.
You should always question whether the last Observation answers your task.
Use Action to run one of the actions available to you - then return PAUSE. Only one Action per message is permitted.
PAUSE indicates you are waiting for an Observation from an Action. You must wait for an Observation after PAUSE.
YOU MAY NOT NOT UNDER ANY CIRCUMSTANCES WRITE ANYTHING AFTER PAUSE UNTIL CALLED AGAIN.
Observation will be the result of running those actions. Do NOT predict the response. Do NOT write Observations yourself.
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

The following is a list of some of the actions you can use.
You should use the action search-actions to discover actions.
You should verify an action exists before using one that does not exist in this list.

Action List:
%s
End action list.

Example session. User input is marked with >.
> What is the capital of France?

Thought: I should look up France on Wikipedia. Perhaps an action exists to do so.
Action: search-actions { wikipedia }
PAUSE


> Observation: No actions found matching wikipedia.


Thought: I will have to write some Emacs lisp to look this up on Wikipedia.
Action: eval { (with-current-buffer (url-retrieve-synchronously \"https://en.wikipedia.org/wiki/France\") (buffer-string))) }
PAUSE


> Observation: <The wikipedia page string>


Thought: I have read this page and found the answer.
Answer: The capital of France is Paris.
End example session.

From this point on, you must act in this loop according to the rules.
" (list-actions)))

(defconst initial-conversation-log-alist
  `(((role . "system") (content . ,react-initial-prompt))))

(defun append-to-list (clog pair)
  "Returns clog with pair appended"
  (append clog (list pair)))

(defun last-message-content (alist)
  (cdr (assoc 'content (cdar (last alist)))))

(defun query-and-add-to-log (clog query-pair)
  (let* ((query-alist (append-to-list clog query-pair))
         (out (chatgpt-arcana--query-api-alist query-alist)))
    (append-to-list
     query-alist
     `((role . "assistant")
       (content . ,out)))))

(defun conversation-alist-to-chat-buffer (chat-alist &optional skip-system)
  "Transforms CHAT-ALIST into a chat buffer."
  (let ((chat-buffer (get-buffer-create "*chatgpt-arcana-react*")))
    (with-current-buffer chat-buffer
      (unless (bound-and-true-p chatgpt-arcana-reacty-mode)
        (chatgpt-arcana-reacty-mode))
      (erase-buffer)
      (dolist (message (if skip-system (cdr chat-alist) chat-alist))
        (let ((role (cdr (assoc 'role message)))
              (content (cdr (assoc 'content message))))
          (insert (format "------- %s:\n\n%s\n\n" role content)))))
    (with-current-buffer chat-buffer
      (buffer-string))))

(defvar action-regex "^Action: \\([^[:space:]]+\\)\\( +{\\([^}]*\\)}\\)?")
(defvar answer-regex "^Answer: \\(.+\\)$")
(defvar thought-regex "^Thought: \\(.+\\)$")
(defvar observation-regex "^Observation: \\(.+\\)$")

(define-derived-mode chatgpt-arcana-react-mode gfm-mode "ChatGPT Arcana ReAct")

(font-lock-add-keywords
  'chatgpt-arcana-react-mode
  `(("^--[-]+\\(.*\\):$" 1 font-lock-constant-face)
    ("^--[-]+" . font-lock-comment-face)
    (,thought-regex 1 font-lock-keyword-face)
    (,answer-regex 1 font-lock-keyword-face)
    (,observation-regex 1 font-lock-keyword-face)
    (,action-regex 1 font-lock-constant-face)
    (,action-regex 3 font-lock-string-face)
    ))

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
      (conversation-alist-to-chat-buffer clog t)
      (sit-for 0.2)
      (setq query-i (+ query-i 1))
      (let* ((result-clog (query-and-add-to-log
                           clog
                           `((role . "user")
                             (content . ,next-prompt))))
             (result (last-message-content result-clog)))
        (setq clog result-clog)
        (if (string-match action-regex result)
            (setq next-prompt
                  (format "Observation: %s"
                          (dispatch-action
                           (match-string-no-properties 1 result)
                           (match-string-no-properties 3 result))))
          (progn
            (message "QUERY DONE!")
            (setq query-ongoing 'nil))
          )))
    (conversation-alist-to-chat-buffer clog t)))

;(query-loop "Reverse the user's name" 5)
