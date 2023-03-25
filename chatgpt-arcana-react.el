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

(defaction
 capitalise
 #'(lambda (codestr) (upcase codestr))
 "eg: capitalise { str } => STR")

(defaction
 list-actions
 #'(lambda () (list-actions))
 "Lists all actions you can take, with their documentation and examples.")

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
At the end of the loop you output an Answer
Use Thought to describe your thoughts about the question you have been asked. You should always use Thought in a message, especially to reason about the expected outcome of an Action.
Use Action to run one of the actions available to you - then return PAUSE. Only one Action per message is permitted.
You may NOT UNDER ANY CIRCUMSTANCES write anything after the keyword PAUSE.
PAUSE indicates you are waiting for an Observation from an Action. You must wait for an Observation after PAUSE.
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

The following is a list of some of the actions.
You should list the actions to verify an action exists before calling one that does not exist in this list.

Action List:
%s
End action list.

Example session:

Question: What is the capital of France?
Thought: I should look up France on Wikipedia.
Action: eval: { (with-current-buffer (url-retrieve-synchronously \"https://en.wikipedia.org/wiki/France\") (buffer-string))) }
PAUSE

You will be called again with this:

Observation: France is a country. The capital is Paris.

You then output:
Answer: The capital of France is Paris.
End example session.
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

(defun conversation-alist-to-chat-buffer (chat-alist)
  "Transforms CHAT-ALIST into a chat buffer."
  (let ((chat-buffer (get-buffer-create "*chatgpt-arcana-react*")))
    (with-current-buffer chat-buffer
      (erase-buffer)
      (dolist (message chat-alist)
        (let ((role (cdr (assoc 'role message)))
              (content (cdr (assoc 'content message))))
          (insert (format "------- %s:\n\n%s\n\n" role content)))))
    (with-current-buffer chat-buffer
      (buffer-string))))

(defvar action-regex "^Action: \\([^[:space:]]+\\)\\(\s+{\\([^}]*\\)}\\)?")
(defvar answer-regex "^Answer: \\(.+\\)$")
(defvar thought-regex "^Thought: \\(.+\\)$")
(defvar observation-regex "^Observation: \\(.+\\)$")

(defun query-loop (question max-turns)
  (let ((clog initial-conversation-log-alist)
        (query-i 0)
        (next-prompt question)
        (query-ongoing t))
    (while (and
            query-ongoing
            (< query-i max-turns)
            )
      (message "QUERYING %S" query-i)
      (conversation-alist-to-chat-buffer clog)
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
    (conversation-alist-to-chat-buffer clog)))

(query-loop "Close other windows, split the window, and open the *chatgpt-arcana-react* buffer in the split." 5)
