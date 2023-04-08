(defun chatgpt-arcana-autofix--error-at-point ()
  "Return the error message if the cursor is on a line with an error."
  (let ((get-errors-func nil)
        (get-message-func nil)
        (get-location-func nil))
    (cond ((bound-and-true-p flymake-mode)
           (setq get-errors-func #'flymake-diagnostics)
           (setq get-message-func #'flymake-diagnostic-text)
           (setq get-location-func #'flymake-diagnostic-beg))

          ((bound-and-true-p flycheck-mode)
           (setq get-errors-func #'flycheck-overlay-errors-at)
           (setq get-message-func #'flycheck-error-message)
           (setq get-location-func #'flycheck-error-location))
          ;; Add more modes here, if necessary.
          (t nil))
    (let ((errors (funcall get-errors-func (point))))
      (when errors
        (let ((error-at-point (car errors)))
          (when error-at-point
            (funcall get-message-func error-at-point)))))))

(defun chatgpt-arcana-autofix--get-surrounding-code-start-end (error-location)
  "Get the code relative to an error."
  (save-excursion
    (let* ((line (line-number-at-pos error-location))
           (start-line (max 1 (- line 20)))
           (end-line (+ line 20))
           (start-pos (progn (goto-char (point-min))
                             (forward-line (1- start-line))
                             (point)))
           (end-pos (progn (goto-char (point-min))
                           (forward-line end-line)
                           (point))))
      `(,start-pos ,end-pos))))

(defvar chatgpt-arcana-autofix-system-prompt
  "You are a machine that fixes a code error in any programming language.
Your task is to respond with valid code that fixes the error.
You have access to the error message and surrounding code.
You must ONLY respond with the fixed input code and no commentary.
You must respond with ALL code input as all input code will be fully replaced.
Do not include explanations, commentary, or any other text that is not the fixed code.
Do not wrap code in triple backticks."
  "System prompt for ChatGPT-arcana auto fix functionality.")

(defvar chatgpt-arcana-autofix-user-prompt
  "Please fix the following error:

Error message:
%s

Surrounding code:
%s"
  "The prompt used for automatically fixing code.")

(defun chatgpt-arcana-autofix-query-api (prompt)
  (chatgpt-arcana--query-api-alist
   `(((role . "system") (content . ,chatgpt-arcana-autofix-system-prompt))
     ((role . "system") (name . "example_user")
      (content . "Please fix the following error:

      Error message:
      Cannot read property 'length' of null

      Surrounding code:
      function getLength(arr) {
          return arr.length;
      }"))
     ((role . "system") (name . "example_assistant")
      (content . "function getLength(arr) {
        return arr && arr.length || 0;
      }"))
     ((role . "user") (content . ,prompt)))))

(defun chatgpt-arcana-autofix ()
  "Attempt to automatically fix error at point."
  (interactive)
  (let ((error-message (chatgpt-arcana-autofix--error-at-point)))
    (if error-message
        (let* ((orig-buffer (buffer-name))
               (surrounding-start-end (chatgpt-arcana-autofix--get-surrounding-code-start-end (point)))
               (surrounding-code (buffer-substring-no-properties (car surrounding-start-end) (cadr surrounding-start-end)))
               (prompt (format chatgpt-arcana-autofix-user-prompt error-message surrounding-code)))
          (message "Attempting to fix error: %s" error-message)
          (let ((fixed-code (chatgpt-arcana-autofix-query-api prompt)))
            (delete-region (car surrounding-start-end) (cadr surrounding-start-end))
            (insert fixed-code)
            (insert "\n")
            (message "Fix applied.")))
      (message "No error found at point!"))))
