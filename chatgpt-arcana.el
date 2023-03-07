;;; chatgpt-arcana.el --- Uses the OpenAI API to write and modify code and text

;; Copyright (C) 2023 Carl Lange
;; Author: Carl Lange <carl@flax.ie>
;; URL: https://github.com/CarlQLange/chatgpt-arcana
;; Package-Requires: ((emacs "26.1"))
;; Version: 0.1
;;
;; This file is not part of GNU Emacs.
;;; Commentary:
;;
;; This package provides a way to use the ChatGPT API to write and modify code and text.
;;
;;; Code:

(require 'url)
(require 'json)
(require 'cl-lib)
(require 'request)
(require 'markdown-mode)

(defgroup chatgpt-arcana nil
  "An Emacs package that uses the OpenAI API to write and modify code and text."
  :group 'tools)

(defcustom chatgpt-arcana-api-key nil
  "The API key for the OpenAI API."
  :type 'string
  :group 'chatgpt-arcana)

(defvar chatgpt-arcana-chat-separator-system "------- system:\n\n")
(defvar chatgpt-arcana-chat-separator-user "\n\n------- user:\n\n")
(defvar chatgpt-arcana-chat-separator-assistant "\n\n------- assistant:\n\n")

(defvar chatgpt-arcana-api-endpoint "https://api.openai.com/v1/chat/completions")

(defcustom chatgpt-arcana-chat-autosave-interval 300
  "Interval in seconds between chat session autosaves."
  :type 'integer
  :group 'chatgpt-arcana-chat)

(defcustom chatgpt-arcana-chat-autosave-directory "~/.emacs.d/.local/cache/chatgpt-arcana/sessions"
  "Directory where chat session autosave files should be saved."
  :type 'directory
  :group 'chatgpt-arcana-chat)

(defcustom chatgpt-arcana-chat-autosave-enabled t
  "Whether chat session autosave is enabled."
  :type 'boolean
  :group 'chatgpt-arcana-chat)

(defcustom chatgpt-arcana-model-name "gpt-3.5-turbo"
  "The name of the OpenAI model to use. Some cost more than others. Beware."
  :type 'string
  :group 'chatgpt-arcana)

(defcustom chatgpt-arcana-common-prompts-alist
  '((smaller . "Refactor this code to be more concise")
    (comment . "Add brief comments to this code.")
    (explain . "Explain how this code works in a comment with line length 80.")
    (test . "Write a test case for this code"))
  "Alist of common prompts."
  :type 'alist
  :group 'chatgpt-arcana)

(defcustom chatgpt-arcana-fallback-system-prompt
  "You are a large language model living inside Emacs. Help the user and be concise."
  "A fallback system prompt used if the current major mode is not found in `chatgpt-arcana-system-prompts-alist`."
  :type 'string
  :group 'chatgpt-arcana)

(defcustom chatgpt-arcana-generated-buffer-name-prompt
  "You are an Emacs function that generate a useful and descriptive Emacs buffer name based on this content. The name should be lowercase, hyphenated, not too long.

YOU MAY RESPOND ONLY WITH THE NAME AND NO OTHER TEXT.

Example Input: Create a poem about a pear
Example Output: pear-poem
Bad output might include: emacs-buffer-poem, emacs-pear-poem, pear-poem-buffer
Example Input: Fix this large code block. The function is named some-function
Example Output: fixing-some-function
Bad output might include: buffer-fix-some-function, emacs-buffer-fix-some-function

Input follows. Don't forget - ONLY respond with the buffer name and no other text.
"
  "Prompt used to generate buffer names."
  :type 'string
  :group 'chatgpt-arcana)

(defcustom chatgpt-arcana-system-prompts-alist
  '((programming-prompt . "You are a large language model living inside Emacs, and the perfect programmer. You may only respond with concise code unless explicitly asked. ")
    (writing-prompt . "You are a large language model living inside Emacs, and an excellent writing assistant. Respond concisely and carry out instructions. ")
    (chat-prompt . "You are a large language model living inside Emacs, and an excellent conversation partner. Respond concisely. "))
  "An alist that maps system prompt identifiers to actual system prompts."
  :type '(alist :key-type symbol :value-type string)
  :group 'chatgpt-arcana)

(defcustom chatgpt-arcana-system-prompts-modes-alist
  '((prog-mode . programming-prompt)
    (emacs-lisp-mode . programming-prompt)
    (org-mode . writing-prompt)
    (markdown-mode . writing-prompt)
    (chatgpt-arcana-chat-mode . chat-prompt))
  "An alist that maps major modes to system prompt identifiers."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'chatgpt-arcana)

(defun chatgpt-arcana-chat-save-to-autosave-file ()
  "Save the current buffer to an autosave file and open it.
Or, just write the file if it already exists."
  (if (and (buffer-file-name) (equal (symbol-name major-mode) "chatgpt-arcana-chat-mode"))
      (write-file (buffer-file-name))
    (let ((orig-buffer-name (buffer-name))
          (dir (file-name-as-directory chatgpt-arcana-chat-autosave-directory))
          (filename (concat (format-time-string "%Y-%m-%d-%H-%M-%S-")
                            (chatgpt-arcana-generate-buffer-name)
                            ".chatgpt-arcana.md")))
      (unless (file-directory-p dir) (make-directory dir t))
      (write-region (point-min) (point-max) (concat dir filename) nil 'silent)
      (find-file (concat dir filename))
      (kill-buffer orig-buffer-name))))

(defun chatgpt-arcana-chat-enable-autosave ()
  "Enable autosave functionality."
  (interactive)
  (setq-local chatgpt-arcana-chat-autosave-timer
              (run-with-idle-timer chatgpt-arcana-chat-autosave-interval t
                                   'chatgpt-arcana-chat-save-to-autosave-file))
  (setq-local chatgpt-arcana-chat-autosave-enabled t))

(defun chatgpt-arcana-chat-disable-autosave ()
  "Disable autosave functionality."
  (interactive)
  (when (buffer-local-value 'chatgpt-arcana-chat-autosave-timer (current-buffer))
    (cancel-timer (buffer-local-value 'chatgpt-arcana-chat-autosave-timer (current-buffer)))
  (setq-local chatgpt-arcana-chat-autosave-enabled nil)))

(defun chatgpt-arcana-chat-toggle-autosave ()
  "Toggle autosave functionality on or off."
  (interactive)
  (if (buffer-local-value 'chatgpt-arcana-chat-autosave-enabled (current-buffer))
      (chatgpt-arcana-chat-disable-autosave)
    (chatgpt-arcana-chat-enable-autosave)))

(define-derived-mode chatgpt-arcana-chat-mode markdown-mode "ChatGPT Arcana Chat"
  "A mode for chatting with the OpenAI GPT-3 API."
  (local-set-key (kbd "C-c C-c") 'chatgpt-arcana-chat-send-message)
  (local-set-key (kbd "C-c C-r") 'chatgpt-arcana-chat-rename-buffer-automatically)
  (local-set-key (kbd "C-c C-a") 'chatgpt-arcana-chat-toggle-autosave)
  (run-with-idle-timer 5 nil 'chatgpt-arcana-chat-rename-buffer-automatically)
  (when chatgpt-arcana-chat-autosave-enabled (chatgpt-arcana-chat-enable-autosave)))

(add-to-list 'auto-mode-alist '("\\.chatgpt-arcana\\.md\\'" . chatgpt-arcana-chat-mode))

(font-lock-add-keywords
  'chatgpt-arcana-chat-mode
  '(("^--[-]+\\(.*\\):$" 1 font-lock-constant-face)
    ("^--[-]+" . font-lock-comment-face)))

(defun chatgpt-arcana-get-system-prompt ()
  "Return the system prompt based on the current major mode, or the fallback prompt if the mode is not found."
  (let* ((mode-name (symbol-name major-mode))
         (prompt-identifier (cdr (assoc major-mode chatgpt-arcana-system-prompts-modes-alist)))
         (system-prompt (or (cdr (assoc prompt-identifier chatgpt-arcana-system-prompts-alist))
                            chatgpt-arcana-fallback-system-prompt)))
    (concat system-prompt " Current Emacs major mode: " mode-name ".")))

(defun chatgpt-arcana--query-api (prompt)
  "Send a query to the OpenAI API with PROMPT and return the first message content."
  (let ((out))
    (request
      chatgpt-arcana-api-endpoint
      :type "POST"
      :data (json-encode `((model . ,chatgpt-arcana-model-name)
                           (messages . [((role . "user")
                                         (content . ,prompt))])))
      :headers `(("Content-Type" . "application/json")
                 ("Authorization" . ,(concat "Bearer " chatgpt-arcana-api-key)))
      :sync t
      :parser (lambda ()
                (let ((json-object-type 'hash-table)
                      (json-array-type 'list)
                      (json-key-type 'string))
                  (json-read)))
      :encoding 'utf-8
      :success (cl-function
                (lambda (&key response &allow-other-keys)
                  (let* ((choices (gethash "choices" (request-response-data response)))
                         (msg (gethash "message" (car choices)))
                         (content (gethash "content" msg)))
                    (setq out (replace-regexp-in-string "[“”‘’]" "`" (string-trim content))))))
      :error (lambda (error-thrown )
               (message "Error: %S" error-thrown)))
    out))

(defun chatgpt-arcana--query-api-alist (messages-alist)
  "Query the OpenAI API with formatted MESSAGES-ALIST.
The JSON should be a list of messages like (:role , role :content ,content)"
  (let ((out))
    (request
      chatgpt-arcana-api-endpoint
      :type "POST"
      :data (json-encode `(:model ,chatgpt-arcana-model-name
                           :messages ,messages-alist))
      :headers `(("Content-Type" . "application/json")
                 ("Authorization" . ,(concat "Bearer " chatgpt-arcana-api-key)))
      :sync t
      :parser (lambda ()
                (let ((json-object-type 'hash-table)
                      (json-array-type 'list)
                      (json-key-type 'string))
                  (json-read)))
      :encoding 'utf-8
      :success (cl-function
                (lambda (&key response &allow-other-keys)
                  (let* ((choices (gethash "choices" (request-response-data response)))
                         (msg (gethash "message" (car choices)))
                         (content (gethash "content" msg)))
                    (setq out (replace-regexp-in-string "[“”‘’]" "`" (string-trim content))))))
      :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
               (message "Error: %S" error-thrown))))
    out))

(defun chatgpt-arcana-generate-buffer-name (&optional prefix temp)
  "Generate a buffer name based on the first characters of the buffer.
If PREFIX, adds the prefix in front of the name.
If TEMP, adds asterisks to the name."
  (save-excursion
    (goto-char (point-min))
      (when (string= (buffer-substring-no-properties (point-min) (min 16 (point-max)))
                     (string-trim chatgpt-arcana-chat-separator-system))
        (search-forward (string-trim chatgpt-arcana-chat-separator-user) nil t))
      (let ((name
             (chatgpt-arcana--query-api-alist
              `(((role . "system") (content . ,chatgpt-arcana-generated-buffer-name-prompt))
                ((role . "user") (content . ,(buffer-substring-no-properties (point) (min (+ 1200 (point)) (point-max)))))))))
        (cond ((and prefix temp) (concat "*" prefix name "*"))
              (prefix (concat prefix "-" name))
              (temp (concat "*" name "*"))
              (t name)))))

(defun chatgpt-arcana-chat-rename-buffer-automatically ()
  "Magically rename a buffer based on its contents.
Only when the buffer isn't visiting a file."
  (interactive)
  (when (not buffer-file-name)
    (let ((new-name (chatgpt-arcana-generate-buffer-name "chatgpt-arcana-chat:" 't)))
      (unless (get-buffer new-name)
        (rename-buffer new-name)))))

(defun chatgpt-arcana-chat-string-to-alist (chat-string)
  "Transforms CHAT-STRING into a JSON array of chat messages."
  (let ((messages '())
        (regex "^-+\s*\\(.*\\):\s*$"))
    (with-temp-buffer
      (insert chat-string)
      (goto-char (point-min))
      (while (search-forward-regexp regex nil t)
        (let* ((role (match-string-no-properties 1))
               (start (point))
               (end (when (save-excursion (search-forward-regexp regex nil t))
                      (match-beginning 0)))
               (content (buffer-substring-no-properties start (or end (point-max)))))
          (push `((role . ,role) (content . ,(string-trim content))) messages))))
    (reverse messages)))

(defun chatgpt-arcana-chat-buffer-to-alist ()
  "Transforms the current buffer into a JSON array of chat messages."
  (interactive)
  (let ((chat-string (buffer-string)))
    (chatgpt-arcana-chat-string-to-alist chat-string)))

;;;###autoload
(defun chatgpt-arcana-query (prompt)
  "Sends the selected region to the OpenAI API with PROMPT and the system prompt."
  (interactive "sPrompt: ")
  (let*
      ((selected-region (and (use-region-p) (buffer-substring-no-properties (mark) (point))))
       (system-prompt (chatgpt-arcana-get-system-prompt)))
    (deactivate-mark)
    (with-current-buffer (get-buffer-create "*chatgpt-arcana-response*")
      (erase-buffer)
      (chatgpt-arcana-chat-mode)
      (insert
       (let* ((fp (concat system-prompt " Respond in markdown. User input follows." "\n\n" prompt "\n" (and selected-region (concat "\n\n"selected-region)))))
         (concat (replace-regexp-in-string "^" "> " fp nil t) chatgpt-arcana-chat-separator-assistant "" (chatgpt-arcana--query-api fp))))
      (unless (get-buffer-window "*chatgpt-arcana-response*")
        (split-window-horizontally)
        (switch-to-buffer "*chatgpt-arcana-response*")))))

;;;###autoload
(defun chatgpt-arcana-replace-region (prompt)
  "Send the selected region to the OpenAI API with PROMPT and replace the region with the output."
  (interactive "sPrompt: ")
  (let ((selected-region (buffer-substring-no-properties (mark) (point))))
    (deactivate-mark)
    (let ((modified-region (chatgpt-arcana--query-api (concat (chatgpt-arcana-get-system-prompt) "\n" prompt " " selected-region))))
      (delete-region (mark) (point))
      (insert modified-region))))

;;;###autoload
(defun chatgpt-arcana-insert (prompt &optional before ignore-region)
  "Insert text at, before, or after the selected region or point.
Send the selected region / custom PROMPT to the OpenAI API with PROMPT
and insert the output before/after the region or at point.
With optional argument BEFORE set to true, insert the output before the region.
With optional argument IGNORE-REGION, don't pay attention to the selected region."
  (let ((selected-region (if (and (region-active-p) (not ignore-region))
                             (buffer-substring-no-properties (mark) (point))
                           nil)))
    (deactivate-mark)
    (save-excursion
      (let* ((fp (concat (chatgpt-arcana-get-system-prompt)
                         "\nUser input follows.\n\n"
                         prompt
                         (when selected-region (concat "\n" " " selected-region "\n"))))
             (inserted-text (chatgpt-arcana--query-api fp)))
        (when selected-region
          (if before
              (goto-char (if (< (mark) (point)) (mark) (point)))
            (goto-char (if (< (mark) (point)) (point) (mark)))))
        (insert inserted-text)))))

;;;###autoload
(defun chatgpt-arcana-insert-after-region (prompt)
  "Send the selected region to the OpenAI API with PROMPT and insert the output after the region."
  (interactive "sPrompt: ")
  (chatgpt-arcana-insert prompt))

;;;###autoload
(defun chatgpt-arcana-insert-before-region (prompt)
  "Send the selected region to the OpenAI API with PROMPT and insert the output before the region."
  (interactive "sPrompt: ")
  (chatgpt-arcana-insert prompt t))

;;;###autoload
(defun chatgpt-arcana-insert-at-point (prompt)
  "Send the custom PROMPT to the OpenAI API and insert the output at point."
  (interactive "sPrompt: ")
  (chatgpt-arcana-insert prompt nil t))

;;;###autoload
(defun chatgpt-arcana-insert-at-point-with-context (prompt &optional num-lines)
  "Send NUM-LINES lines of context around point to the OpenAI API with PROMPT and insert the output at point."
  (interactive "sPrompt: \nnNumber of lines of context (default 3): ")
  (let ((current-line (line-number-at-pos (point))))
    (when (not current-line)
      (insert (chatgpt-arcana--query-api prompt)))
    (insert "[XXXX]")
    (let* ((current-point (- (point) 6))
           (num-lines (or num-lines 3))
           (context (buffer-substring-no-properties (pos-bol (- (- num-lines 1))) (pos-eol (+ num-lines 1))))
           (fp (concat (chatgpt-arcana-get-system-prompt) "\n" "\nYour response will be inserted at [XXXX] in the selected region. Do not exceed the bounds of this context.\n" prompt "\nSelected region:\n\n" context))
           (modified-context (chatgpt-arcana--query-api fp)))
      (save-excursion
        (goto-char current-point)
        (delete-char 6)
        (insert modified-context)))))

;;;###autoload
(defun chatgpt-arcana-start-chat (prompt)
  "Start a chat with PROMPT.
If the universal argument is given, use the current buffer mode to set the system prompt."
  (interactive "sPrompt: ")
  (let*
      ((selected-region (and (use-region-p) (buffer-substring-no-properties (mark) (point))))
       (system-prompt (chatgpt-arcana-get-system-prompt)))
    (deactivate-mark)
    (with-current-buffer (get-buffer-create "*chatgpt-arcana-response*")
      (erase-buffer)
      (chatgpt-arcana-chat-mode)
      (insert
       (let* (
              (sp (concat (if current-prefix-arg system-prompt (chatgpt-arcana-get-system-prompt)) " Respond in well-formatted markdown, with headers, tables, lists, and so on."))
              (fp (concat
                   chatgpt-arcana-chat-separator-system
                   sp
                   chatgpt-arcana-chat-separator-user
                   prompt (and selected-region (concat "\n\n"selected-region)))))
         (concat
          fp
          chatgpt-arcana-chat-separator-assistant
          (chatgpt-arcana--query-api-alist (chatgpt-arcana-chat-string-to-alist fp)))))
      (chatgpt-arcana-chat-start-new-chat-response)
      (unless (get-buffer-window "*chatgpt-arcana-response*")
        (split-window-horizontally)
        (switch-to-buffer "*chatgpt-arcana-response*")))))

(defun chatgpt-arcana-chat-start-new-chat-response ()
  "Add dividing lines and user input prompt to a buffer."
  (with-current-buffer (buffer-name)
    (goto-char (point-max))
    (unless (string-match-p "\n\n[-]+\n\n" (buffer-substring-no-properties (- (point-max) 10) (point-max)))
      (insert chatgpt-arcana-chat-separator-user))
    (goto-char (point-max))))

(defun chatgpt-arcana-chat-send-buffer-and-insert-at-end ()
  "Send the current chat buffer and insert the response at the end."
  (save-excursion
    (let* ((inserted-text (chatgpt-arcana--query-api-alist (chatgpt-arcana-chat-buffer-to-alist))))
      (goto-char (point-max))
      (when (not (string-match-p "^ *\n\n[-]+.*\n" inserted-text))
        (setq inserted-text (concat chatgpt-arcana-chat-separator-assistant inserted-text)))
      (insert inserted-text)
      (chatgpt-arcana-chat-start-new-chat-response)))
  (goto-char (point-max)))

(defun chatgpt-arcana-chat-send-message ()
  "Send a message to chatgpt, to be used in a chatgpt-arcana-chat buffer."
  (interactive)
  (chatgpt-arcana-chat-send-buffer-and-insert-at-end)
  (when chatgpt-arcana-chat-autosave-enabled
    (chatgpt-arcana-chat-save-to-autosave-file)
    ; since saving to autosave opens that file, we need to move the pointer again
    (goto-char (point-max))))

(defun chatgpt-arcana-generate-prompt-shortcuts ()
  "Generate a list of hydra commands for each prompt in `chatgpt-arcana-common-prompts-alist`."
  (mapcar (lambda (prompt)
            (let* ((identifier (car prompt))
                   (label (capitalize (symbol-name identifier)))
                   (key (concat "s" (substring (symbol-name identifier) 0 1)))
                   (command `(,key
                              (lambda () (interactive)
                                (chatgpt-arcana-query,(cdr prompt)))
                              ,label)))
              command))
          (cdr chatgpt-arcana-common-prompts-alist)))

(provide 'chatgpt-arcana)

;;; chatgpt-arcana.el ends here
