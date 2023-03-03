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
;; This package provides a way to use the OpenAI API to write and modify code and text.
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

(defvar chatgpt-arcana-api-endpoint "https://api.openai.com/v1/chat/completions")

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
  "You are ChatGPT, a large language model trained by OpenAI, called from an Emacs package. Be concise."
  "A fallback system prompt used when the current major mode is not found in the `chatgpt-arcana-system-prompts-alist`."
  :type 'string
  :group 'chatgpt-arcana)

(defcustom chatgpt-arcana-system-prompts-alist
  '((programming-prompt . "You are ChatGPT, a large language model trained by OpenAI to be the perfect programmer, called from an Emacs package. You may only respond with concise code unless explicitly asked. ")
    (writing-prompt . "You are ChatGPT, a large language model trained by OpenAI to be an excellent writing assistant, called from an Emacs package. Respond concisely and carry out instructions. "))
  "An alist that maps system prompt identifiers to actual system prompts."
  :type '(alist :key-type symbol :value-type string)
  :group 'chatgpt-arcana)

(defcustom chatgpt-arcana-system-prompts-modes-alist
  '((prog-mode . programming-prompt)
    (emacs-lisp-mode . programming-prompt)
    (org-mode . writing-prompt)
    (markdown-mode . writing-prompt))
  "An alist that maps major modes to system prompt identifiers."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'chatgpt-arcana)

(defun chatgpt-arcana-get-system-prompt ()
  "Returns the system prompt based on the current major mode, or the fallback prompt if the mode is not found."
  (let* ((mode-name (symbol-name major-mode))
         (prompt-identifier (cdr (assoc major-mode chatgpt-arcana-system-prompts-modes-alist)))
         (system-prompt (or (cdr (assoc prompt-identifier chatgpt-arcana-system-prompts-alist))
                            chatgpt-arcana-fallback-system-prompt)))
    (concat system-prompt " Current Emacs major mode: " mode-name ".")))
(require 'request)

(defun chatgpt-arcana--query-api (prompt)
  "Sends a query to the OpenAI API with PROMPT and returns the first message content."
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
              (lambda (&key data &allow-other-keys)
                (message "aXXXX %S" (gethash "content" (gethash "message" (car (gethash "choices" data)))))
                (let* ((choices (gethash "choices" data))
                       (msg (gethash "message" (car choices)))
                       (content (gethash "content" msg)))
                  (message content)
                  (replace-regexp-in-string "[“”‘’]" "`" (string-trim content)))))
    :error (lambda (error-thrown)
             (message "Error: %S" error-thrown))))

;;;###autoload
(defun chatgpt-arcana-query (prompt)
  "Sends the selected region to the OpenAI API with PROMPT and the system prompt."
  (interactive "sPrompt: ")
  (let*
      ((selected-region (and (use-region-p) (buffer-substring (mark) (point))))
       (system-prompt (chatgpt-arcana-get-system-prompt)))
    (deactivate-mark)
    (with-current-buffer (get-buffer-create "*chatgpt-arcana-response*")
      (erase-buffer)
      (markdown-mode)
      (insert
       (let* ((fp (concat system-prompt " Respond in markdown. User input follows." "\n\n" prompt "\n" (and selected-region (concat "Selected region:\n"selected-region)))))
         (concat (replace-regexp-in-string "^" "> " fp nil t) "\n\n-------\n\n" (chatgpt-arcana--query-api fp)))))))

;;;###autoload
(defun chatgpt-arcana-replace-region (prompt)
  "Sends the selected region to the OpenAI API with PROMPT and replaces the region with the output."
  (interactive "sPrompt: ")
  (let ((selected-region (buffer-substring (mark) (point))))
    (deactivate-mark)
    (let ((modified-region (chatgpt-arcana--query-api (concat (chatgpt-arcana-get-system-prompt) "\n" prompt " " selected-region))))
      (delete-region (mark) (point))
      (insert modified-region))))

;;;###autoload
(defun chatgpt-arcana-insert (prompt &optional before ignore-region)
  "Sends the selected region / custom PROMPT to the OpenAI API with PROMPT and inserts the output before/after the region or at point.
   With optional argument BEFORE set to true, insert the output before the region."
  (let ((selected-region (if (and (region-active-p) (not ignore-region))
                             (buffer-substring (mark) (point))
                           nil)))
    (deactivate-mark)
    (save-excursion
      (let* ((fp (concat (chatgpt-arcana-get-system-prompt)
                         "\nUser input follows.\n\n"
                         (when selected-region (concat "Selected region:\n" " " selected-region "\n"))
                         prompt))
             (inserted-text (chatgpt-arcana--query-api fp)))
        (when selected-region
          (if before
              (goto-char (if (< (mark) (point)) (mark) (point)))
            (goto-char (if (< (mark) (point)) (point) (mark)))))
        (insert inserted-text)))))

;;;###autoload
(defun chatgpt-arcana-insert-after-region (prompt)
  "Sends the selected region to the OpenAI API with PROMPT and inserts the output after the region."
  (interactive "sPrompt: ")
  (chatgpt-arcana-insert prompt))

;;;###autoload
(defun chatgpt-arcana-insert-before-region (prompt)
  "Sends the selected region to the OpenAI API with PROMPT and inserts the output before the region."
  (interactive "sPrompt: ")
  (chatgpt-arcana-insert prompt t))

;;;###autoload
(defun chatgpt-arcana-insert-at-point (prompt)
  "Sends the custom PROMPT to the OpenAI API and inserts the output at point."
  (interactive "sPrompt: ")
  (chatgpt-arcana-insert prompt nil t))

;;;###autoload
(defun chatgpt-arcana-insert-at-point-with-context (prompt &optional num-lines)
  "Sends NUM-LINES lines of context around point to the OpenAI API with PROMPT and inserts the output at point."
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

(defun chatgpt-arcana-generate-prompt-shortcuts ()
  "Generate a list of hydra commands for each prompt in `chatgpt-arcana-common-prompts-alist`."
  (mapcar (lambda (prompt)
            (let* ((identifier (car prompt))
                   (label (capitalize (symbol-name identifier)))
                   (key (concat "s" (substring (symbol-name identifier) 0 1)))
                   (command `(,key
                              (lambda () (interactive)
                                (chatgpt-arcana-query-region ,(cdr prompt)))
                              ,label)))
              command))
          (cdr chatgpt-arcana-common-prompts-alist)))

(use-package pretty-hydra
  :config
  (eval `(pretty-hydra-define chatgpt-arcana-hydra (:color blue :quit-key "q" :title "ChatGPT Arcana")
           ("Query"
            (("a" chatgpt-arcana-query "Query")
             ("A" chatgpt-arcana-query-region "Query with region")
             ("r" chatgpt-arcana-replace-region "Replace region"))
            "Insert"
            (("i" chatgpt-arcana-insert-at-point-with-context "At point with context")
             ("I" chatgpt-arcana-insert-at-point "At point")
             ("j" chatgpt-arcana-insert-after-region "Before region")
             ("J" chatgpt-arcana-insert-before-region "After region"))
            "Shortcuts"
            (,@(chatgpt-arcana-generate-prompt-shortcuts))))))


(provide 'chatgpt-arcana)

;;; chatgpt-arcana.el ends here
