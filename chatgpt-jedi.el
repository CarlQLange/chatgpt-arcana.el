;;; chatgpt-jedi.el --- An Emacs package that uses the OpenAI API to write and modify code and text

;; Copyright (C) 2023 Carl Lange
;; Author: Carl Lange <carl.lange@example.com>
;; URL: https://github.com/carl-lange/chatgpt-jedi
;; Package-Requires: ((emacs "24.4"))
;; Version: 0.1

;;; Commentary:
;;
;; This package provides a way to use the OpenAI API to write and modify code and text within Emacs.
;;
;;; Code:

(require 'url)
(require 'json)
(require 'cl-lib)

(defgroup chatgpt-jedi nil
  "An Emacs package that uses the OpenAI API to write and modify code and text."
  :group 'tools)

(defcustom chatgpt-jedi-api-key nil
  "The API key for the OpenAI API."
  :type 'string
  :group 'chatgpt-jedi)

(defvar chatgpt-jedi-api-endpoint "https://api.openai.com/v1/completions")

(defcustom chatgpt-jedi-model-name "gpt-3.5-turbo"
  "The name of the OpenAI model to use. Some cost more than others. Beware."
  :type 'string
  :group 'chatgpt-jedi)

(defcustom chatgpt-jedi-system-prompts-alist
  '((programming-prompt . "You are ChatGPT, a large language model trained by OpenAI to be the perfect programmer, called from an Emacs package. Respond only with concise code unless explicitly asked. ")
    (writing-prompt . "You are ChatGPT, a large language model trained by OpenAI to be an excellent writing assistant, called from an Emacs package. Respond concisely and carry out instructions. "))
  "An alist that maps system prompt identifiers to actual system prompts."
  :type '(alist :key-type symbol :value-type string)
  :group 'chatgpt-jedi)

(defcustom chatgpt-jedi-fallback-prompt
  "You are ChatGPT, a large language model trained by OpenAI, called from an Emacs package. Be concise."
  "A fallback system prompt used when the current major mode is not found in the `chatgpt-jedi-system-prompts-alist`."
  :type 'string
  :group 'chatgpt-jedi)

(defcustom chatgpt-jedi-system-prompts-modes-alist
  '((prog-mode . programming-prompt)
    (org-mode . writing-prompt)
    (markdown-mode . writing-prompt))
  "An alist that maps major modes to system prompt identifiers."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'chatgpt-jedi)

(defun chatgpt-jedi-get-system-prompt ()
  "Returns the system prompt based on the current major mode, or the fallback prompt if the mode is not found."
  (let* ((mode-name (symbol-name major-mode))
         (prompt-identifier (cdr (assoc major-mode chatgpt-jedi-system-prompts-modes-alist)))
         (system-prompt (or (cdr (assoc prompt-identifier chatgpt-jedi-system-prompts-alist))
                            chatgpt-jedi-fallback-prompt)))
    (concat system-prompt "\nCurrent major mode: " mode-name)))

(defun chatgpt-jedi--query-api (prompt)
  "Sends a query to the OpenAI API with PROMPT and returns the first message content."
  (let* ((url-request-method "POST")
         (url-request-data (json-encode `((model . ,chatgpt-jedi-model-name)
                                          (messages . [((role . "user")
                                                        (content . ,prompt))]))))
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " chatgpt-jedi-api-key))))
         (response (url-retrieve-synchronously chatgpt-jedi-api-endpoint)))
    (with-current-buffer response
      (goto-char url-http-end-of-headers)
      (let* ((json-object-type 'hash-table)
             (json-array-type 'list)
             (json-key-type 'string))
        (let* ((response-data (json-read))
               (choices (gethash "choices" response-data))
               (msg (gethash "message" (car choices)))
               (content (gethash "content" msg)))
          (message (string-trim content)))))))

(defun chatgpt-jedi-query (prompt)
  "Sends PROMPT to the OpenAI API and returns the result."
  (interactive "sPrompt: ")
  (chatgpt-jedi--query-api (concat (chatgpt-jedi-get-system-prompt) "\n" prompt)))

(defun chatgpt-jedi-query-region (prompt)
  "Sends the selected region to the OpenAI API with PROMPT and the system prompt."
  (interactive "sPrompt: ")
  (let ((selected-region (buffer-substring (mark) (point))))
    (deactivate-mark)
    (chatgpt-jedi--query-api (concat (chatgpt-jedi-get-system-prompt) "\n" prompt "\n" selected-region))))

(defun chatgpt-jedi-replace-region (prompt)
  "Sends the selected region to the OpenAI API with PROMPT and replaces the region with the output."
  (interactive "sPrompt: ")
  (let ((selected-region (buffer-substring (mark) (point))))
    (deactivate-mark)
    (let ((modified-region (chatgpt-jedi--query-api (concat (chatgpt-jedi-get-system-prompt) "\n" prompt " " selected-region))))
      (delete-region (mark) (point))
      (insert modified-region))))

(defun chatgpt-jedi-insert-after-region (prompt)
  "Sends the selected region to the OpenAI API with PROMPT and inserts the output after the region."
  (interactive "sPrompt: ")
  (let ((selected-region (buffer-substring (mark) (point))))
    (deactivate-mark)
    (save-excursion
      (let ((inserted-text (chatgpt-jedi--query-api (concat (chatgpt-jedi-get-system-prompt) "\n" prompt " " selected-region))))
        (goto-char (point))
        (insert inserted-text)))))

(defun chatgpt-jedi-insert-before-region (prompt)
  "Sends the selected region to the OpenAI API with PROMPT and inserts the output before the region."
  (interactive "sPrompt: ")
  (let ((selected-region (buffer-substring (mark) (point))))
    (deactivate-mark)
    (save-excursion
      (let ((inserted-text (chatgpt-jedi--query-api (concat (chatgpt-jedi-get-system-prompt) "\n" prompt " " selected-region))))
        (goto-char (mark))
        (insert inserted-text)))))

(defun chatgpt-jedi-insert-at-point (prompt)
  "Sends the custom PROMPT to the OpenAI API and inserts the output at point."
  (interactive "sPrompt: ")
  (let ((inserted-text (chatgpt-jedi--query-api (concat (chatgpt-jedi-get-system-prompt) "\n" prompt))))
    (insert inserted-text)))

(defun chatgpt-jedi-insert-at-point-with-context (prompt &optional num-lines)
  "Sends NUM-LINES lines of context around point to the OpenAI API with PROMPT and inserts the output at point."
  (interactive "sPrompt: \nnNumber of lines of context (default 3): ")
  (let ((line-number (line-number-at-pos (point))))
    (when (not line-number)
      (insert (chatgpt-jedi--query-api prompt)))
    (let* ((num-lines (or num-lines 3))
           (start-line (max 1 (- line-number (floor ( / num-lines 2)))))
           (end-line (min (line-number-at-pos (point-max)) (+ line-number (floor (/ num-lines 2)))))
           (context (buffer-substring (pos-bol start-line) (pos-bol end-line)))
           (modified-context (chatgpt-jedi--query-api (message (concat (chatgpt-jedi-get-system-prompt) "\n" prompt "\nYour response will go at [XXXX] in the following:\n" (concat (substring context 0 (- (length context) 1)) "[XXXX]" (substring context (- (length context) 1))))))))
      (save-excursion
        (goto-char (pos-bol line-number))
        (insert modified-context)))))

(use-package pretty-hydra
  :config
  (pretty-hydra-define chatgpt-jedi-hydra (:color blue :quit-key "q" :title "ChatGPT Jedi")
    ("Query"
     (("Q" chatgpt-jedi-query "Query")
      ("q" chatgpt-jedi-query-region "Query with region")
      ("r" chatgpt-jedi-replace-region "Replace region"))
     "Insert"
     (("i" chatgpt-jedi-insert-at-point-with-context "At point with context")
      ("I" chatgpt-jedi-insert-at-point "At point")
      ("b" chatgpt-jedi-insert-after-region "Before region")
      ("a" chatgpt-jedi-insert-before-region "After region")))
