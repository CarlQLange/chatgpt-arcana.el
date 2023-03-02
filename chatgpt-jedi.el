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

(defvar chatgpt-jedi-api-endpoint "https://api.openai.com/v1/chat/completions")

(defcustom chatgpt-jedi-model-name "gpt-3.5-turbo"
  "The name of the OpenAI model to use."
  :type 'string
  :group 'chatgpt-jedi)

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

(defvar chatgpt-jedi-last-query-result nil
  "The result of the last query to the OpenAI API.")

(defun chatgpt-jedi-query (prompt)
  "Sends PROMPT to the OpenAI API and returns the result."
  (interactive "sPrompt: ")
  (chatgpt-jedi--query-api prompt))

(defun chatgpt-jedi-query-region (prompt)
  "Sends the selected region to the OpenAI API with PROMPT."
  (interactive "sPrompt: ")
  (let ((selected-region (buffer-substring (mark) (point))))
    (deactivate-mark)
    (chatgpt-jedi--query-api (concat prompt " " selected-region))))

(defun chatgpt-jedi-replace-region (prompt)
  "Sends the selected region to the OpenAI API with PROMPT and replaces the region with the output."
  (interactive "sPrompt: ")
  (let ((selected-region (buffer-substring (mark) (point))))
    (deactivate-mark)
    (let ((modified-region (chatgpt-jedi--query-api (concat prompt " " selected-region))))
      (delete-region (mark) (point))
      (insert modified-region))))

(defun chatgpt-jedi-insert-after-region (prompt)
  "Sends the selected region to the OpenAI API with PROMPT and inserts the output after the region."
  (interactive "sPrompt: ")
  (let ((selected-region (buffer-substring (mark) (point))))
    (deactivate-mark)
    (save-excursion
      (let ((inserted-text (chatgpt-jedi--query-api (concat prompt " " selected-region))))
        (goto-char (point))
        (insert inserted-text)))))

(defun chatgpt-jedi-insert-before-region (prompt)
  "Sends the selected region to the OpenAI API with PROMPT and inserts the output before the region."
  (interactive "sPrompt: ")
  (let ((selected-region (buffer-substring (mark) (point))))
    (deactivate-mark)
    (save-excursion
      (let ((inserted-text (chatgpt-jedi--query-api (concat prompt " " selected-region))))
        (goto-char (mark))
        (insert inserted-text)))))

(defun chatgpt-jedi-insert-at-point (prompt)
  "Sends the custom PROMPT to the OpenAI API and inserts the output at point."
  (interactive "sPrompt: ")
  (let ((inserted-text (chatgpt-jedi--query-api prompt)))
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
           (modified-context (chatgpt-jedi--query-api (message (concat prompt "\nYour response will go at [XXXX] in the following:\n" (concat (substring context 0 (- (length context) 1)) "[XXXX]" (substring context (- (length context) 1))))))))
      (save-excursion
        (goto-char (pos-bol line-number))
        (insert modified-context)))))

(use-package hydra
  :config
  (defhydra chatgpt-jedi-hydra (:color blue :hint nil)
    "
_q_: Query region   _r_: Insert result   _c_: Clear result
"
    ("i" chatgpt-jedi-insert-at-point-with-context)
    ("I" chatgpt-jedi-insert-at-point)
    ("q" chatgpt-jedi-query-region)
    ("m" chatgpt-jedi-replace-region)
    ("b" chatgpt-jedi-insert-before-region)
    ("a" chatgpt-jedi-insert-after-region)
    ("r" (insert chatgpt-jedi-last-query-result))
    ("c" (setq chatgpt-jedi-last-query-result nil))))
