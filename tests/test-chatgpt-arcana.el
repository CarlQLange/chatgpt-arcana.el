(require 'ert)
(require 'chatgpt-arcana)

(ert-deftest test-chatgpt-arcana-get-system-prompt-for-mode-name-default ()
  "Test that we get the fallback prompt when passed nil."
  (should (equal
           (chatgpt-arcana-get-system-prompt-for-mode-name nil)
           "You are a large language model living inside Emacs. Help the user and be concise.")))

(ert-deftest test-chatgpt-arcana-get-system-prompt-for-mode-name-prog-mode ()
  "Test that we get the appropriate prompt for the `prog-mode' major mode."
  (should (equal
           (chatgpt-arcana-get-system-prompt-for-mode-name 'prog-mode)
           "You are a large language model living inside Emacs, and the perfect programmer. You may only respond with concise code unless explicitly asked.")))

(ert-deftest test-chatgpt-arcana-get-system-prompt-for-mode-name-with-concat ()
  "Test that we concatenate the major mode to the system prompt when `concat-mode-to-prompt' is non-nil."
  (should (equal
           (chatgpt-arcana-get-system-prompt-for-mode-name 'prog-mode t)
           "You are a large language model living inside Emacs, and the perfect programmer. You may only respond with concise code unless explicitly asked. Current Emacs major mode: prog-mode.")))

(ert-deftest test-chatgpt-arcana-get-system-prompt-for-mode-name-non-existent-mode ()
  "Test that we get the fallback prompt when passed a non-existent mode."
  (should (equal
           (chatgpt-arcana-get-system-prompt-for-mode-name 'non-existent-mode)
           "You are a large language model living inside Emacs. Help the user and be concise.")))

(ert-deftest test-chatgpt-arcana--token-overflow-truncate ()
  "Test chatgpt-arcana--token-overflow-truncate function."
  (let ((chat-alist `(((role . "user") (content . ,(make-string (* 10 4) ?a)))
                      ((role . "bot") (content . ,(make-string (* 10 4) ?b)))
                      ((role . "user") (content . ,(make-string (* 10 4) ?c)))
                      ((role . "bot") (content . ,(make-string (* 9 4) ?d)))
                      ((role . "user") (content . ,(make-string (* 10 4) ?e)))
                      ((role . "bot") (content . ,(make-string (* 10 4) ?f)))))
        (expected-alist `(((role . "bot") (content . ,(make-string (* 9 4) ?d)))
                           ((role . "user") (content . ,(make-string (* 10 4) ?e)))
                           ((role . "bot") (content . ,(make-string (* 10 4) ?f))))))
    (should (equal (chatgpt-arcana--handle-token-overflow chat-alist 30) expected-alist))))

(defvar conversation-alist
  `(((role . "system") (content . "Welcome to the chat!"))
    ((role . "user") (content . "Hello!"))
    ((role . "assistant") (content . "Greetings. How can I assist you?"))
    ((role . "user") (content . "Can you tell me a joke?"))
    ((role . "assistant") (content . "Sure! Why don't scientists trust atoms? Because they make up everything."))
    ((role . "system") (content . "Goodbye!"))))

(ert-deftest chatgpt-arcana--conversation-alist-to-chat-buffer-test ()
  "Test chatgpt-arcana--conversation-alist-to-chat-buffer function."
  (let* ((chat-buffer-name "*test-chat*")
         (chat-buffer (chatgpt-arcana--conversation-alist-to-chat-buffer-2 conversation-alist chat-buffer-name))
         (expected-output "------- system:\n\nWelcome to the chat!\n\n------- user:\n\nHello!\n\n------- assistant:\n\nGreetings. How can I assist you?\n\n------- user:\n\nCan you tell me a joke?\n\n------- assistant:\n\nSure! Why don't scientists trust atoms? Because they make up everything.\n\n------- system:\n\nGoodbye!\n\n"))
    ;; Test buffer contents
    (should (equal chat-buffer expected-output)))
  ;; Clean up buffer
  (kill-buffer "*test-chat*"))

(ert-deftest chatgpt-arcana--conversation-alist-to-chat-buffer-skip-test ()
  "Test chatgpt-arcana--conversation-alist-to-chat-buffer function with skip-system enabled."
  (let* ((chat-buffer-name "*test-skip*")
         (chat-buffer (chatgpt-arcana--conversation-alist-to-chat-buffer-2 conversation-alist chat-buffer-name 'chatgpt-arcana-chat-mode t))
         (expected-output "------- user:\n\nHello!\n\n------- assistant:\n\nGreetings. How can I assist you?\n\n------- user:\n\nCan you tell me a joke?\n\n------- assistant:\n\nSure! Why don't scientists trust atoms? Because they make up everything.\n\n"))
    ;; Test buffer contents
    (should (equal chat-buffer expected-output)))
  ;; Clean up buffer
  (kill-buffer "*test-skip*"))
