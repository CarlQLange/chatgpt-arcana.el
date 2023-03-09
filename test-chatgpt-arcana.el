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
