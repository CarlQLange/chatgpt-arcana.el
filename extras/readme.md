# ChatGPT Arcana Extras

These are some extras that are not part of the main package and I disavow any knowledge of them.

## ChatGPT Arcana ReAct

This is a work in progress inplementation of the [ReAct framework](https://react-lm.github.io/), courtesy [LangChain](https://github.com/hwchase17/langchain) and [Simon Willison's blog](https://til.simonwillison.net/llms/python-react-pattern).
It defines a macro, `defaction`, which allows ChatGPT to run code. This is a deeply powerful and dangerous magic and may genuinely result in the destruction of all you hold dear (eg, `rm -rf /`). **BE EXTREMELY CAREFUL** with what you do and ask of ChatGPT in this mode because it is, **by default**, able to just `eval` arbitrary elisp.

Define your actions with code like the following:

```elisp
(defaction capitalise #'(lambda (s) (upcase s)) "eg: capitalise { str } => STR")
```

The first argument is the action name, second the function to be run, and third the docstring, which will be passed to ChatGPT (if it asks for it).

Call it with `query-loop` (good name TBD). None of the definitions in this file are namespaced.

Here's an example of a query loop in action.

```
------- user:

Close other windows, split the window, and open the *chatgpt-arcana-react* buffer in the split.

------- assistant:

Thought: To accomplish this, I'll need to use some Emacs Lisp functions for window and buffer handling.
Action: eval {
(progn
  ;; Close all other windows except the current one.
  (delete-other-windows)
  ;; Split the current window vertically.
  (split-window-right)
  ;; Switch to the new window.
  (other-window 1)
  ;; Open the *chatgpt-arcana-react* buffer.
  (switch-to-buffer "*chatgpt-arcana-react*"))
}
PAUSE

------- user:

Observation: #<buffer *chatgpt-arcana-react*>

------- assistant:

Answer: The *chatgpt-arcana-react* buffer has been successfully opened in a split window.
```

And another example:

```
------- user:

Task: Reverse the user's name

------- assistant:

Thought: I need to get the user's name and then reverse it.
Action: get-user-input { Please enter your name: }
PAUSE

------- user:

Observation: Carl Lange

------- assistant:

Thought: I will use Emacs lisp to reverse the string.
Action: eval { (apply 'string (reverse (string-to-list "Carl Lange"))) }
PAUSE

------- user:

Observation: "egnaL lraC"

------- assistant:

Answer: The reverse of "Carl Lange" is "egnaL lraC".
```
