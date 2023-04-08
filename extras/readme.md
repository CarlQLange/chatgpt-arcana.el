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


## ChatGPT Arcana Eshell

Sick of staring into the abyss of your terminal, trying to remember how to invoke `tar`? Don't despair - with ChatGPT-arcana, you can generate complex shell commands from natural language descriptions. Just type `spell tar this directory` and ChatGPT will insert the command to do what you want on the next line. All you need to do is press enter.

```
~/Desktop $ ls
2023-03-31 12-44-10.mkv
~/Desktop $ spell convert this mkv into mp4
~/Desktop $ ffmpeg -i 2023-03-31\ 12-44-10.mkv 2023-03-31\ 12-44-10.mp4
```

But like they tell us all in wizard school: with great power comes great responsibility. You should still know what the command is doing - it absolutely 100% will with no qualms tell you how to destroy the universe (`rm -rf /`).

Also, this command by default sends an `ls` of the current directory and the first 500 bytes of any named files to the ChatGPT API.

## ChatGPT Arcana Autofix

Automatically fix errors at point, flagged by `flycheck` or `flymake`, with `chatgpt-arcana-autofix`.

## ChatGPT Arcana Commitmsg

Automatically write commit messages. Who needs to do that manually anyways.

`chatgpt-arcana-commitmsg` will insert the commit message at point, so use it in a magit `COMMIT_EDITMSG` buffer.

This will send the current diff against HEAD and the last few commit messages to ChatGPT.
