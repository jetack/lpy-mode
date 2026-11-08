# lpy-mode

An Emacs major mode for [LisPython](https://github.com/jetack/lispython).

## Features

- Syntax highlighting (special forms, builtins, macros, operators)
- Indentation (Lisp-aware, special form vs function call)
- Paredit / Smartparens support
- **nREPL client** — connect to `lpy --nrepl` for REPL-driven development:
  - `C-c C-e` eval last sexp (inline overlay + minibuffer)
  - `C-M-x` eval current form
  - `C-c C-r` eval region
  - `C-c C-b` eval buffer contents
  - `C-c C-k` load file into REPL
  - `C-c C-m` macroexpand form
  - `C-c M-n` choose the file scope for string evals (empty: `__main__`)
  - `C-c M-:` eval a string from the minibuffer in that scope
  - `C-c C-z` start nREPL server / show REPL buffer
  - `M-x lpy-nrepl-connect-project` connect via the project's `.nrepl-port` (also the default port for `lpy-nrepl-connect`)
  - Completion via `completion-at-point` (works with corfu, company, default), annotated with the server's `labelDetails.description` when it sends one (`special form`, `macro`), otherwise the candidate's kind as a lowercase word (function/method/class/module/variable/keyword/...), like LSP; when eglot manages the buffer, its LSP candidates are merged in (one list, duplicates shown once with eglot's docs), so no `cape-capf-super` is needed, and candidates follow the server per keystroke (an incomplete LSP answer is re-asked as you type). LSP candidates are annotated the same way; their signature goes to the doc popup (e.g. `corfu-popupinfo`) and the echo-area docsig, not the list
  - Eldoc integration (signature display in minibuffer)
- Comint shell fallback when nREPL is not connected

## Installation

Install via [straight.el](https://github.com/radian-software/straight.el):

```elisp
(use-package lpy-mode
  :straight (lpy-mode :type git :host github :repo "jetack/lpy-mode"))
```

## Usage

Open a `.lpy` file and press `C-c C-z` to start the nREPL server. Then use `C-c C-e` to evaluate expressions inline.

The nREPL server requires `lispython` to be installed:

```bash
pip install lispython
```

## Requirements

- Emacs 27+ (for `json-serialize` / `json-parse-string`)
- [lispython](https://pypi.org/project/lispython/) (`lpy --nrepl` server)
- [dash.el](https://github.com/magnars/dash.el)
- [s.el](https://github.com/magnars/s.el)
