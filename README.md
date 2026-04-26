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
  - `C-c C-z` start nREPL server / show REPL buffer
  - Completion via `completion-at-point` (works with corfu, company, default)
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
