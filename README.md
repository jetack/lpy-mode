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
  - `C-c C-c` interrupt the running eval (`C-u C-c C-c`: every queued one too); the mode line shows `eval…` while one runs
  - Output streams into the REPL buffer while an eval runs, so an endless printing loop shows its output and can be stopped
  - `C-c C-z` start nREPL server / show REPL buffer; the server starts in the background and a dead server is reported in the REPL buffer
  - `M-x lpy-nrepl-restart` kill the Emacs-started server and start a fresh one (also the way out of a hung eval on a server that cannot interrupt)
  - `M-x lpy-nrepl-connect-project` connect via the project's `.nrepl-port` (also the default port for `lpy-nrepl-connect`)
  - Completion via `completion-at-point` (works with corfu, company, default), annotated with the server's `labelDetails.description` when it sends one (`special form`, `macro`), otherwise the candidate's kind as a lowercase word (function/method/class/module/variable/keyword/...), like LSP; when eglot manages the buffer, its LSP candidates are merged in (one list, duplicates shown once with eglot's docs), so no `cape-capf-super` is needed. Candidates are re-asked from both the LSP and the nREPL on every keystroke, and the nREPL list is matched on the server's `filterText`, so `my_v` finds `my-var`; if you set `completion-category-defaults` to nil (a common orderless setup), add `(lpy-nrepl (styles lpy-nrepl-passthrough))` to `completion-category-overrides`, as for eglot's `eglot-capf`. LSP candidates are annotated the same way; their signature goes to the doc popup (e.g. `corfu-popupinfo`) and the echo-area docsig, not the list
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

Output streaming and `C-c C-c` interrupt need lispython with nREPL streaming and interrupt support. With an older server, output appears when the eval ends, and `M-x lpy-nrepl-restart` stops a runaway eval. Completion never waits on a server that cannot answer during an eval; otherwise it waits at most `lpy-nrepl-request-timeout` once per completion. The REPL buffer keeps at most `lpy-nrepl-log-max-size` characters (oldest lines dropped first).

## Requirements

- Emacs 30.1+ (eglot completion bounds via `bounds-of-thing-at-point-provider-alist`, and the Unicode 15.1 tables the word-boundary spec needs)
- [lispython](https://pypi.org/project/lispython/) (`lpy --nrepl` server)
- [dash.el](https://github.com/magnars/dash.el)
- [s.el](https://github.com/magnars/s.el)
