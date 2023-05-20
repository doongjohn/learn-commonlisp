sbcl --non-interactive --load main.cl --eval "(sb-ext:save-lisp-and-die \"app.exe\" :toplevel 'main :executable t)"
