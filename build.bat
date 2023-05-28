@echo off

sbcl --non-interactive --load src/main.lisp --eval "(sb-ext:save-lisp-and-die \"app.exe\" :toplevel 'main :executable t)"
