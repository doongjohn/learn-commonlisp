@echo off
sbcl --non-interactive --load app.lisp --eval "(sb-ext:save-lisp-and-die \"app.exe\" :toplevel 'main:main :executable t)"
