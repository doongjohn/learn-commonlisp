@echo off
sbcl --noinform --non-interactive --load src/main.lisp --eval "(main)"
