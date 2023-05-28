@echo off

sbcl --noinform --load src/main.lisp --eval "(main)"
