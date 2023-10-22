@echo off
sbcl --noinform --non-interactive --load src/app.lisp --eval "(main:main)"
