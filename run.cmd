@echo off
sbcl --noinform --non-interactive --load app.lisp --eval "(main:main)"
