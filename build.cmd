@echo off
sbcl --non-interactive --load src/project.lisp --eval "(build)"
