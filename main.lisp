;;;; learn common lisp

#|
comment style rules
https://stackoverflow.com/a/6365579
https://www.gnu.org/software/emacs/manual/html_node/elisp/Comment-Tips.html
|#

;;; global variable
;; https://stackoverflow.com/questions/8927741/whats-difference-between-defvar-defparameter-setf-and-setq
(defparameter *some-global-var* "global var")
(defconstant immutable-global-var "immutable")

(defun main ()
  (write-string "This is lisp!")
  (write-string " Hi!")
  (write-char #\Newline *standard-output*)
  (terpri)
  (fresh-line)

  ;;; global variable
  (write-string "# global string")
  (terpri)
  (format t "~a~%" *some-global-var*)
  (setf *some-global-var* "mutable")
  (format t "~a~%" *some-global-var*)
  (format t "~a~%" immutable-global-var)
  (terpri)

  ;;; scoped variable
  (write-string "# scoped variable")
  (terpri)
  (let ((x 123)
        (y 456))
    (format t "~a~%" x)
    (format t "~a~%" y))
  (terpri)

  ;;; named function
  (write-string "# named function")
  (terpri)
  (defun add (a b)
    ;; function body
    (+ a b))

  (format t "~a~%" (+ 1 2)) ; 3
  (format t "~a~%" (+ 1 2 3)) ; 6
  (format t "~a~%" (1+ 3)) ; 4
  (format t "~a~%" (1- 3)) ; 2
  (terpri)

  ;;; format string
  (write-string "# format string")
  (terpri)
  (format t "1 + 2 = ~a~%" (add 1 2))
  ;                  ^^^^
  ;                  │ └-> newline
  ;                  └---> next argument

  (format t "~:(~a, ~a!~)~%" "hello" "world")
  ;          ^^^^^^^^^^^^ --> anything between ~:( and ~) gets converted to title case

  ;;; number formatting
  (format t "~r~%" 12) ; twelve
  (format t "~:r~%" 12) ; twelfth
  (format t "~@r~%" 12) ; XII
  (terpri)

  ;;; read line
  (write-string "# read line from stdin")
  (terpri)
  (format t "What's your name? ")
  (finish-output) ; <-- https://stackoverflow.com/a/40985570
  (handler-case ; <-- https://lispcookbook.github.io/cl-cookbook/error_handling.html
    (let ((input (read-line)))
      (format t "Hello, ~a~%" input)
      (terpri))
    (sb-sys:interactive-interrupt () ; <-- ctrl-c
                                  (exit)))

  ;;; yes or no prompt
  (if (y-or-n-p "Do you like lisp?")
        (format t "yay!~%")
        (format t "sad...~%"))
      (terpri)

      ;;; optional parameter with default value
      (write-string "# optional parameter with default value")
      (terpri)
      (defun say-hello (&optional (name "John"))
        (format t "Hello, ~a!~%" name))

      (say-hello)
      (say-hello "동준")

      ;;; optional named parameter with default value
      (write-string "# optional named parameter with default value")
      (terpri)
      (defun say-wow (&key (name "John"))
        (format t "Wow, ~a!~%" name))

      (say-wow)
      (say-wow :name "동준")
      (terpri)

      ;;; cons cell
      (write-string "# cons cell")
      (terpri)
      (let ((a '(1 . 2)))
        (format t "a => ~a~%" a)
        (format t "(first a) => ~a~%" (first a))
        (format t "(rest a) => ~a~%" (rest a)))
      (terpri)

      ;;; list
      (write-string "# list")
      (terpri)
      (if (equal '(1 . (2 . (3 . (4)))) '(1 2 3 4))
          (format t "it's same~%"))
      (let ((a '(1 2 3 4)))
        (format t "a => ~a~%" a)
        (format t "(first a) => ~a~%" (first a))
        (format t "(rest a) => ~a~%" (rest a)))
      (terpri)

      ;;; list index
      (format t "index 2 = ~a~%" (nth 2 '(a b c d)))
      ;                           ^^^^^ --> list index is zero based
      (terpri)

      ;;; add item to list
      (let ((a '(1 2)))
        (push 0 a)
        (setq a (append a '(3 4)))
        (nconc a '(5))
        (format t "~a~%" a))
      (terpri)

      ;;; property list (plist)
      (write-string "# property list")
      (terpri)
      (let ((person '(:name "Kate" :age 21)))
        (format t "~a ~a~%" (getf person :name) (getf person :age)))
      (terpri)

      ;;; dotimes macro
      (write-string "# dotimes macro")
      (terpri)
      (dotimes (i 10)
        (format t "~a " i))
      (terpri)
      (let ((res (dotimes (i 10 "return value") (format t "~a " i))))
        (format t "~%~a~%" res))
      (terpri)

      ;;; dolist macro
      (write-string "# dolist macro")
      (terpri)
      (dolist (item '("hi" "yo" "cool"))
        (format t "~a~%" item))
      (terpri)

      ;;; do macro
      (write-string "# do macro")
      (terpri)
      (do ((i 0 (1+ i))
           (j 0 (+ j 2)))
          ((= i 3))
        (format t "i = ~a, j = ~a~%" i j))
      (terpri)

      ;;; loop macro
      ;; https://cl-cookbook.sourceforge.net/loop.html
      (write-string "# loop macro")
      (terpri)
      (defun print-n (n str)
        (loop :repeat n
              :do (format t "~a~%" str)))

      (print-n 3 "wow")
      (terpri)

      (loop for a in '(10 20 30)
            for b in '(100 200 300)
            do (format t "~a ~a~%" a b))

      (exit))
