;;;; learn common lisp

;;; TODO:
;; - learn error handling
;;   https://gigamonkeys.com/book/beyond-exception-handling-conditions-and-restarts.html
;; - learn defclass
;; - learn type system
;;   https://lispcookbook.github.io/cl-cookbook/type.html
;; - learn macro

;;; tutorials
;; https://www.youtube.com/watch?v=LqBbGFMPcDI
;; https://cs.gmu.edu/~sean/lisp/LispTutorial.html
;; https://github.com/rabbibotton/clog/blob/main/LEARN.md
;; https://gigamonkeys.com/book/

;;; comment style rules
;; https://stackoverflow.com/a/6365579
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Comment-Tips.html

;;; libraries
;; - Regular expression: https://github.com/telekons/one-more-re-nightmare
;; - Run external program: https://github.com/ruricolist/cmd
;; - HTTP client: https://github.com/fukamachi/dexador

;;; global variable
;; https://stackoverflow.com/questions/8927741/whats-difference-between-defvar-defparameter-setf-and-setq
(defparameter *some-global-var* "global var")
(defconstant immutable-global-var "immutable")

(defun main ()
  (write-string "This is common-lisp!")
  (write-string " Hello!")
  (write-char #\Newline *standard-output*)
  (terpri)
  (fresh-line)

  (write-line "< global string >")
  (format t "~a~%" *some-global-var*)
  (setf *some-global-var* "mutable")
  (format t "~a~%" *some-global-var*)
  (format t "~a~%" immutable-global-var)
  (terpri)

  ;;; scoped variable
  (write-line "< scoped variable >")
  (let ((x 123)
        (y 456))
    (format t "~a~%" x)
    (format t "~a~%" y))
  (terpri)

  (let* ((x 10)
         (y (+ x 10)))
    (format t "~a~%" x)
    (format t "~a~%" y))
  (terpri)

  ;;; named function
  (write-line "< named function >")
  (defun add (a b)
    ;; function body
    (+ a b))
  (format t "~a~%" (add 10 20))
  (terpri)

  ;;; anonymous function (lambda)
  ;; https://stackoverflow.com/a/13213772
  (write-line "< anonymous function >")
  (let ((fn #'(lambda () (format t "This is a lambda~%"))))
    (funcall fn))
  (terpri)

  ;;; simple addition & subtraction
  (write-line "< addition & subtraction >")
  (format t "~a~%" (+ 1 2)) ;; 3
  (format t "~a~%" (+ 1 2 3)) ;; 6
  (format t "~a~%" (1+ 3)) ;; 4
  (format t "~a~%" (1- 3)) ;; 2
  (terpri)

  ;;; rational
  (write-line "< rational >")
  (format t "~a~%" (/ 10 (/ 3 2))) ;; 20/3
  (format t "~a~%" (/ 10 (/ 2 3))) ;; 15
  (terpri)

  ;;; format string
  (write-line "< format string >")
  (format t "1 + 2 = ~a~%" (add 1 2))
  ;                  ^^^^
  ;                  │ └-> newline
  ;                  └---> next argument

  (format t "~:(~a, ~a!~)~%" "hello" "world")
  ;          ^^^^^^^^^^^^ --> anything between ~:( and ~) gets converted to title case

  ;;; number formatting
  (format t "~,2f~%" 0.3333) ;; 0.33
  (format t "~,3f~%" 0.3333) ;; 0.333
  (format t "~r~%" 12) ;; twelve
  (format t "~:r~%" 12) ;; twelfth
  (format t "~@r~%" 12) ;; XII
  (terpri)

  ;;; read line
  (write-line "< read line >")
  (format t "What's your name? ")
  (finish-output) ;; <-- https://stackoverflow.com/a/40985570
  (handler-case ;; <-- https://lispcookbook.github.io/cl-cookbook/error_handling.html
    (let ((input (read-line)))
      (format t "Hello, ~a~%" input)
      (terpri))
    (sb-sys:interactive-interrupt () ;; <-- ctrl-c
                                  (exit)))

  ;;; read char
  (write-line "< read char >")
  (format t "char: ~a~%" (read-char))
  (terpri)

  ;;; yes or no prompt
  (if (y-or-n-p "Do you like lisp?")
    (format t "yay!~%")
    (format t "sad...~%"))
  (terpri)

  ;;; optional parameter with default value
  (write-line "< optional parameter with default value >")
  (defun say-hello (&optional (name "John"))
    (format t "Hello, ~a!~%" name))

  (say-hello)
  (say-hello "동준")

  ;;; optional named parameter with default value
  (write-line "< optional named parameter with default value >")
  (defun say-wow (&key (name "John") (age 10))
    (format t "Wow, ~a ~a!~%" name age))

  (say-wow)
  (say-wow :name "동준" :age 24)
  (terpri)

  ;;; cons cell
  (write-line "< cons cell >")
  (let ((a '(1 . 2)))
    (format t "a => ~a~%" a)
    (format t "(first a) => ~a~%" (first a))
    (format t "(rest a) => ~a~%" (rest a)))
  (terpri)

  ;;; list
  ;; '(a b c d) ; <-- quoted list does not evaluate items
  ;; (list a b c d) ; <-- list evaluates items
  (write-line "< list >")
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
    (setf a (append a '(3 4)))
    (nconc a '(5 6))
    (format t "~a~%" a))
  (terpri)

  ;;; property list (plist)
  (write-line "< property list >")
  (let ((person '(:name "Kate" :age 21)))
    (format t "~a ~a~%" (getf person :name) (getf person :age)))
  (terpri)

  ;;; vector
  ;; https://gigamonkeys.com/book/collections.html
  ;; https://lispcookbook.github.io/cl-cookbook/arrays.html
  (write-line "< vector >")
  (let ((arr1 (make-array 5 :initial-element 0))
        (arr2 #(1 2 3)))
    (format t "arr1 length: ~a~%" (length arr1))
    (format t "arr1 = ~a~%" arr1)
    (setf (elt arr1 0) 100)
    (setf (elt arr1 1) 200)
    (format t "arr1 = ~a~%" arr1)
    (format t "arr2 = ~a~%" arr2))
  (terpri)

  ;;; dotimes macro
  (write-line "< dotimes macro >")
  (dotimes (i 10)
    (format t "~a " i))
  (terpri)
  (let ((res (dotimes (i 10 "return value") (format t "~a " i))))
    (terpri)
    (format t "res = ~a~%" res))
  (terpri)

  ;;; dolist macro
  (write-line "< dolist macro >")
  (dolist (item '("hi" "yo" "cool"))
    (format t "~a~%" item))
  (terpri)

  ;;; do macro
  (write-line "< do macro >")
  (do ((i 0 (1+ i))
       (j 0 (+ j 2)))
      ((= i 3))
    (format t "i = ~a, j = ~a~%" i j))
  (terpri)

  ;;; loop macro
  ;; https://cl-cookbook.sourceforge.net/loop.html
  ;; https://lispcookbook.github.io/cl-cookbook/iteration.html
  (write-line "< loop macro >")
  (defun print-ntimes (n str)
    (loop :repeat n
          :do (format t "~a~%" str)))

  (print-ntimes 3 "wow")
  (terpri)

  (loop :for i :from 10 :downto 1
        :do (format t "~a " i))
  (finish-output)
  (terpri)
  (terpri)

  (loop :for a :in '(10 20 30)
        :for b :in '(100 200 300 400)
        :do (format t "~a ~a~%" a b))
  (terpri)

  (loop :with a = 1
        :with b = 10
        :while (< a b)
        :do (setf a (* a 2))
        :do (setf b (+ b 1))
        :finally (format t "a = ~a, b = ~a~%" a b))
  (terpri)

  (exit))
