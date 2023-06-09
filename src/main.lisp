;;;; Learn Common Lisp
;;; TODO:
;;; - learn asdf
;;; - learn quicklisp
;;; - learn defstruct & defclass
;;; - learn macro
;;; - learn error handling
;;;   - https://gigamonkeys.com/book/beyond-exception-handling-conditions-and-restarts.html
;;; - learn type system
;;;   - https://lispcookbook.github.io/cl-cookbook/type.html
;;;   - https://github.com/coalton-lang/coalton

(require 'ansi-esc "src/ansi-esc.lisp")

;; global variable
;; https://stackoverflow.com/questions/8927741/whats-difference-between-defvar-defparameter-setf-and-setq
(defvar *global-defvar* "defvar")
(defparameter *global-defparameter* "defparameter")
(defconstant +immutable-global-var+ "immutable")

(defun main ()
  (ansi-esc:fmt (:fg :black :bg :green)
    "Let's learn Common Lisp!~%")
  (write-string "Hello, World!")
  (write-char #\Newline)
  (terpri)

  (ansi-esc:fmt (:fg :green) "< global variable >~%")
  ;; both defvar and defparameter are mutable variable
  (format t "*global-defvar* = ~a~%" *global-defvar*)
  (setf *global-defvar* "mutable")
  (format t "*global-defvar* = ~a~%" *global-defvar*)
  (format t "+immutable-global-var+ = ~a~%" +immutable-global-var+)
  (terpri)

  ;; scoped variable
  (ansi-esc:fmt (:fg :green) "< scoped variable >~%")
  (let ((x 123)
        (y 456))
    (format t "x = ~a~%" x)
    (format t "y = ~a~%" y))
  (terpri)

  (let* ((x 10)
         (y (+ x 10)))
    (format t "x = ~a~%" x)
    (format t "y = ~a~%" y))
  (terpri)

  ;; named function
  (ansi-esc:fmt (:fg :green) "< named function >~%")
  (defun concat-digit (a b)
    ;; function body
    (+ (* a (expt 10 (1- (truncate (log b))))) b))
  (format t "~a~%" (concat-digit 12 34))
  (terpri)

  ;; anonymous function (lambda)
  ;; https://stackoverflow.com/a/13213772
  (ansi-esc:fmt (:fg :green) "< anonymous function >~%")
  (let ((fn #'(lambda () (format t "This is a lambda~%"))))
    (funcall fn))
  (terpri)

  ;; optional parameter with default value
  (ansi-esc:fmt (:fg :green) "< optional parameter with default value >~%")
  (defun say-hello (&optional (name "John"))
    (format t "Hello, ~a!~%" name))

  (say-hello)
  (say-hello "동준")
  (terpri)

  ;; optional named parameter with default value
  (ansi-esc:fmt (:fg :green) "< optional named parameter with default value >~%")
  (defun say-wow (&key (name "John") (age 10))
    (format t "Wow, ~a ~a!~%" name age))

  (say-wow)
  (say-wow :name "동준" :age 24)
  (terpri)

  ;; function pointer
  (ansi-esc:fmt (:fg :green) "< function pointer >~%")
  (defun haha () (write-line "haha"))
  (defun hoho () (write-line "hoho"))
  (let ((haha-ptr (function haha))
        (hoho-ptr #'hoho))
    ;             ^^^^^^ --> same as (function hoho)
    (funcall haha-ptr)
    (funcall hoho-ptr))
  (terpri)

  ;; simple addition & subtraction
  (ansi-esc:fmt (:fg :green) "< addition & subtraction >~%")
  (format t "(+ 1 2) => ~a~%" (+ 1 2)) ;; 3
  (format t "(+ 1 2 3) => ~a~%" (+ 1 2 3)) ;; 6
  (format t "(1+ 3) => ~a~%" (1+ 3)) ;; 4
  (format t "(1- 3) => ~a~%" (1- 3)) ;; 2
  (let ((a 2)
        (b 2))
    (format t "(incf a 3) => ~a~%" (incf a 3)) ;; 5
    (format t "(decf b 3) => ~a~%" (decf b 3))) ;; -1
  (terpri)

  ;; rational
  (ansi-esc:fmt (:fg :green) "< rational >~%")
  (format t "~a~%" (/ 10 (/ 3 2))) ;; 20/3
  (format t "~a~%" (/ 10 (/ 2 3))) ;; 15
  (terpri)

  ;; format string
  (ansi-esc:fmt (:fg :green) "< format string >~%")
  (format t "1 + 2 = ~a~%" (+ 1 2))
  ;                  ^^^^
  ;                  │ └-> newline
  ;                  └---> next argument

  (format t "~:(~a, ~a!~)~%" "hello" "world")
  ;          ^^^^^^^^^^^^ --> anything between ~:( and ~) gets converted to title case

  ;; number formatting
  (format t "~,2f~%" 0.3333) ;; 0.33
  (format t "~,3f~%" 0.3333) ;; 0.333
  (format t "~r~%" 12) ;; twelve
  (format t "~:r~%" 12) ;; twelfth
  (format t "~@r~%" 12) ;; XII
  (terpri)

  ;; read line
  (ansi-esc:fmt (:fg :green) "< read line >~%")
  (format t "What's your name? ")
  (finish-output) ;; <-- https://stackoverflow.com/a/40985570
  (handler-case ;; <-- https://lispcookbook.github.io/cl-cookbook/error_handling.html
    (let ((input (read-line)))
      (format t "Hello, ~a~%" input)
      (terpri))
    (sb-sys:interactive-interrupt () ;; <-- ctrl-c
                                  (exit)))

  ;; read char
  (ansi-esc:fmt (:fg :green) "< read char >~%")
  (format t "char: ~a~%" (read-char))
  (terpri)

  ;; if else if ...
  (cond ((y-or-n-p "stop? (1)") (write-line "stopped at 1"))
        ((y-or-n-p "stop? (2)") (write-line "stopped at 2"))
        ((y-or-n-p "stop? (3)") (write-line "stopped at 3")))
  (terpri)

  ;; case
  ;; http://www.lispworks.com/documentation/HyperSpec/Body/m_case_.htm

  ;; cons cell
  (ansi-esc:fmt (:fg :green) "< cons cell >~%")
  (let ((a '(1 . 2)))
    (format t "a = ~a~%" a)
    (format t "(first a) => ~a~%" (first a))
    (format t "(rest a) => ~a~%" (rest a)))
  (terpri)

  ;; list
  ;; '(a b c d) ; <-- quoted list does not evaluate items
  ;; (list a b c d) ; <-- list evaluate items
  (ansi-esc:fmt (:fg :green) "< list >~%")
  (if (equal '(1 . (2 . (3 . (4)))) '(1 2 3 4))
      (format t "'(1 . (2 . (3 . (4)))) == '(1 2 3 4)~%"))
  (let ((a '(1 2 3 4)))
    (format t "a = ~a~%" a)
    (format t "(first a) => ~a~%" (first a))
    (format t "(rest a) => ~a~%" (rest a)))
  (terpri)

  ;; list index
  (format t "index 2 of '(a b c d) => ~a~%" (nth 2 '(a b c d)))
  ;                                          ^^^^^ --> list index is zero based
  (terpri)

  ;; add item to list
  (let ((a '(1 2)))
    (push 0 a)
    (setf a (append a '(3 4)))
    (nconc a '(5 6))
    (format t "~a~%" a))
  (terpri)

  ;; property list (plist)
  (ansi-esc:fmt (:fg :green) "< property list >~%")
  (let ((person (list :name "Kate"
                      :age 21)))
    (format t "~a ~a~%" (getf person :name) (getf person :age)))
  (terpri)

  ;; vector
  ;; https://gigamonkeys.com/book/collections.html
  ;; https://lispcookbook.github.io/cl-cookbook/arrays.html
  (ansi-esc:fmt (:fg :green) "< vector >~%")
  (let ((vec1 (make-array 5 :initial-element 0))
        (vec2 #(1 2 3)))
    (format t "vec1 length: ~a~%" (length vec1))
    (format t "vec1 = ~a~%" vec1)
    (setf (elt vec1 0) 100)
    (setf (elt vec1 1) 200)
    (format t "vec1 = ~a~%" vec1)
    (format t "vec2 = ~a~%" vec2))
  (terpri)

  (let ((vec (make-array 5 :initial-contents #(1 2 3 4 5))))
    (rotatef (elt vec 0) (elt vec 1) (elt vec 2))
    (format t "~a~%" vec))
  (terpri)

  (ansi-esc:fmt (:fg :green) "< multidimentional vector >~%")
  (let ((grid (make-array '(5 5))))
    (setf grid #2A((1 2 3 4 5)
                   (1 2 3 4 5)
                   (1 2 3 4 5)
                   (1 2 3 4 5)
                   (1 2 3 4 5)))
    (format t "grid = ~a~%" grid)
    (format t "grid[2][3] = ~a~%" (aref grid 2 3)))
  ;                                ^^^^^^^^^^^^^ --> row major indexing
  (terpri)

  ;; dotimes macro
  (ansi-esc:fmt (:fg :green) "< dotimes macro >~%")
  (dotimes (i 10)
    (format t "~a " i))
  (terpri)
  (let ((res (dotimes (i 10 "return value")
               (format t "~a " i))))
    (terpri)
    (format t "res = ~a~%" res))
  (terpri)

  ;; dolist macro
  (ansi-esc:fmt (:fg :green) "< dolist macro >~%")
  (dolist (item '("hi" "yo" "cool"))
    (format t "~a~%" item))
  (terpri)

  ;; do macro
  (ansi-esc:fmt (:fg :green) "< do macro >~%")
  (do ((i 0 (1+ i))
       (j 0 (+ j 2)))
      ((= i 3))
    (format t "i = ~a, j = ~a~%" i j))
  (terpri)

  ;; loop macro
  ;; https://cl-cookbook.sourceforge.net/loop.html
  ;; https://lispcookbook.github.io/cl-cookbook/iteration.html
  (ansi-esc:fmt (:fg :green) "< loop macro >~%")
  (defun print-repeat (n str)
    (loop :repeat n :do (format t "~a~%" str)))

  (print-repeat 3 "wow")
  (terpri)

  (loop :for i :from 1 :to 10
        :do (format t "~a " i))
  (finish-output)
  (dotimes (_ 2) (terpri))

  (loop :for i :from 10 :downto 1
        :do (format t "~a " i))
  (finish-output)
  (dotimes (_ 2) (terpri))

  (loop :for a :in '(10 20 30)
        :for b :in '(100 200 300 400)
        :do (format t "~a ~a~%" a b))
  (terpri)

  (loop :for item :across #(1 2 3 4)
        :do (format t "~a " item))
  (finish-output)
  (dotimes (_ 2) (terpri))

  (loop :with a = 1
        :with b = 10
        :while (< a b)
        :do (setf a (* a 2))
        :do (setf b (+ b 1))
        :finally (format t "a = ~a, b = ~a~%" a b))
  (terpri)

  (exit))
