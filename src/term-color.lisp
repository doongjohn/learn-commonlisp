(defpackage #:term-color
  (:use
   #:cl)
  (:export
   #:fg-set   ; function
   #:fg-reset ; function
   #:fmt-fg   ; macro
  ))

(in-package #:term-color)

;; https://en.wikipedia.org/wiki/ANSI_escape_code
;; https://gist.github.com/JBlond/2fea43a3049b38287e5e9cefc87b2124
(defconstant +colors+ '(:black 0
                        :red 1
                        :green 2
                        :yellow 3
                        :blue 4
                        :purple 5
                        :cyan 6
                        :white 7
                        :default 9))

(defun fg-set (color-name)
  (format t "~a[~am" #\Esc (+ 30 (getf +colors+ color-name))))

(defun fg-reset ()
  (fg-set :default)
  (finish-output))

(defmacro fmt-fg (color-name fmt-string &rest args)
  `(progn
    (fg-set ,color-name)
    (apply #'format t ,fmt-string ,args)
    (fg-reset)))
