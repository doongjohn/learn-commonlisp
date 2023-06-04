(defpackage #:ansi-esc
  (:use
   #:cl)
  (:export
   #:fg-set ; function
   #:bg-set ; function
   #:fg-reset ; function
   #:bg-reset ; function
   #:fmt)) ; macro


(in-package #:ansi-esc)

;; https://en.wikipedia.org/wiki/ANSI_escape_code
;; https://gist.github.com/JBlond/2fea43a3049b38287e5e9cefc87b2124
(defconstant +colors+ (list :black 0
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

(defun bg-set (color-name)
  (format t "~a[~am" #\Esc (+ 40 (getf +colors+ color-name))))

(defun fg-reset ()
  (fg-set :default)
  (finish-output))

(defun bg-reset ()
  (bg-set :default)
  (finish-output))

(defmacro fmt ((&key (fg :default) (bg :default)) fmt-string &rest args)
  `(progn
    (fg-set ,fg)
    (bg-set ,bg)
    (apply #'format t ,fmt-string ,args)
    (fg-set :default)
    (bg-set :default)
    (finish-output)))
