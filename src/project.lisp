(let ((cwd (uiop:getcwd)))
  (asdf:initialize-source-registry
  `(:source-registry
    (:directory ,(merge-pathnames #P"src/" cwd))
    :inherit-configuration)))

(asdf:load-system "main")

;; run project
(defun run ()
  (main:main))

;; build executable
(defun build ()
  (sb-ext:save-lisp-and-die "app.exe" :toplevel 'main:main :executable t))
