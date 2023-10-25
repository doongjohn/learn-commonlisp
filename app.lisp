; (defparameter *cwd* (uiop:getcwd))
; (push (merge-pathnames #P"src/" *cwd*) asdf:*central-registry*)

(defparameter *asdf-system-search-paths*
              '(#P"./src/"))

(loop :for path :in *asdf-system-search-paths* :do
  (when (not (position path asdf:*central-registry*))
    (push path asdf:*central-registry*)))

(asdf:load-system "main")
