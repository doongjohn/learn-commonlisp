(let ((cwd (uiop:getcwd)))
  (asdf:initialize-source-registry
  `(:source-registry
    (:directory ,(merge-pathnames #P"src/" cwd))
    :inherit-configuration)))

(asdf:load-system "main")
