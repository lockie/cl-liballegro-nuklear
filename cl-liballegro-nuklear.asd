(defclass makefile (asdf:source-file) ())
(defmethod perform ((o load-op) (c makefile)) t)
(defmethod perform ((o compile-op) (c makefile))
  (uiop:run-program (format nil "cd ~a ~a && make"
                            #+windows "/d" #-windows ""
                            (uiop:pathname-directory-pathname
                             (component-pathname c)))
                    :output t))

(asdf:defsystem :cl-liballegro-nuklear
  :version "0.0.5"
  :description "CFFI wrapper for the Nuklear IM GUI library with liballegro backend, to be used with cl-liballegro."
  :author "Andrew Kravchuk <awkravchuk@gmail.com>"
  :license "MIT"
  :depends-on (:cffi :cffi-libffi :trivial-features)
  :pathname "src"
  :serial t
  :components ((:makefile "Makefile")
               (:file "package")
               (:file "library")
               (:file "interface")
               (:file "lispy")
               (:file "offsets")))
