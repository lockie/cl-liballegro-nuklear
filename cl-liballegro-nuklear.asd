(defclass makefile (asdf:source-file) ())
(defmethod perform ((o load-op) (c makefile)) t)
(defmethod perform ((o compile-op) (c makefile))
  (uiop:run-program
   (format nil "make -C ~a -s"
           (uiop:pathname-directory-pathname
            (component-pathname c)))
   :output t))

(asdf:defsystem :cl-liballegro-nuklear
  :version "0.0.9"
  :description "CFFI wrapper for the Nuklear IM GUI library with liballegro backend, to be used with cl-liballegro."
  :author "Andrew Kravchuk <awkravchuk@gmail.com>"
  :license "MIT"
  :depends-on (:cffi :cffi-libffi :cl-liballegro :trivial-features)
  :pathname "src"
  :serial t
  :components ((:makefile "Makefile")
               (:file "package")
               (:file "library")
               (:file "interface")
               (:file "lispy")
               (:file "offsets")))

(asdf:defsystem :cl-liballegro-nuklear/declarative
  :description "Declarative UI interface for cl-liballegro-nuklear."
  :author "Andrew Kravchuk <awkravchuk@gmail.com>"
  :license "MIT"
  :depends-on (:cl-liballegro-nuklear :alexandria :uiop)
  :pathname "src"
  :serial t
  :components ((:file "declarative")))
