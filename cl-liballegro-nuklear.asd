(defclass makefile (asdf:source-file) ())
(defmethod perform ((o load-op) (c makefile)) t)
(defmethod perform ((o compile-op) (c makefile))
  (uiop:run-program (format nil "make -f ~a" (component-pathname c)) :output :interactive))

(asdf:defsystem :cl-liballegro-nuklear
  :version "0.0.1"
  :description "CFFI wrapper for the Nuklear IM GUI library with liballegro backend, to be used with cl-liballegro."
  :author "Andrew Kravchuk <awkravchuk@gmail.com>"
  :license "MIT"
  :depends-on (:cffi :cffi-libffi)
  :pathname "src"
  :serial t
  :components ((:makefile "Makefile")
               (:file "package")
               (:file "library")
               (:file "interface")
               (:file "lispy")
               (:file "offsets")))
