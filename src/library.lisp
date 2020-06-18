(in-package :cl-liballegro-nuklear)

(cffi:define-foreign-library liballegro-nuklear
  (:windows "allegro_nuklear.dll")
  (t (:default "liballegro_nuklear")))

(pushnew
 (asdf:system-relative-pathname :cl-liballegro-nuklear "src/")
 cffi:*foreign-library-directories*)

(cffi:use-foreign-library liballegro-nuklear)
