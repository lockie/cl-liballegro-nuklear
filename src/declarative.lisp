(in-package :cl-user)

(defpackage :cl-liballegro-nuklear/declarative
  (:documentation "Declarative UI interface for cl-liballegro-nuklear.")
  (:use :cl)
  (:import-from :alexandria :destructuring-case :format-symbol :non-negative-fixnum :parse-body :with-gensyms)
  (:import-from :uiop :string-prefix-p)
  (:export
   #:defwindow
   #:defgroup
   #:with-context
   #:with-disabled-widgets
   #:styles
   #:layout-row-static
   #:layout-row-dynamic
   #:layout-rows
   #:layout-row-push
   #:layout-space
   #:layout-space-push
   #:label
   #:label-wrap
   #:selectable-label
   #:button-label
   #:button-image
   #:progress
   #:edit
   #:input-has-mouse-click
   #:input-has-mouse-click-in))

(in-package :cl-liballegro-nuklear/declarative)


(defmacro coerce-constant (c type)
  (if (and (constantp c) (atom c) (not (symbolp c)))
      (coerce c type)
      `(coerce ,c ',type)))

(defmacro coerce-rect (&key x y w h)
  ``(nk::x ,(coerce-constant ,x single-float)
           nk::y ,(coerce-constant ,y single-float)
           nk::w ,(coerce-constant ,w single-float)
           nk::h ,(coerce-constant ,h single-float)))

(defun coerce-flags (type prefix &rest flags)
  (if flags
      `(nk:flags
        ,type
        ,@(mapcar
           #'(lambda (flag)
               (if (string-prefix-p prefix flag)
                   flag
                   (format-symbol :keyword "~a~:@(~a~)+" prefix flag)))
           flags))
      0))

(defmacro defwindow (name args
                     (&key title (x 0) (y 0) (w 200) (h 200) flags styles)
                     &body body)
  (with-gensyms (context result)
    (multiple-value-bind (forms declarations docstring)
        (parse-body body :documentation t)
      `(defun ,name (,context ,@args)
         (declare (type cffi:foreign-pointer ,context))
         ,@declarations
         ,(if docstring docstring title)
         (macrolet ((call-with-context (function)
                      `(funcall ,function ,',context)))
           (,(if styles 'styles 'progn)
            ,styles
            (let ((,result
                    (plusp
                     (the fixnum
                          (,(if title 'nk:begin-titled 'nk:begin)
                           ,context
                           (string-upcase ',name)
                           ,@(when title (list title))
                           `(,@(coerce-rect :x ,x :y ,y :w ,w :h ,h))
                           ,(apply #'coerce-flags :panel-flags "+WINDOW-"
                                   flags))))))
              (unwind-protect
                   (when ,result
                     ,@forms)
                (nk:end ,context)))))))))

(defmacro defgroup (name (&key title flags styles)  &body body)
  (with-gensyms (context result)
    (multiple-value-bind (forms declarations docstring)
        (parse-body body :documentation t)
    `(call-with-context
      (lambda (,context)
        (declare (type cffi:foreign-pointer ,context))
        ,@declarations
        ,(if docstring docstring title)
        (,(if styles 'styles 'progn)
         ,styles
         (let ((,result
                 (plusp
                  (the fixnum
                       (,(if title 'nk:group-begin-titled 'nk:group-begin)
                        ,context
                        (string-upcase ',name)
                        ,@(when title `(,(string title)))
                        ,(apply #'coerce-flags :panel-flags "+WINDOW-"
                                flags))))))
           (unwind-protect
                (when ,result
                  ,@forms
                  nil)
             (when ,result
               (nk:group-end ,context))))))))))

(defmacro with-context (context &body body)
  `(call-with-context
    (lambda (,context)
      ,@body)))

(defmacro with-disabled-widgets ((&key (disabled t)) &body body)
  (with-gensyms (context)
    `(call-with-context
      (lambda (,context)
        (when ,disabled
          (nk:widget-disable-begin ,context))
        ,@body
        (when ,disabled
          (nk:widget-disable-end ,context))))))

(defmacro style-offset (context offset)
  `(cffi:inc-pointer
    ,context
    ,(if (string-prefix-p "+STYLE-" offset)
         offset
         (format-symbol :nk "+STYLE-~:@(~a~)+" offset))))

(defmacro styles (styles &body body)
  (with-gensyms (context offset item)
    `(call-with-context
      (lambda (,context)
        ,@(loop
            :for style :in styles
            :for (initializer finalizer)
               := (destructuring-case style
                    ((:item-color name &key (r 0) (g 0) (b 0) (a 255))
                     `((let ((,offset (style-offset ,context ,name))
                             (,item
                               ,(if (and (constantp r)
                                         (constantp g)
                                         (constantp b)
                                         (constantp a))
                                    `(load-time-value
                                      (nk:style-item-color
                                       '(nk::r ,r nk::g ,g nk::b ,b nk::a ,a))
                                      t)
                                    `(nk:style-item-color
                                      '(nk::r ,r nk::g ,g nk::b ,b nk::a ,a)))))
                         (nk:style-push-style-item ,context ,offset ,item))
                       (nk:style-pop-style-item ,context)))
                    ((:item-image name image)
                     `((let ((,offset (style-offset ,context ,name))
                             (,item `(nk::data (nk::img ,,image)
                                               nk::type :+style-item-image+)))
                         (declare (dynamic-extent ,item))
                         (nk:style-push-style-item ,context ,offset ,item))
                       (nk:style-pop-style-item ,context)))
                    ((:item-9slice name image &key left top right bottom)
                     (let ((w/3 `(truncate
                                  (cffi:foreign-slot-value ,image
                                                           '(:struct nk:image)
                                                           'nk::w)
                                  3))
                           (h/3 `(truncate
                                  (cffi:foreign-slot-value ,image
                                                           '(:struct nk:image)
                                                           'nk::h)
                                  3)))
                       `((let ((,offset (style-offset ,context ,name))
                               (,item `(nk::data
                                        (nk::img
                                         ,,image
                                         nk::l
                                         ,(if ,left ,left ,w/3)
                                         t ,(if ,top ,top ,h/3)
                                         nk::r ,(if ,right ,right ,w/3)
                                         nk::b ,(if ,bottom ,bottom ,h/3))
                                        nk::type :+style-item-nine-slice+)))
                           (declare (dynamic-extent ,item))
                           (nk:style-push-style-item ,context ,offset ,item))
                         (nk:style-pop-style-item ,context))))
                    ((:color name &key (r 0) (g 0) (b 0) (a 255))
                     `((let ((,offset (style-offset ,context ,name)))
                         (nk:style-push-color
                          ,context ,offset
                          `(nk::r ,,r nk::g ,,g nk::b ,,b nk::a ,,a)))
                       (nk:style-pop-color ,context)))
                    ((:font font)
                     `((nk:style-push-font ,context ,font)
                       (nk:style-pop-font ,context)))
                    ((:float name value)
                     `((let ((,offset (style-offset ,context ,name)))
                         (nk:style-push-float ,context ,offset (float ,value)))
                       (nk:style-pop-float ,context)))
                    ((:vec2 name &key (x 0) (y 0))
                     `((let ((,offset (style-offset ,context ,name)))
                         (nk:style-push-vec-2
                          ,context ,offset
                          `(nk::x ,(coerce-constant ,x single-float)
                                  nk::y ,(coerce-constant ,y single-float))))
                       (nk:style-pop-vec-2 ,context))))
            :collect initializer :into initializers
            :collect finalizer :into finalizers
            :finally (return
                       `(,@initializers
                         (unwind-protect
                              (progn ,@body)
                           ,@(reverse finalizers)))))))))

(defmacro layout-row-static (&key height item-width columns)
  (with-gensyms (context)
   `(call-with-context
     (lambda (,context)
       (nk:layout-row-static
        ,context
        (coerce-constant ,height single-float)
        (coerce-constant ,item-width fixnum)
        (coerce-constant ,columns fixnum))))))

(defmacro layout-row-dynamic (&key height columns)
  (with-gensyms (context)
    `(call-with-context
      (lambda (,context)
        (nk:layout-row-dynamic
         ,context
         (coerce-constant ,height single-float)
         (coerce-constant ,columns fixnum))))))

(defmacro layout-rows ((&key height format columns) &body body)
  (with-gensyms (context)
    `(call-with-context
      (lambda (,context)
        (nk:layout-row-begin
         ,context
         ,(coerce-flags :layout-format "+" format)
         (coerce-constant ,height single-float)
         ,columns)
        (unwind-protect
             (progn ,@body)
          (nk:layout-row-end ,context))))))

(defmacro layout-row-push (width-or-ratio)
  (with-gensyms (context)
    `(call-with-context
      (lambda (,context)
        (nk:layout-row-push ,context
                            (coerce-constant ,width-or-ratio single-float))))))

(defmacro layout-space ((&key height format widget-count) &body body)
  (with-gensyms (context)
    `(call-with-context
      (lambda (,context)
        (nk:layout-space-begin
         ,context
         ,(coerce-flags :layout-format "+" format)
         (coerce-constant ,height single-float)
         (coerce-constant ,(if widget-count
                               widget-count
                               #.(1- (ash 1 31)))
                          fixnum))
        (unwind-protect
             (progn ,@body)
          (nk:layout-space-end ,context))))))

(defmacro layout-space-push (&key x y w h)
  (with-gensyms (context)
    `(call-with-context
      (lambda (,context)
        (nk:layout-space-push
         ,context
         `(,@(coerce-rect :x ,x :y ,y :w ,w :h ,h)))))))

(defmacro label (text &key (align :left))
  (with-gensyms (context)
   `(call-with-context
     (lambda (,context)
       (nk:label
        ,context
        ,text
        ,(etypecase align
           (list (apply #'coerce-flags :text-align "+TEXT-ALIGN-" align))
           ((or symbol string)
            (coerce-flags :text-alignment "+TEXT-" align))))))))

(defmacro label-wrap (text)
  (with-gensyms (context)
   `(call-with-context
     (lambda (,context)
       (nk:label-wrap ,context ,text)))))

(defmacro selectable-label (text value &key (align :left))
  (with-gensyms (context)
   `(call-with-context
     (lambda (,context)
       (nk:selectable-label
        ,context
        ,text
        ,(etypecase align
           (list (apply #'coerce-flags :text-align "+TEXT-ALIGN-" align))
           ((or symbol string)
            (coerce-flags :text-alignment "+TEXT-" align)))
        ,value)))))

(defmacro button-label (text &body body)
  (with-gensyms (context)
    `(call-with-context
      (lambda (,context)
        (when (plusp (the fixnum (nk:button-label ,context ,text)))
          ,@body)))))

(defmacro button-image (image &body body)
  (with-gensyms (context)
    `(call-with-context
      (lambda (,context)
        (when (plusp (the fixnum (nk:button-image ,context ,image)))
          ,@body)))))

(defmacro progress (&key current (maximum 100) modifyable)
  (with-gensyms (context)
   `(call-with-context
     (lambda (,context)
       (nk:prog- ,context
                 (coerce-constant ,current non-negative-fixnum)
                 (coerce-constant ,maximum non-negative-fixnum)
                 ,(if modifyable nk::+true+ nk::+false+))))))

(defmacro edit (buffer &key size (filter (cffi:null-pointer)) flags)
  (with-gensyms (context)
    `(call-with-context
      (lambda (,context)
        (the fixnum
             (nk:edit-string-zero-terminated
              ,context
              ,(apply #'coerce-flags :edit-flags "+EDIT-" flags)
              ,buffer ,(or size `(1+ (length ,buffer))) ,filter))))))

(defmacro input-has-mouse-click (button)
  (with-gensyms (context)
    `(call-with-context
      (lambda (,context)
        (plusp
         (the fixnum
              (nk:input-has-mouse-click
               ,context
               ,(coerce-flags :buttons "+BUTTON-" button))))))))

(defmacro input-has-mouse-click-in (button &key release x y w h)
  (with-gensyms (context)
   `(call-with-context
     (lambda (,context)
       (plusp
        (the fixnum
             (,(if release
                   'nk:input-has-mouse-click-in-button-rect
                   'nk:input-has-mouse-click-in-rect)
              ,context
              ,(coerce-flags :buttons "+BUTTON-" button)
              `(,@(coerce-rect :x ,x :y ,y :w ,w :h ,h)))))))))
