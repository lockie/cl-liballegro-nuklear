(in-package :cl-liballegro-nuklear)

(defmacro with-color ((var &key r g b a) &body body)
  "Executes BODY with the variable VAR bound to Nuklear color struct with fields R, G, B, A."
  `(let ((,var `(r ,,r g ,,g b ,,b a ,,a)))
     ,@body))

(defmacro with-rect ((var &key x y w h) &body body)
  "Executes BODY with the variable VAR bound to Nuklear rect struct with fields X, Y, W, H."
  `(let ((,var `(x ,(float ,x) y ,(float ,y) w ,(float ,w) h ,(float ,h))))
     ,@body))

(defmacro with-window (context title bounds flags &body body)
  "Calls nk_begin with CONTEXT, TITLE, BOUNDS and FLAGS arguments, executes BODY when
it returns non-zero value, then calls nk_end."
  `(progn
     (let ((panel-flags (apply 'flags :panel-flags ,flags)))
       (unless (zerop (the fixnum (begin ,context ,title ,bounds panel-flags)))
         ,@body)
       (end ,context))))

(defmacro with-button-label (context title &body body)
  "Calls nk_button_label with CONTEXT and TITLE arguments and executes BODY when
it returns non-zero value (e.g. when the button is pressed)."
  `(unless (zerop (the fixnum (button-label ,context ,title)))
     ,@body))

(defmacro with-layout-space (context format height widget-count &body body)
  "Calls nk_layout_space_begin with CONTEXT, FORMAT, HEIGHT and WIDGET-COUNT arguments,
then executes BODY, then calls nk_layout_space_end."
  `(progn
     (layout-space-begin ,context ,format ,(float height) ,widget-count)
     ,@body
     (layout-space-end ,context)))

(defmacro with-style-item (context offset item &body body)
  "Calls nk_style_push_style_item with CONTEXT, OFFSET and ITEM arguments,
then executes BODY, then calls nk_style_pop_style_item."
  `(progn
     (style-push-style-item ,context (cffi:inc-pointer ,context ,offset) ,item)
     ,@body
     (style-pop-style-item ,context)))

(defmacro with-style-items (items &body body)
  "Use this instead of chain calling WITH-STYLE-ITEM."
  (if items
      (let ((item (first items)))
        `(with-style-item ,(first item) ,(second item) ,(third item)
           (with-style-items ,(rest items)
             ,@body)))
      `(progn ,@body)))

(defmacro with-style-color (context offset (&key r g b a) &body body)
  "Calls nk_style_push_color with CONTEXT, OFFSET and R, G, B, A arguments,
then executes BODY, then calls nk_style_pop_color."
  `(progn
     (style-push-color ,context (cffi:inc-pointer ,context ,offset) `(r ,,r g ,,g b ,,b a ,,a))
     ,@body
     (style-pop-color ,context)))

(defmacro with-style-colors (colors &body body)
  "Use this instead of chain calling WITH-STYLE-COLOR."
  (if colors
      (let ((color (first colors)))
        (destructuring-bind (context offset (&key r g b a)) color
          `(with-style-color ,context ,offset (:r ,r :g ,g :b ,b :a ,a)
             (with-style-colors ,(rest colors)
               ,@body))))
      `(progn ,@body)))

(defmacro with-style-font (context font &body body)
  "Calls nk_style_push_font with CONTEXT and FONT arguments, then executes BODY,
then calls nk_style_pop_font."
  `(progn
     (style-push-font ,context ,font)
     ,@body
     (style-pop-font ,context)))

(defmacro with-input (context &body body)
  "Calls nk_input_begin with CONTEXT argument, then executes BODY, then calls nk_input_end."
  `(progn
     (input-begin ,context)
     ,@body
     (input-end ,context)))
