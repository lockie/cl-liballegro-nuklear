(ql:quickload '(:cl-liballegro :cl-liballegro-nuklear :parse-float :float-features))

(defconstant +WINDOW-WIDTH+ 1200)
(defconstant +WINDOW-HEIGHT+ 800)

(defun set-style (ctx theme)
  (cffi:with-foreign-object (table
                             '(:struct nk:color)
                             (cffi:foreign-enum-value 'nk:style-colors :+color-count+))
    (flet ((set-table (color-designator new-r new-g new-b new-a)
             (declare (unsigned-byte new-r new-g new-b new-a))
             (let ((color (cffi:mem-aptr
                           table
                           '(:struct nk:color)
                           (cffi:foreign-enum-value 'nk:style-colors color-designator))))
               (cffi:with-foreign-slots ((nk::r nk::g nk::b nk::a) color (:struct nk:color))
                 (setf nk::r new-r
                       nk::g new-g
                       nk::b new-b
                       nk::a new-a)))))
      (case theme
        (white
         (set-table :+color-text+ 70 70 70 255)
         (set-table :+color-window+ 175 175 175 255)
         (set-table :+color-header+ 175 175 175 255)
         (set-table :+color-border+ 0 0 0 255)
         (set-table :+color-button+ 185 185 185 255)
         (set-table :+color-button-hover+ 170 170 170 255)
         (set-table :+color-button-active+ 160 160 160 255)
         (set-table :+color-toggle+ 150 150 150 255)
         (set-table :+color-toggle-hover+ 120 120 120 255)
         (set-table :+color-toggle-cursor+ 175 175 175 255)
         (set-table :+color-select+ 190 190 190 255)
         (set-table :+color-select-active+ 175 175 175 255)
         (set-table :+color-slider+ 190 190 190 255)
         (set-table :+color-slider-cursor+ 80 80 80 255)
         (set-table :+color-slider-cursor-hover+ 70 70 70 255)
         (set-table :+color-slider-cursor-active+ 60 60 60 255)
         (set-table :+color-property+ 175 175 175 255)
         (set-table :+color-edit+ 150 150 150 255)
         (set-table :+color-edit-cursor+ 0 0 0 255)
         (set-table :+color-combo+ 175 175 175 255)
         (set-table :+color-chart+ 160 160 160 255)
         (set-table :+color-chart-color+ 45 45 45 255)
         (set-table :+color-chart-color-highlight+  255 0 0 255)
         (set-table :+color-scrollbar+ 180 180 180 255)
         (set-table :+color-scrollbar-cursor+ 140 140 140 255)
         (set-table :+color-scrollbar-cursor-hover+ 150 150 150 255)
         (set-table :+color-scrollbar-cursor-active+ 160 160 160 255)
         (set-table :+color-tab-header+ 180 180 180 255)
         (nk:style-from-table ctx table))
        (red
         (set-table :+color-text+ 190 190 190 255)
         (set-table :+color-window+ 30 33 40 215)
         (set-table :+color-header+ 181 45 69 220)
         (set-table :+color-border+ 51 55 67 255)
         (set-table :+color-button+ 181 45 69 255)
         (set-table :+color-button-hover+ 190 50 70 255)
         (set-table :+color-button-active+ 195 55 75 255)
         (set-table :+color-toggle+ 51 55 67 255)
         (set-table :+color-toggle-hover+ 45 60 60 255)
         (set-table :+color-toggle-cursor+ 181 45 69 255)
         (set-table :+color-select+ 51 55 67 255)
         (set-table :+color-select-active+ 181 45 69 255)
         (set-table :+color-slider+ 51 55 67 255)
         (set-table :+color-slider-cursor+ 181 45 69 255)
         (set-table :+color-slider-cursor-hover+ 186 50 74 255)
         (set-table :+color-slider-cursor-active+ 191 55 79 255)
         (set-table :+color-property+ 51 55 67 255)
         (set-table :+color-edit+ 51 55 67 225)
         (set-table :+color-edit-cursor+ 190 190 190 255)
         (set-table :+color-combo+ 51 55 67 255)
         (set-table :+color-chart+ 51 55 67 255)
         (set-table :+color-chart-color+ 170 40 60 255)
         (set-table :+color-chart-color-highlight+  255 0 0 255)
         (set-table :+color-scrollbar+ 30 33 40 255)
         (set-table :+color-scrollbar-cursor+ 64 84 95 255)
         (set-table :+color-scrollbar-cursor-hover+ 70 90 100 255)
         (set-table :+color-scrollbar-cursor-active+ 75 95 105 255)
         (set-table :+color-tab-header+ 181 45 69 220)
         (nk:style-from-table ctx table))
        (blue
         (set-table :+color-text+ 20 20 20 255)
         (set-table :+color-window+ 202 212 214 215)
         (set-table :+color-header+ 137 182 224 220)
         (set-table :+color-border+ 140 159 173 255)
         (set-table :+color-button+ 137 182 224 255)
         (set-table :+color-button-hover+ 142 187 229 255)
         (set-table :+color-button-active+ 147 192 234 255)
         (set-table :+color-toggle+ 177 210 210 255)
         (set-table :+color-toggle-hover+ 182 215 215 255)
         (set-table :+color-toggle-cursor+ 137 182 224 255)
         (set-table :+color-select+ 177 210 210 255)
         (set-table :+color-select-active+ 137 182 224 255)
         (set-table :+color-slider+ 177 210 210 255)
         (set-table :+color-slider-cursor+ 137 182 224 245)
         (set-table :+color-slider-cursor-hover+ 142 188 229 255)
         (set-table :+color-slider-cursor-active+ 147 193 234 255)
         (set-table :+color-property+ 210 210 210 255)
         (set-table :+color-edit+ 210 210 210 225)
         (set-table :+color-edit-cursor+ 20 20 20 255)
         (set-table :+color-combo+ 210 210 210 255)
         (set-table :+color-chart+ 210 210 210 255)
         (set-table :+color-chart-color+ 137 182 224 255)
         (set-table :+color-chart-color-highlight+  255 0 0 255)
         (set-table :+color-scrollbar+ 190 200 200 255)
         (set-table :+color-scrollbar-cursor+ 64 84 95 255)
         (set-table :+color-scrollbar-cursor-hover+ 70 90 100 255)
         (set-table :+color-scrollbar-cursor-active+ 75 95 105 255)
         (set-table :+color-tab-header+ 156 193 220 255)
         (nk:style-from-table ctx table))
        (dark
         (set-table :+color-text+ 210 210 210 255)
         (set-table :+color-window+ 57 67 71 215)
         (set-table :+color-header+ 51 51 56 220)
         (set-table :+color-border+ 46 46 46 255)
         (set-table :+color-button+ 48 83 111 255)
         (set-table :+color-button-hover+ 58 93 121 255)
         (set-table :+color-button-active+ 63 98 126 255)
         (set-table :+color-toggle+ 50 58 61 255)
         (set-table :+color-toggle-hover+ 45 53 56 255)
         (set-table :+color-toggle-cursor+ 48 83 111 255)
         (set-table :+color-select+ 57 67 61 255)
         (set-table :+color-select-active+ 48 83 111 255)
         (set-table :+color-slider+ 50 58 61 255)
         (set-table :+color-slider-cursor+ 48 83 111 245)
         (set-table :+color-slider-cursor-hover+ 53 88 116 255)
         (set-table :+color-slider-cursor-active+ 58 93 121 255)
         (set-table :+color-property+ 50 58 61 255)
         (set-table :+color-edit+ 50 58 61 225)
         (set-table :+color-edit-cursor+ 210 210 210 255)
         (set-table :+color-combo+ 50 58 61 255)
         (set-table :+color-chart+ 50 58 61 255)
         (set-table :+color-chart-color+ 48 83 111 255)
         (set-table :+color-chart-color-highlight+ 255 0 0 255)
         (set-table :+color-scrollbar+ 50 58 61 255)
         (set-table :+color-scrollbar-cursor+ 48 83 111 255)
         (set-table :+color-scrollbar-cursor-hover+ 53 88 116 255)
         (set-table :+color-scrollbar-cursor-active+ 58 93 121 255)
         (set-table :+color-tab-header+ 48 83 111 255)
         (nk:style-from-table ctx table))
        (t (nk:style-default ctx))))))

(cffi:defcallback filter-float :int ((edit :pointer) (rune :unsigned-int))
  (nk:filter-float edit rune))

(defun calculator (ctx)
  (unless (zerop (nk:begin ctx "Calculator" '(nk::x 10f0 nk::y 10f0 nk::w 180f0 nk::h 250f0)
                           (nk:flags :panel-flags
                                     :+window-border+
                                     :+window-no-scrollbar+
                                     :+window-movable+)))
    (defvar st nil)
    (defvar prev 0)
    (defvar opr 0)
    (defvar numbers "789456123")
    (defvar ops "+-*/")
    (defvar a 0d0)
    (defvar b 0d0)
    (defvar current 'a)
    (defparameter solve nil)

    (flet ((set-current (value)
             (ecase current
               (a (setf a value))
               (b (setf b value)))))
      (nk:layout-row-dynamic ctx 35f0 1)
      (cffi:with-foreign-string (buffer (format nil "~,2f" (eval current)))
        (nk:edit-string-zero-terminated ctx (cffi:foreign-enum-value 'nk:edit-types :+edit-simple+) buffer 255 (cffi:callback filter-float))
        (set-current (parse-float:parse-float (cffi:foreign-string-to-lisp buffer))))

      (nk:layout-row-dynamic ctx 35f0 4)
      (loop :for i :from 0 :below 16 :do
        (block continue
          (cond
            ((and (>= i 12) (< i 15))
             (when (> i 12)
               (return-from continue))
             (unless (zerop (nk:button-label ctx "C"))
               (setf a 0d0
                     b 0d0
                     opr 0
                     current 'a
                     st nil))
             (unless (zerop (nk:button-label ctx "0"))
               (set-current (* (eval current) 10d0))
               (setf st nil))
             (unless (zerop (nk:button-label ctx "="))
               (setf solve t)
               (setf prev opr)
               (setf opr 0)))
            ((not (zerop (mod (1+ i) 4)))
             (unless (zerop (nk:button-text
                             ctx
                             (subseq numbers (+ (* (truncate i 4) 3) (mod i 4)))
                             1))
             (set-current
              (+ (* (eval current) 10d0)
                 (digit-char-p (char numbers (+ (* (truncate i 4) 3) (mod i 4))))))
             (setf st nil)))
            ((not (zerop (nk:button-text ctx (subseq ops (truncate i 4)) 1)))
             (unless st
               (if (not (eq current 'b))
                   (setf current 'b)
                   (setf prev opr
                         solve t)))
             (setf opr (char ops (truncate i 4))
                   st t)))))
      (when solve
        (case prev
          (#\+ (setf a (+ a b)))
          (#\- (setf a (- a b)))
          (#\* (setf a (* a b)))
          (#\/ (setf a (/ a b))))
        (setf current 'a)
        (when st
          (setf current 'b))
        (setf b 0d0
              st nil))
      (nk:end ctx))))

(declaim (inline event-type))
(defun event-type (event)
  (cffi:foreign-slot-value event '(:union al:event) 'al::type))

(defmacro defstatic* (type var &optional init &key (count 1))
  `(progn
     (defvar ,var (cffi:null-pointer))
     (when (cffi:null-pointer-p ,var)
       (setf ,var (cffi:foreign-alloc ,type ,@(when init `(:initial-element ,init)) :count ,count)))))

(cffi:defcallback main :int ((argc :int) (argv :pointer))
  (declare (ignore argc argv))
  (unless (and (al:init)
               (al:init-primitives-addon)
               (al:init-image-addon)
               (or (al:init-font-addon) t)
               (al:init-ttf-addon))
    (error "failed to initialize allegro5!"))
  (al:install-mouse)
  (al:install-keyboard)
  (al:set-new-display-flags '(:windowed :resizable :opengl))
  (al:set-new-display-option :sample-buffers 1 :suggest)
  (al:set-new-display-option :samples 8 :suggest)
  (let ((display (al:create-display +WINDOW-WIDTH+ +WINDOW-HEIGHT+)))
    (when (cffi:null-pointer-p display)
      (error "failed to create display!"))
    (let ((event-queue (al:create-event-queue)))
      (when (cffi:null-pointer-p event-queue)
        (error "failed to create event_queue!"))
      (al:register-event-source event-queue (al:get-display-event-source display))
      (al:register-event-source event-queue (al:get-mouse-event-source))
      (al:register-event-source event-queue (al:get-keyboard-event-source))
      (loop
        :with font := (nk:allegro-font-create-from-file "../Roboto-Regular.ttf" 12 0)
        :with ctx := (nk:allegro-init font display +WINDOW-WIDTH+ +WINDOW-HEIGHT+)
        :with op := :easy
          :initially
             (defstatic* '(:union al:event) ev)
             (set-style ctx 'default)
             ;; (set-style ctx 'white)
             ;; (set-style ctx 'red)
             ;; (set-style ctx 'blue)
             ;; (set-style ctx 'dark)
        :do (let ((get-event (al:wait-for-event-timed event-queue ev 0.06)))
              (when (and get-event (eq (event-type ev) :display-close))
                (loop-finish))
              ;; Very Important: Always do nk_input_begin / nk_input_end even if
              ;; there are no events, otherwise internal nuklear state gets messed up
              (nk:input-begin ctx)
              (loop :while get-event
                    :do (nk:allegro-handle-event ev)
                        (setf get-event (al:get-next-event event-queue ev)))
              (nk:input-end ctx)
              ;; GUI
              (unless (zerop (nk:begin
                              ctx "Demo" '(nk::x 50f0 nk::y 50f0 nk::w 200f0 nk::h 200f0)
                              (nk:flags :panel-flags
                                        :+window-border+
                                        :+window-movable+
                                        :+window-scalable+
                                        :+window-closable+
                                        :+window-minimizable+
                                        :+window-title+)))
                (defstatic* :int property 20)
                (nk:layout-row-static ctx 30f0 80 1)
                (unless (zerop (nk:button-label ctx "button"))
                  (format t "button pressed~%"))
                (nk:layout-row-dynamic ctx 30f0 2)
                (unless (zerop (nk:option-label
                                ctx "easy"
                                (cffi:convert-to-foreign (eq op :easy) :boolean)))
                  (setf op :easy))
                (unless (zerop (nk:option-label
                                ctx "hard"
                                (cffi:convert-to-foreign (eq op :hard) :boolean)))
                  (setf op :hard))
                (nk:layout-row-dynamic ctx 22f0 1)
                (nk:property-int ctx "Compression:" 0 property 100 10 1f0))
              (nk:end ctx)
              (calculator ctx)
              (al:clear-to-color (al:map-rgb 19 43 81))
              (nk:allegro-render)
              (al:flip-display))
        :finally
           (nk:allegro-font-del font)
           (nk:allegro-shutdown)
           (al:destroy-display display)
           (al:destroy-event-queue event-queue)
           (return 0)))))

(float-features:with-float-traps-masked
    (:divide-by-zero :invalid :inexact :overflow :underflow)
  (al:run-main 0 (cffi:null-pointer) (cffi:callback main)))
