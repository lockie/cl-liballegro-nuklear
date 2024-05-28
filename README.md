cl-liballegro-nuklear
---------------------
This is [CFFI](https://common-lisp.net/project/cffi) wrapper for the [Nuklear](https://github.com/Immediate-Mode-UI/Nuklear) IM GUI library with [liballegro](https://liballeg.github.io) backend, to be used with [cl-liballegro](https://github.com/resttime/cl-liballegro).

Nuklear is minimal immediate-mode graphical user interface toolkit. To use it alongside liballegro, you have to write [some amount of glue code](https://github.com/Immediate-Mode-UI/Nuklear/blob/master/demo/allegro5/nuklear_allegro5.h). This library brings both Nuklear itself and that glue code to Common Lisp, so you can just plug it in and have some nice minimal GUI in your [cl-liballegro](https://github.com/resttime/cl-liballegro)-powered application.

Screenshots
-----------
![screenshot](https://gitlab.com/lockie/cl-liballegro-nuklear/-/raw/master/screenshot.png)

Installation
------------
Just execute `(ql:quickload :cl-liballegro-nuklear)` in your Lisp.

To get the latest version of the package, install [Lucky Lambda](http://dist.luckylambda.technology/releases/lucky-lambda) Quicklisp distribution.

**Note**: upon loading this library with [asdf](https://common-lisp.net/project/asdf), the corresponding foreign library is automatically built (`allegro_nuklear.dll` on Windows, `liballegro_nuklear.so` on more sane operating systems), so you'll have to have [compiler toolchain](https://gcc.gnu.org) and [liballegro dev files](https://liballeg.github.io/download.html) installed on your machine.

Currently tested to work on following OSes:
* Windows (using [MinGW64](https://mingw-w64.org/downloads/#mingw-builds) and `seh-dynamic` variant of [official liballegro binaries](https://github.com/liballeg/allegro5/releases))
* MacOS (using liballegro from both [Homebrew](https://formulae.brew.sh/formula/allegro) and [MacPorts](https://ports.macports.org/port/allegro5))
* Linux
* FreeBSD

...and on following Lisp compilers:
* [SBCL](https://sbcl.org)
* [CCL](https://ccl.clozure.com)
* [ECL](https://common-lisp.net/project/ecl/main.html)
* [ACL](https://franz.com/products/allegro-common-lisp)
* [LispWorks](https://lispworks.com/products/lispworks.html)

Usage
-----

```common-lisp
(ql:quickload '(:cl-liballegro :cl-liballegro-nuklear :float-features))

(cffi:defcallback main :int ((argc :int) (argv :pointer))
  (al:init) (al:init-primitives-addon) (al:init-image-addon)
  (al:init-font-addon) (al:init-ttf-addon)
  (al:install-mouse)
  (let ((display (al:create-display 800 600))
        (queue (al:create-event-queue)))
    (al:register-event-source queue (al:get-display-event-source display))
    (al:register-event-source queue (al:get-mouse-event-source))
    (cffi:with-foreign-object (ev '(:union al:event))
      (loop :with font := (nk:allegro-font-create-from-file
                           "Roboto-Regular.ttf" 12 0)
            :with ctx := (nk:allegro-init font display 800 600)
        :do (let ((get-event (al:wait-for-event-timed queue ev 0.06)))
              (when (and get-event (eq (cffi:foreign-slot-value
                                        ev '(:union al:event) 'al::type)
                                       :display-close))
                (loop-finish))
              (nk:input-begin ctx)
              (loop :while get-event
                    :do (nk:allegro-handle-event ev)
                        (setf get-event (al:get-next-event queue ev)))
              (nk:input-end ctx)
              (unless (zerop (nk:begin ctx "Demo"
                                       '(nk::w 100f0 nk::h 100f0) 0))
                (nk:layout-row-static ctx 30f0 80 1)
                (unless (zerop (nk:button-label ctx "button"))
                  (format t "button pressed~%"))
                (nk:end ctx)
                (nk:allegro-render)
                (al:flip-display)))
        :finally (nk:allegro-font-del font)
                 (nk:allegro-shutdown)
                 (al:destroy-display display)
                 (al:destroy-event-queue queue)
                 (return 0)))))

(float-features:with-float-traps-masked
    (:divide-by-zero :invalid :inexact :overflow :underflow)
  (al:run-main 0 (cffi:null-pointer) (cffi:callback main)))
```

See [example.lisp](https://gitlab.com/lockie/cl-liballegro-nuklear/-/blob/master/src/example.lisp) for more involved example.

**Note**: `nk:prog` is renamed to `nk:prog-` because of obvious `NAME-CONFLICT` error.

There's also lispy interface making library interaction more idiomatic of CL. See docstrings in [lispy.lisp](https://gitlab.com/lockie/cl-liballegro-nuklear/-/blob/master/src/lispy.lisp) for documentation.

Declarative interface
---------------------
There's also declarative interface in separate package `cl-liballegro-nuklear/declarative` which allows you to define an anonymous function doing all required FFI calls and type conversions. To do that, use `defwindow` macro in the following fashion:

```common-lisp
(uiop:add-package-local-nickname :ui :cl-liballegro-nuklear/declarative)

(setf *window*
      (ui:defwindow demo ()
          (:x 50 :y 50 :w 200 :h 200
           :flags (border movable))
        (ui:layout-row-static :height 30 :item-width 80 :columns 1)
        (ui:button-label "button"
          (format t "button pressed!~%"))))

;; then somewhere in your main loop
(funcall *window* nuklear-context)


;; another example:
(setf *window*
      (ui:defwindow loading (&key progress file)
          (:w display-width :h display-height
           :styles ((:item-color :window-fixed-background :r 20)))
        (declare (type alexandria:non-negative-fixnum progress))
        (ui:layout-space (:format :dynamic :height 54 :widget-count 1)
          (ui:layout-space-push :x 0.28 :y 6 :w 0.45 :h 1)
          (ui:styles ((:item-color :progress-normal :r 50 :g 50 :b 50)
                      (:item-color :progress-cursor-normal :g 50)
                      (:vec2 :progress-padding :x 0 :y 0))
            (ui:progress :current progress))
          (ui:label (format nil " Loading ~a..." file)))))

;; then somewhere in your main loop
(funcall *window* nuklear-context :progress 42 :file "some.file")
```

See [declarative.lisp](https://gitlab.com/lockie/cl-liballegro-nuklear/-/blob/master/src/declarative.lisp) for details.

Games made using cl-liballegro-nuklear
--------------------------------------
* [Darkness Looming: The Dawn](https://awkravchuk.itch.io/darkness-looming-the-dawn)
* [Thoughtbound](https://awkravchuk.itch.io/thoughtbound)
* [Mana Break](https://awkravchuk.itch.io/mana-break)
* [Cycle of Evil](https://awkravchuk.itch.io/cycle-of-evil)
* [make your own!](https://gitlab.com/lockie/cl-liballegro-nuklear/-/issues/new)

Related projects
----------------
* [bodge-nuklear](http://quickdocs.org/bodge-nuklear), which depends on [nuklear-blob](http://quickdocs.org/nuklear-blob) is other CFFI wrapper for Nuklear. It does not include glue code for liballegro, but designed to work with OpenGL-based [cl-bodge](https://github.com/borodust/cl-bodge) game framework.

Contributing
------------
Merge requests are welcome. For major changes, please [open an issue](https://gitlab.com/lockie/cl-liballegro-nuklear/-/issues/new) first to discuss what you would like to change.

License
-------
[MIT](https://choosealicense.com/licenses/mit)
