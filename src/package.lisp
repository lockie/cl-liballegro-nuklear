(in-package :cl-user)

(defpackage :cl-liballegro-nuklear
  (:documentation "CFFI wrapper for Nuklear IM GUI library, to be used with cl-liballegro.")
  (:nicknames #:nk)
  (:use :cl)
  (:export
   #:false
   #:true
   #:color
   #:colorf
   #:vec-2
   #:rect
   #:glyph
   #:handle
   #:image
   #:nine-slice
   #:scroll
   #:heading
   #:button-behavior
   #:chart-type
   #:popup-type
   #:symbol-type
   #:keys
   #:buttons
   #:collapse-states
   #:show-states
   #:layout-format
   #:tree-type
   #:panel-flags
   #:widget-layout-states
   #:widget-states
   #:text-align
   #:text-alignment
   #:edit-flags
   #:edit-types
   #:edit-events
   #:style-colors
   #:style-cursor
   ;; Context
   #:init-default
   #:init-fixed
   #:init
   #:init-custom
   #:clear
   #:free
   ;; Input
   #:input-begin
   #:input-motion
   #:input-key
   #:input-button
   #:input-scroll
   #:input-char
   #:input-glyph
   #:input-unicode
   #:input-end
   ;; Window
   #:begin
   #:begin-titled
   #:end
   #:window-find
   #:window-get-bounds
   #:window-get-position
   #:window-get-size
   #:window-get-width
   #:window-get-height
   #:window-get-panel
   #:window-get-content-region
   #:window-get-content-region-min
   #:window-get-content-region-max
   #:window-get-content-region-size
   #:window-get-canvas
   #:window-get-scroll
   #:window-has-focus
   #:window-is-hovered
   #:window-is-collapsed
   #:window-is-closed
   #:window-is-hidden
   #:window-is-active
   #:window-is-any-hovered
   #:item-is-any-active
   #:window-set-bounds
   #:window-set-position
   #:window-set-size
   #:window-set-focus
   #:window-set-scroll
   #:window-close
   #:window-collapse
   #:window-collapse-if
   #:window-show
   #:window-show-if
   ;; Layout
   #:layout-set-min-row-height
   #:layout-reset-min-row-height
   #:layout-widget-bounds
   #:layout-ratio-from-pixel
   #:layout-row-dynamic
   #:layout-row-static
   #:layout-row-begin
   #:layout-row-push
   #:layout-row-end
   #:layout-row
   #:layout-row-template-begin
   #:layout-row-template-push-dynamic
   #:layout-row-template-push-variable
   #:layout-row-template-push-static
   #:layout-row-template-end
   #:layout-space-begin
   #:layout-space-push
   #:layout-space-end
   #:layout-space-bounds
   #:layout-space-to-screen
   #:layout-space-to-local
   #:layout-space-rect-to-screen
   #:layout-space-rect-to-local
   #:spacer
   ;; Groups
   #:group-begin
   #:group-begin-titled
   #:group-end
   #:group-scrolled-offset-begin
   #:group-scrolled-begin
   #:group-scrolled-end
   #:group-get-scroll
   #:group-set-scroll
   ;; 9-Slice
   #:nine-slice-handle
   #:nine-slice-ptr
   #:nine-slice-id
   #:nine-slice-is-sub9slice
   #:sub-9slice-ptr
   #:sub-9slice-id
   #:sub-9slice-handle
   ;; Tree
   #:tree-push
   #:tree-push-hashed
   #:tree-image-push-hashed
   #:tree-pop
   #:tree-state-push
   #:tree-state-image-push
   #:tree-state-pop
   #:tree-element-push-hashed
   #:tree-element-image-push-hashed
   #:tree-element-pop
   ;; List view
   #:list-view-begin
   #:list-view-end
   ;; Widget
   #:widget
   #:widget-fitting
   #:widget-bounds
   #:widget-position
   #:widget-size
   #:widget-width
   #:widget-height
   #:widget-is-hovered
   #:widget-is-mouse-clicked
   #:widget-has-mouse-click-down
   #:spacing
   #:widget-disable-begin
   #:widget-disable-end
   ;; Text
   #:text
   #:text-colored
   #:text-wrap
   #:text-wrap-colored
   #:label
   #:label-colored
   #:label-wrap
   #:label-colored-wrap
   #:image
   #:image-color
   ;; Button
   #:button-text
   #:button-label
   #:button-color
   #:button-symbol
   #:button-image
   #:button-symbol-label
   #:button-symbol-text
   #:button-image-label
   #:button-image-text
   #:button-text-styled
   #:button-label-styled
   #:button-symbol-styled
   #:button-image-styled
   #:button-symbol-text-styled
   #:button-symbol-label-styled
   #:button-image-label-styled
   #:button-image-text-styled
   #:button-set-behavior
   #:button-push-behavior
   #:button-pop-behavior
   ;; Checkbox
   #:check-label
   #:check-text
   #:check-text-align
   #:check-flags-label
   #:check-flags-text
   #:checkbox-label
   #:checkbox-label-align
   #:checkbox-text
   #:checkbox-text-align
   #:checkbox-flags-label
   #:checkbox-flags-text
   ;; Radio button
   #:radio-label
   #:radio-label-align
   #:radio-text
   #:radio-text-align
   #:option-label
   #:option-label-align
   #:option-text
   #:option-text-align
   ;; Selectable
   #:selectable-label
   #:selectable-text
   #:selectable-image-label
   #:selectable-image-text
   #:selectable-symbol-label
   #:selectable-symbol-text
   #:select-label
   #:select-text
   #:select-image-label
   #:select-image-text
   #:select-symbol-label
   #:select-symbol-text
   ;; Slider
   #:slide-float
   #:slide-int
   #:slider-float
   #:slider-int
   ;; Progressbar
   #:progress
   #:prog-
   ;; Color picker
   #:colorf nk-color-picker
   #:color-pick
   ;; Properties
   #:property-int
   #:property-float
   #:property-double
   #:propertyi
   #:propertyf
   #:propertyd
   ;; Text edit
   #:edit-string
   #:edit-string-zero-terminated
   #:edit-buffer
   #:edit-focus
   #:edit-unfocus
   ;; Chart
   #:chart-begin
   #:chart-begin-colored
   #:chart-add-slot
   #:chart-add-slot-colored
   #:chart-push
   #:chart-push-slot
   #:chart-end
   #:plot
   #:plot-function
   ;; Popup
   #:popup-begin
   #:popup-close
   #:popup-end
   #:popup-get-scroll
   #:popup-set-scroll
   ;; Combobox
   #:combo
   #:combo-separator
   #:combo-string
   #:combo-callback
   #:combobox
   #:combobox-string
   #:combobox-separator
   #:combobox-callback
   ;; Abstract combox
   #:combo-begin-text
   #:combo-begin-label
   #:combo-begin-color
   #:combo-begin-symbol
   #:combo-begin-symbol-label
   #:combo-begin-symbol-text
   #:combo-begin-image
   #:combo-begin-image-label
   #:combo-begin-image-text
   #:combo-item-label
   #:combo-item-text
   #:combo-item-image-label
   #:combo-item-image-text
   #:combo-item-symbol-label
   #:combo-item-symbol-text
   #:combo-close
   #:combo-end
   ;; Contextual
   #:contextual-begin
   #:contextual-item-text
   #:contextual-item-label
   #:contextual-item-image-label
   #:contextual-item-image-text
   #:contextual-item-symbol-label
   #:contextual-item-symbol-text
   #:contextual-close
   #:contextual-end
   ;; Tooltip
   #:tooltip
   #:tooltip-begin
   #:tooltip-end
   ;; Menu
   #:menubar-begin
   #:menubar-end
   #:menu-begin-text
   #:menu-begin-label
   #:menu-begin-image
   #:menu-begin-image-text
   #:menu-begin-image-label
   #:menu-begin-symbol
   #:menu-begin-symbol-text
   #:menu-begin-symbol-label
   #:menu-item-text
   #:menu-item-label
   #:menu-item-image-label
   #:menu-item-image-text
   #:menu-item-symbol-text
   #:menu-item-symbol-label
   #:menu-close
   #:menu-end
   ;; Style
   #:style-default
   #:style-from-table
   #:style-load-cursor
   #:style-load-all-cursors
   #:style-get-color-by-name
   #:style-set-font
   #:style-set-cursor
   #:style-show-cursor
   #:style-hide-cursor
   #:style-push-font
   #:style-push-float
   #:style-push-vec-2
   #:style-push-style-item
   #:style-push-flags
   #:style-push-color
   #:style-pop-font
   #:style-pop-float
   #:style-pop-vec-2
   #:style-pop-style-item
   #:style-pop-flags
   #:style-pop-color
   ;; Color
   #:rgb
   #:rgb-iv
   #:rgb-bv
   #:rgb-f
   #:rgb-fv
   #:rgb-cf
   #:rgb-hex
   #:rgb-factor
   #:rgba
   #:rgba-u32
   #:rgba-iv
   #:rgba-bv
   #:rgba-f
   #:rgba-fv
   #:rgba-cf
   #:rgba-hex
   #:hsva-colorf
   #:hsva-colorfv
   #:colorf-hsva-f
   #:colorf-hsva-fv
   #:hsv
   #:hsv-iv
   #:hsv-bv
   #:hsv-f
   #:hsv-fv
   #:hsva
   #:hsva-iv
   #:hsva-bv
   #:hsva-f
   #:hsva-fv
   #:color-f
   #:color-fv
   #:color-cf
   #:color-d
   #:color-dv
   #:color-u32
   #:color-hex-rgba
   #:color-hex-rgb
   #:color-hsv-i
   #:color-hsv-b
   #:color-hsv-iv
   #:color-hsv-bv
   #:color-hsv-f
   #:color-hsv-fv
   #:color-hsva-i
   #:color-hsva-b
   #:color-hsva-iv
   #:color-hsva-bv
   #:color-hsva-f
   #:color-hsva-fv
   ;; Image
   #:handle-ptr
   #:handle-id
   #:image-handle
   #:image-ptr
   #:image-id
   #:image-is-subimage
   #:subimage-ptr
   #:subimage-id
   #:subimage-handle
   ;; Math
   #:murmur-hash
   #:triangle-from-direction
   #:vec-2i
   #:vec-2v
   #:vec-2iv
   #:get-null-rect
   #:recti
   #:recta
   #:rectv
   #:rectiv
   #:rect-pos
   #:rect-size
   ;; String
   #:strfilter
   #:strmatch-fuzzy-string
   #:strmatch-fuzzy-text
   ;; UTF-8
   #:utf-decode
   #:utf-encode
   #:utf-len
   #:utf-at
   ;; Text editor
   #:filter-default
   #:filter-ascii
   #:filter-float
   #:filter-decimal
   #:filter-hex
   #:filter-oct
   #:filter-binary
   #:textedit-init-default
   #:textedit-init
   #:textedit-init-fixed
   #:textedit-free
   #:textedit-text
   #:textedit-delete
   #:textedit-delete-selection
   #:textedit-select-all
   #:textedit-cut
   #:textedit-paste
   #:textedit-undo
   #:textedit-redo
   ;; Drawing
   #:stroke-line
   #:stroke-curve
   #:stroke-rect
   #:stroke-circle
   #:stroke-arc
   #:stroke-triangle
   #:stroke-polyline
   #:stroke-polygon
   #:fill-rect
   #:fill-rect-multi-color
   #:fill-circle
   #:fill-arc
   #:fill-triangle
   #:fill-polygon
   #:draw-image
   #:draw-nine-slice
   #:draw-text
   #:push-scissor
   #:push-custom
   ;; Input
   #:input-has-mouse-click
   #:input-has-mouse-click-in-rect
   #:input-has-mouse-click-in-button-rect
   #:input-has-mouse-click-down-in-rect
   #:input-is-mouse-click-in-rect
   #:input-is-mouse-click-down-in-rect
   #:input-any-mouse-click-in-rect
   #:input-is-mouse-prev-hovering-rect
   #:input-is-mouse-hovering-rect
   #:input-mouse-clicked
   #:input-is-mouse-down
   #:input-is-mouse-pressed
   #:input-is-mouse-released
   #:input-is-key-pressed
   #:input-is-key-released
   #:input-is-key-down
   ;; GUI
   #:style-item-image
   #:style-item-nine-slice
   #:style-item-color
   #:style-item-hide
   ;; Allegro backend
   #:allegro-init
   #:allegro-handle-event
   #:allegro-shutdown
   #:allegro-render
   #:allegro-create-image
   #:allegro-del-image
   #:allegro-font-create-from-file
   #:allegro-font-del
   #:allegro-font-set-font
   #:allegro-setup-assert
   ;; Helpers
   #:flags
   ;; Lispy interface
   #:with-color
   #:with-colors
   #:with-rect
   #:with-rects
   #:with-window
   #:with-button-label
   #:with-button-label*
   #:with-layout-space
   #:with-style-item
   #:with-style-items
   #:with-style-color
   #:with-style-color*
   #:with-style-colors
   #:with-style-colors*
   #:with-style-font
   #:with-input
   #:with-styles
   #:style-item-image*
   #:vec-2*
   ;; Offsets
   #:+style-text-color+
   #:+style-text-padding+
   #:+style-button-normal+
   #:+style-button-hover+
   #:+style-button-active+
   #:+style-button-border-color+
   #:+style-button-text-background+
   #:+style-button-text-normal+
   #:+style-button-text-hover+
   #:+style-button-text-active+
   #:+style-button-text-alignment+
   #:+style-button-border+
   #:+style-button-rounding+
   #:+style-button-padding+
   #:+style-button-image-padding+
   #:+style-button-touch-padding+
   #:+style-contextual-button-normal+
   #:+style-contextual-button-hover+
   #:+style-contextual-button-active+
   #:+style-contextual-button-border-color+
   #:+style-contextual-button-text-background+
   #:+style-contextual-button-text-normal+
   #:+style-contextual-button-text-hover+
   #:+style-contextual-button-text-active+
   #:+style-contextual-button-text-alignment+
   #:+style-contextual-button-border+
   #:+style-contextual-button-rounding+
   #:+style-contextual-button-padding+
   #:+style-contextual-button-image-padding+
   #:+style-contextual-button-touch-padding+
   #:+style-menu-button-normal+
   #:+style-menu-button-hover+
   #:+style-menu-button-active+
   #:+style-menu-button-border-color+
   #:+style-menu-button-text-background+
   #:+style-menu-button-text-normal+
   #:+style-menu-button-text-hover+
   #:+style-menu-button-text-active+
   #:+style-menu-button-text-alignment+
   #:+style-menu-button-border+
   #:+style-menu-button-rounding+
   #:+style-menu-button-padding+
   #:+style-menu-button-image-padding+
   #:+style-menu-button-touch-padding+
   #:+style-option-normal+
   #:+style-option-hover+
   #:+style-option-active+
   #:+style-option-border-color+
   #:+style-option-cursor-normal+
   #:+style-option-cursor-hover+
   #:+style-option-text-normal+
   #:+style-option-text-hover+
   #:+style-option-text-active+
   #:+style-option-text-background+
   #:+style-option-text-alignment+
   #:+style-option-padding+
   #:+style-option-touch-padding+
   #:+style-option-spacing+
   #:+style-option-border+
   #:+style-checkbox-normal+
   #:+style-checkbox-hover+
   #:+style-checkbox-active+
   #:+style-checkbox-border-color+
   #:+style-checkbox-cursor-normal+
   #:+style-checkbox-cursor-hover+
   #:+style-checkbox-text-normal+
   #:+style-checkbox-text-hover+
   #:+style-checkbox-text-active+
   #:+style-checkbox-text-background+
   #:+style-checkbox-text-alignment+
   #:+style-checkbox-padding+
   #:+style-checkbox-touch-padding+
   #:+style-checkbox-spacing+
   #:+style-checkbox-border+
   #:+style-selectable-normal+
   #:+style-selectable-hover+
   #:+style-selectable-pressed+
   #:+style-selectable-normal-active+
   #:+style-selectable-hover-active+
   #:+style-selectable-pressed-active+
   #:+style-selectable-text-normal+
   #:+style-selectable-text-hover+
   #:+style-selectable-text-pressed+
   #:+style-selectable-text-normal-active+
   #:+style-selectable-text-hover-active+
   #:+style-selectable-text-pressed-active+
   #:+style-selectable-text-background+
   #:+style-selectable-text-alignment+
   #:+style-selectable-rounding+
   #:+style-selectable-padding+
   #:+style-selectable-touch-padding+
   #:+style-selectable-image-padding+
   #:+style-slider-normal+
   #:+style-slider-hover+
   #:+style-slider-active+
   #:+style-slider-border-color+
   #:+style-slider-bar-normal+
   #:+style-slider-bar-hover+
   #:+style-slider-bar-active+
   #:+style-slider-bar-filled+
   #:+style-slider-cursor-normal+
   #:+style-slider-cursor-hover+
   #:+style-slider-cursor-active+
   #:+style-slider-border+
   #:+style-slider-rounding+
   #:+style-slider-bar-height+
   #:+style-slider-padding+
   #:+style-slider-spacing+
   #:+style-slider-cursor-size+
   #:+style-slider-show-buttons+
   #:+style-slider-inc-button-normal+
   #:+style-slider-inc-button-hover+
   #:+style-slider-inc-button-active+
   #:+style-slider-inc-button-border-color+
   #:+style-slider-inc-button-text-background+
   #:+style-slider-inc-button-text-normal+
   #:+style-slider-inc-button-text-hover+
   #:+style-slider-inc-button-text-active+
   #:+style-slider-inc-button-text-alignment+
   #:+style-slider-inc-button-border+
   #:+style-slider-inc-button-rounding+
   #:+style-slider-inc-button-padding+
   #:+style-slider-inc-button-image-padding+
   #:+style-slider-inc-button-touch-padding+
   #:+style-slider-dec-button-normal+
   #:+style-slider-dec-button-hover+
   #:+style-slider-dec-button-active+
   #:+style-slider-dec-button-border-color+
   #:+style-slider-dec-button-text-background+
   #:+style-slider-dec-button-text-normal+
   #:+style-slider-dec-button-text-hover+
   #:+style-slider-dec-button-text-active+
   #:+style-slider-dec-button-text-alignment+
   #:+style-slider-dec-button-border+
   #:+style-slider-dec-button-rounding+
   #:+style-slider-dec-button-padding+
   #:+style-slider-dec-button-image-padding+
   #:+style-slider-dec-button-touch-padding+
   #:+style-slider-inc-symbol+
   #:+style-slider-dec-symbol+
   #:+style-progress-normal+
   #:+style-progress-hover+
   #:+style-progress-active+
   #:+style-progress-border-color+
   #:+style-progress-cursor-normal+
   #:+style-progress-cursor-hover+
   #:+style-progress-cursor-active+
   #:+style-progress-cursor-border-color+
   #:+style-progress-rounding+
   #:+style-progress-border+
   #:+style-progress-cursor-border+
   #:+style-progress-cursor-rounding+
   #:+style-progress-padding+
   #:+style-property-normal+
   #:+style-property-hover+
   #:+style-property-active+
   #:+style-property-border-color+
   #:+style-property-label-normal+
   #:+style-property-label-hover+
   #:+style-property-label-active+
   #:+style-property-sym-left+
   #:+style-property-sym-right+
   #:+style-property-border+
   #:+style-property-rounding+
   #:+style-property-padding+
   #:+style-property-edit-normal+
   #:+style-property-edit-hover+
   #:+style-property-edit-active+
   #:+style-property-edit-border-color+
   #:+style-property-edit-border-color+
   #:+style-property-edit-scrollbar-normal+
   #:+style-property-edit-scrollbar-hover+
   #:+style-property-edit-scrollbar-active+
   #:+style-property-edit-scrollbar-border-color+
   #:+style-property-edit-scrollbar-cursor-normal+
   #:+style-property-edit-scrollbar-cursor-hover+
   #:+style-property-edit-scrollbar-cursor-active+
   #:+style-property-edit-scrollbar-cursor-border-color+
   #:+style-property-edit-scrollbar-border+
   #:+style-property-edit-scrollbar-rounding+
   #:+style-property-edit-scrollbar-border-cursor+
   #:+style-property-edit-scrollbar-padding+
   #:+style-property-edit-scrollbar-show-buttons+
   #:+style-property-edit-scrollbar-inc-button-normal+
   #:+style-property-edit-scrollbar-inc-button-hover+
   #:+style-property-edit-scrollbar-inc-button-active+
   #:+style-property-edit-scrollbar-inc-button-border-color+
   #:+style-property-edit-scrollbar-inc-button-text-background+
   #:+style-property-edit-scrollbar-inc-button-text-normal+
   #:+style-property-edit-scrollbar-inc-button-text-hover+
   #:+style-property-edit-scrollbar-inc-button-text-active+
   #:+style-property-edit-scrollbar-inc-button-text-alignment+
   #:+style-property-edit-scrollbar-inc-button-border+
   #:+style-property-edit-scrollbar-inc-button-rounding+
   #:+style-property-edit-scrollbar-inc-button-padding+
   #:+style-property-edit-scrollbar-inc-button-image-padding+
   #:+style-property-edit-scrollbar-inc-button-touch-padding+
   #:+style-property-edit-scrollbar-dec-button-normal+
   #:+style-property-edit-scrollbar-dec-button-hover+
   #:+style-property-edit-scrollbar-dec-button-active+
   #:+style-property-edit-scrollbar-dec-button-border-color+
   #:+style-property-edit-scrollbar-dec-button-text-background+
   #:+style-property-edit-scrollbar-dec-button-text-normal+
   #:+style-property-edit-scrollbar-dec-button-text-hover+
   #:+style-property-edit-scrollbar-dec-button-text-active+
   #:+style-property-edit-scrollbar-dec-button-text-alignment+
   #:+style-property-edit-scrollbar-dec-button-border+
   #:+style-property-edit-scrollbar-dec-button-rounding+
   #:+style-property-edit-scrollbar-dec-button-padding+
   #:+style-property-edit-scrollbar-dec-button-image-padding+
   #:+style-property-edit-scrollbar-dec-button-touch-padding+
   #:+style-property-edit-scrollbar-inc-symbol+
   #:+style-property-edit-scrollbar-dec-symbol+
   #:+style-property-edit-cursor-normal+
   #:+style-property-edit-cursor-hover+
   #:+style-property-edit-cursor-text-normal+
   #:+style-property-edit-cursor-text-hover+
   #:+style-property-edit-text-normal+
   #:+style-property-edit-text-hover+
   #:+style-property-edit-text-active+
   #:+style-property-edit-selected-normal+
   #:+style-property-edit-selected-hover+
   #:+style-property-edit-selected-text-normal+
   #:+style-property-edit-selected-text-hover+
   #:+style-property-edit-border+
   #:+style-property-edit-rounding+
   #:+style-property-edit-cursor-size+
   #:+style-property-edit-scrollbar-size+
   #:+style-property-edit-padding+
   #:+style-property-edit-row-padding+
   #:+style-property-inc-button-normal+
   #:+style-property-inc-button-hover+
   #:+style-property-inc-button-active+
   #:+style-property-inc-button-border-color+
   #:+style-property-inc-button-text-background+
   #:+style-property-inc-button-text-normal+
   #:+style-property-inc-button-text-hover+
   #:+style-property-inc-button-text-active+
   #:+style-property-inc-button-text-alignment+
   #:+style-property-inc-button-border+
   #:+style-property-inc-button-rounding+
   #:+style-property-inc-button-padding+
   #:+style-property-inc-button-image-padding+
   #:+style-property-inc-button-touch-padding+
   #:+style-property-dec-button-normal+
   #:+style-property-dec-button-hover+
   #:+style-property-dec-button-active+
   #:+style-property-dec-button-border-color+
   #:+style-property-dec-button-text-background+
   #:+style-property-dec-button-text-normal+
   #:+style-property-dec-button-text-hover+
   #:+style-property-dec-button-text-active+
   #:+style-property-dec-button-text-alignment+
   #:+style-property-dec-button-border+
   #:+style-property-dec-button-rounding+
   #:+style-property-dec-button-padding+
   #:+style-property-dec-button-image-padding+
   #:+style-property-dec-button-touch-padding+
   #:+style-edit-normal+
   #:+style-edit-hover+
   #:+style-edit-active+
   #:+style-edit-border-color+
   #:+style-edit-scrollbar-normal+
   #:+style-edit-scrollbar-hover+
   #:+style-edit-scrollbar-active+
   #:+style-edit-scrollbar-border-color+
   #:+style-edit-scrollbar-cursor-normal+
   #:+style-edit-scrollbar-cursor-hover+
   #:+style-edit-scrollbar-cursor-active+
   #:+style-edit-scrollbar-cursor-border-color+
   #:+style-edit-scrollbar-border+
   #:+style-edit-scrollbar-rounding+
   #:+style-edit-scrollbar-border-cursor+
   #:+style-edit-scrollbar-padding+
   #:+style-edit-scrollbar-show-buttons+
   #:+style-edit-scrollbar-inc-button-normal+
   #:+style-edit-scrollbar-inc-button-hover+
   #:+style-edit-scrollbar-inc-button-active+
   #:+style-edit-scrollbar-inc-button-border-color+
   #:+style-edit-scrollbar-inc-button-text-background+
   #:+style-edit-scrollbar-inc-button-text-normal+
   #:+style-edit-scrollbar-inc-button-text-hover+
   #:+style-edit-scrollbar-inc-button-text-active+
   #:+style-edit-scrollbar-inc-button-text-alignment+
   #:+style-edit-scrollbar-inc-button-border+
   #:+style-edit-scrollbar-inc-button-rounding+
   #:+style-edit-scrollbar-inc-button-padding+
   #:+style-edit-scrollbar-inc-button-image-padding+
   #:+style-edit-scrollbar-inc-button-touch-padding+
   #:+style-edit-scrollbar-dec-button-normal+
   #:+style-edit-scrollbar-dec-button-hover+
   #:+style-edit-scrollbar-dec-button-active+
   #:+style-edit-scrollbar-dec-button-border-color+
   #:+style-edit-scrollbar-dec-button-text-background+
   #:+style-edit-scrollbar-dec-button-text-normal+
   #:+style-edit-scrollbar-dec-button-text-hover+
   #:+style-edit-scrollbar-dec-button-text-active+
   #:+style-edit-scrollbar-dec-button-text-alignment+
   #:+style-edit-scrollbar-dec-button-border+
   #:+style-edit-scrollbar-dec-button-rounding+
   #:+style-edit-scrollbar-dec-button-padding+
   #:+style-edit-scrollbar-dec-button-image-padding+
   #:+style-edit-scrollbar-dec-button-touch-padding+
   #:+style-edit-scrollbar-inc-symbol+
   #:+style-edit-scrollbar-dec-symbol+
   #:+style-edit-cursor-normal+
   #:+style-edit-cursor-hover+
   #:+style-edit-cursor-text-normal+
   #:+style-edit-cursor-text-hover+
   #:+style-edit-text-normal+
   #:+style-edit-text-hover+
   #:+style-edit-text-active+
   #:+style-edit-selected-normal+
   #:+style-edit-selected-hover+
   #:+style-edit-selected-text-normal+
   #:+style-edit-selected-text-hover+
   #:+style-edit-border+
   #:+style-edit-rounding+
   #:+style-edit-cursor-size+
   #:+style-edit-scrollbar-size+
   #:+style-edit-padding+
   #:+style-edit-row-padding+
   #:+style-chart-background+
   #:+style-chart-border-color+
   #:+style-chart-selected-color+
   #:+style-chart-color+
   #:+style-chart-border+
   #:+style-chart-rounding+
   #:+style-chart-padding+
   #:+style-scrollh-normal+
   #:+style-scrollh-hover+
   #:+style-scrollh-active+
   #:+style-scrollh-border-color+
   #:+style-scrollh-cursor-normal+
   #:+style-scrollh-cursor-hover+
   #:+style-scrollh-cursor-active+
   #:+style-scrollh-cursor-border-color+
   #:+style-scrollh-border+
   #:+style-scrollh-rounding+
   #:+style-scrollh-border-cursor+
   #:+style-scrollh-padding+
   #:+style-scrollh-show-buttons+
   #:+style-scrollh-inc-button-normal+
   #:+style-scrollh-inc-button-hover+
   #:+style-scrollh-inc-button-active+
   #:+style-scrollh-inc-button-border-color+
   #:+style-scrollh-inc-button-text-background+
   #:+style-scrollh-inc-button-text-normal+
   #:+style-scrollh-inc-button-text-hover+
   #:+style-scrollh-inc-button-text-active+
   #:+style-scrollh-inc-button-text-alignment+
   #:+style-scrollh-inc-button-border+
   #:+style-scrollh-inc-button-rounding+
   #:+style-scrollh-inc-button-padding+
   #:+style-scrollh-inc-button-image-padding+
   #:+style-scrollh-inc-button-touch-padding+
   #:+style-scrollh-dec-button-normal+
   #:+style-scrollh-dec-button-hover+
   #:+style-scrollh-dec-button-active+
   #:+style-scrollh-dec-button-border-color+
   #:+style-scrollh-dec-button-text-background+
   #:+style-scrollh-dec-button-text-normal+
   #:+style-scrollh-dec-button-text-hover+
   #:+style-scrollh-dec-button-text-active+
   #:+style-scrollh-dec-button-text-alignment+
   #:+style-scrollh-dec-button-border+
   #:+style-scrollh-dec-button-rounding+
   #:+style-scrollh-dec-button-padding+
   #:+style-scrollh-dec-button-image-padding+
   #:+style-scrollh-dec-button-touch-padding+
   #:+style-scrollh-inc-symbol+
   #:+style-scrollh-dec-symbol+
   #:+style-scrollv-normal+
   #:+style-scrollv-hover+
   #:+style-scrollv-active+
   #:+style-scrollv-border-color+
   #:+style-scrollv-cursor-normal+
   #:+style-scrollv-cursor-hover+
   #:+style-scrollv-cursor-active+
   #:+style-scrollv-cursor-border-color+
   #:+style-scrollv-border+
   #:+style-scrollv-rounding+
   #:+style-scrollv-border-cursor+
   #:+style-scrollv-padding+
   #:+style-scrollv-show-buttons+
   #:+style-scrollv-inc-button-normal+
   #:+style-scrollv-inc-button-hover+
   #:+style-scrollv-inc-button-active+
   #:+style-scrollv-inc-button-border-color+
   #:+style-scrollv-inc-button-text-background+
   #:+style-scrollv-inc-button-text-normal+
   #:+style-scrollv-inc-button-text-hover+
   #:+style-scrollv-inc-button-text-active+
   #:+style-scrollv-inc-button-text-alignment+
   #:+style-scrollv-inc-button-border+
   #:+style-scrollv-inc-button-rounding+
   #:+style-scrollv-inc-button-padding+
   #:+style-scrollv-inc-button-image-padding+
   #:+style-scrollv-inc-button-touch-padding+
   #:+style-scrollv-dec-button-normal+
   #:+style-scrollv-dec-button-hover+
   #:+style-scrollv-dec-button-active+
   #:+style-scrollv-dec-button-border-color+
   #:+style-scrollv-dec-button-text-background+
   #:+style-scrollv-dec-button-text-normal+
   #:+style-scrollv-dec-button-text-hover+
   #:+style-scrollv-dec-button-text-active+
   #:+style-scrollv-dec-button-text-alignment+
   #:+style-scrollv-dec-button-border+
   #:+style-scrollv-dec-button-rounding+
   #:+style-scrollv-dec-button-padding+
   #:+style-scrollv-dec-button-image-padding+
   #:+style-scrollv-dec-button-touch-padding+
   #:+style-scrollv-inc-symbol+
   #:+style-scrollv-dec-symbol+
   #:+style-tab-background+
   #:+style-tab-border-color+
   #:+style-tab-text+
   #:+style-tab-tab-maximize-button-normal+
   #:+style-tab-tab-maximize-button-hover+
   #:+style-tab-tab-maximize-button-active+
   #:+style-tab-tab-maximize-button-border-color+
   #:+style-tab-tab-maximize-button-text-background+
   #:+style-tab-tab-maximize-button-text-normal+
   #:+style-tab-tab-maximize-button-text-hover+
   #:+style-tab-tab-maximize-button-text-active+
   #:+style-tab-tab-maximize-button-text-alignment+
   #:+style-tab-tab-maximize-button-border+
   #:+style-tab-tab-maximize-button-rounding+
   #:+style-tab-tab-maximize-button-padding+
   #:+style-tab-tab-maximize-button-image-padding+
   #:+style-tab-tab-maximize-button-touch-padding+
   #:+style-tab-tab-minimize-button-normal+
   #:+style-tab-tab-minimize-button-hover+
   #:+style-tab-tab-minimize-button-active+
   #:+style-tab-tab-minimize-button-border-color+
   #:+style-tab-tab-minimize-button-text-background+
   #:+style-tab-tab-minimize-button-text-normal+
   #:+style-tab-tab-minimize-button-text-hover+
   #:+style-tab-tab-minimize-button-text-active+
   #:+style-tab-tab-minimize-button-text-alignment+
   #:+style-tab-tab-minimize-button-border+
   #:+style-tab-tab-minimize-button-rounding+
   #:+style-tab-tab-minimize-button-padding+
   #:+style-tab-tab-minimize-button-image-padding+
   #:+style-tab-tab-minimize-button-touch-padding+
   #:+style-tab-node-maximize-button-normal+
   #:+style-tab-node-maximize-button-hover+
   #:+style-tab-node-maximize-button-active+
   #:+style-tab-node-maximize-button-border-color+
   #:+style-tab-node-maximize-button-text-background+
   #:+style-tab-node-maximize-button-text-normal+
   #:+style-tab-node-maximize-button-text-hover+
   #:+style-tab-node-maximize-button-text-active+
   #:+style-tab-node-maximize-button-text-alignment+
   #:+style-tab-node-maximize-button-border+
   #:+style-tab-node-maximize-button-rounding+
   #:+style-tab-node-maximize-button-padding+
   #:+style-tab-node-maximize-button-image-padding+
   #:+style-tab-node-maximize-button-touch-padding+
   #:+style-tab-node-minimize-button-normal+
   #:+style-tab-node-minimize-button-hover+
   #:+style-tab-node-minimize-button-active+
   #:+style-tab-node-minimize-button-border-color+
   #:+style-tab-node-minimize-button-text-background+
   #:+style-tab-node-minimize-button-text-normal+
   #:+style-tab-node-minimize-button-text-hover+
   #:+style-tab-node-minimize-button-text-active+
   #:+style-tab-node-minimize-button-text-alignment+
   #:+style-tab-node-minimize-button-border+
   #:+style-tab-node-minimize-button-rounding+
   #:+style-tab-node-minimize-button-padding+
   #:+style-tab-node-minimize-button-image-padding+
   #:+style-tab-node-minimize-button-touch-padding+
   #:+style-tab-sym-minimize+
   #:+style-tab-sym-maximize+
   #:+style-tab-border+
   #:+style-tab-rounding+
   #:+style-tab-indent+
   #:+style-tab-padding+
   #:+style-tab-spacing+
   #:+style-combo-normal+
   #:+style-combo-hover+
   #:+style-combo-active+
   #:+style-combo-border-color+
   #:+style-combo-label-normal+
   #:+style-combo-label-hover+
   #:+style-combo-label-active+
   #:+style-combo-symbol-normal+
   #:+style-combo-symbol-hover+
   #:+style-combo-symbol-active+
   #:+style-combo-button-normal+
   #:+style-combo-button-hover+
   #:+style-combo-button-active+
   #:+style-combo-button-border-color+
   #:+style-combo-button-text-background+
   #:+style-combo-button-text-normal+
   #:+style-combo-button-text-hover+
   #:+style-combo-button-text-active+
   #:+style-combo-button-text-alignment+
   #:+style-combo-button-border+
   #:+style-combo-button-rounding+
   #:+style-combo-button-padding+
   #:+style-combo-button-image-padding+
   #:+style-combo-button-touch-padding+
   #:+style-combo-sym-normal+
   #:+style-combo-sym-hover+
   #:+style-combo-sym-active+
   #:+style-combo-border+
   #:+style-combo-rounding+
   #:+style-combo-content-padding+
   #:+style-combo-button-padding+
   #:+style-combo-spacing+
   #:+style-window-header-normal+
   #:+style-window-header-hover+
   #:+style-window-header-active+
   #:+style-window-header-close-button-normal+
   #:+style-window-header-close-button-hover+
   #:+style-window-header-close-button-active+
   #:+style-window-header-close-button-border-color+
   #:+style-window-header-close-button-text-background+
   #:+style-window-header-close-button-text-normal+
   #:+style-window-header-close-button-text-hover+
   #:+style-window-header-close-button-text-active+
   #:+style-window-header-close-button-text-alignment+
   #:+style-window-header-close-button-border+
   #:+style-window-header-close-button-rounding+
   #:+style-window-header-close-button-padding+
   #:+style-window-header-close-button-image-padding+
   #:+style-window-header-close-button-touch-padding+
   #:+style-window-header-minimize-button-normal+
   #:+style-window-header-minimize-button-hover+
   #:+style-window-header-minimize-button-active+
   #:+style-window-header-minimize-button-border-color+
   #:+style-window-header-minimize-button-text-background+
   #:+style-window-header-minimize-button-text-normal+
   #:+style-window-header-minimize-button-text-hover+
   #:+style-window-header-minimize-button-text-active+
   #:+style-window-header-minimize-button-text-alignment+
   #:+style-window-header-minimize-button-border+
   #:+style-window-header-minimize-button-rounding+
   #:+style-window-header-minimize-button-padding+
   #:+style-window-header-minimize-button-image-padding+
   #:+style-window-header-minimize-button-touch-padding+
   #:+style-window-header-close-symbol+
   #:+style-window-header-minimize-symbol+
   #:+style-window-header-maximize-symbol+
   #:+style-window-header-label-normal+
   #:+style-window-header-label-hover+
   #:+style-window-header-label-active+
   #:+style-window-header-align+
   #:+style-window-header-padding+
   #:+style-window-header-label-padding+
   #:+style-window-header-spacing+
   #:+style-window-fixed-background+
   #:+style-window-background+
   #:+style-window-border-color+
   #:+style-window-popup-border-color+
   #:+style-window-combo-border-color+
   #:+style-window-contextual-border-color+
   #:+style-window-menu-border-color+
   #:+style-window-group-border-color+
   #:+style-window-tooltip-border-color+
   #:+style-window-scaler+
   #:+style-window-border+
   #:+style-window-combo-border+
   #:+style-window-contextual-border+
   #:+style-window-menu-border+
   #:+style-window-group-border+
   #:+style-window-tooltip-border+
   #:+style-window-popup-border+
   #:+style-window-min-row-height-padding+
   #:+style-window-rounding+
   #:+style-window-spacing+
   #:+style-window-scrollbar-size+
   #:+style-window-padding+
   #:+style-window-group-padding+
   #:+style-window-popup-padding+
   #:+style-window-combo-padding+
   #:+style-window-contextual-padding+
   #:+style-window-menu-padding+
   #:+style-window-tooltip-padding+))
