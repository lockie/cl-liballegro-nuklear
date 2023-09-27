%module interface

%feature("intern_function", "1");

%insert("lisphead") %{
(in-package :cl-liballegro-nuklear)

(defun flags (type &rest flags)
  (let ((flag-type (intern (string-upcase type) :nk)))
    (apply #'logior
      (mapcar
        #'(lambda (flag) (cffi:foreign-enum-value flag-type flag))
        flags))))

(cffi:defcstruct color
  (r :unsigned-char)
  (g :unsigned-char)
  (b :unsigned-char)
  (a :unsigned-char))

(cffi:defcstruct vec-2
  (x :float)
  (y :float))

(cffi:defcstruct rect
  (x :float)
  (y :float)
  (w :float)
  (h :float))

(cffi:defcstruct image
  (handle :pointer)
  (w :unsigned-short)
  (h :unsigned-short)
  (region :uint64))

(defmethod cffi:translate-into-foreign-memory (object (type image) pointer)
  (if (cffi:pointerp object) ;; allow passing result of nk:allegro-create-image as image struct
      (let (handle* w* h* region*)
        (cffi:with-foreign-slots ((handle w h region) object (:struct image))
          (setf handle* handle
                w* w
                h* h
                region* region))
        (cffi:with-foreign-slots ((handle w h region) pointer (:struct image))
          (setf handle handle*
                w w*
                h h*
                region region*)))
      (call-next-method)))

(defmacro tree-push (ctx type title state)
  (let ((hash (string (gensym "nk-tree-hash"))))
    `(tree-push-hashed ,ctx ,type ,title ,state ,hash ,(length hash) 0)))

(cffi:defcfun ("nk_rect" rect) (:struct rect)
  (x :float)
  (y :float)
  (w :float)
  (h :float))

(cffi:defcfun ("nk_image" image) :void
  (arg0 :pointer)
  (arg1 (:struct image)))
%}

typedef unsigned char nk_byte;
typedef unsigned long nk_size;
typedef unsigned int nk_uint;

typedef nk_uint nk_hash;
typedef nk_uint nk_flags;
typedef nk_uint nk_rune;

enum {nk_false, nk_true};
struct nk_color {nk_byte r,g,b,a;};
struct nk_colorf {float r,g,b,a;};
struct nk_vec2 {float x,y;};
struct nk_rect {float x,y,w,h;};
typedef char nk_glyph[4];
typedef void* nk_handle;
struct nk_image {nk_handle handle;unsigned short w,h;unsigned short region[4];};
struct nk_nine_slice {struct nk_image img; unsigned short l, t, r, b;};
struct nk_scroll {nk_uint x, y;};

enum nk_heading         {NK_UP, NK_RIGHT, NK_DOWN, NK_LEFT};
enum nk_button_behavior {NK_BUTTON_DEFAULT, NK_BUTTON_REPEATER};
enum nk_modify          {NK_FIXED, NK_MODIFIABLE};
enum nk_chart_type      {NK_CHART_LINES, NK_CHART_COLUMN, NK_CHART_MAX};
enum nk_popup_type      {NK_POPUP_STATIC, NK_POPUP_DYNAMIC};

enum nk_symbol_type {
    NK_SYMBOL_NONE,
    NK_SYMBOL_X,
    NK_SYMBOL_UNDERSCORE,
    NK_SYMBOL_CIRCLE_SOLID,
    NK_SYMBOL_CIRCLE_OUTLINE,
    NK_SYMBOL_RECT_SOLID,
    NK_SYMBOL_RECT_OUTLINE,
    NK_SYMBOL_TRIANGLE_UP,
    NK_SYMBOL_TRIANGLE_DOWN,
    NK_SYMBOL_TRIANGLE_LEFT,
    NK_SYMBOL_TRIANGLE_RIGHT,
    NK_SYMBOL_PLUS,
    NK_SYMBOL_MINUS,
    NK_SYMBOL_MAX
};

enum nk_keys {
    NK_KEY_NONE,
    NK_KEY_SHIFT,
    NK_KEY_CTRL,
    NK_KEY_DEL,
    NK_KEY_ENTER,
    NK_KEY_TAB,
    NK_KEY_BACKSPACE,
    NK_KEY_COPY,
    NK_KEY_CUT,
    NK_KEY_PASTE,
    NK_KEY_UP,
    NK_KEY_DOWN,
    NK_KEY_LEFT,
    NK_KEY_RIGHT,
    /* Shortcuts: text field */
    NK_KEY_TEXT_INSERT_MODE,
    NK_KEY_TEXT_REPLACE_MODE,
    NK_KEY_TEXT_RESET_MODE,
    NK_KEY_TEXT_LINE_START,
    NK_KEY_TEXT_LINE_END,
    NK_KEY_TEXT_START,
    NK_KEY_TEXT_END,
    NK_KEY_TEXT_UNDO,
    NK_KEY_TEXT_REDO,
    NK_KEY_TEXT_SELECT_ALL,
    NK_KEY_TEXT_WORD_LEFT,
    NK_KEY_TEXT_WORD_RIGHT,
    /* Shortcuts: scrollbar */
    NK_KEY_SCROLL_START,
    NK_KEY_SCROLL_END,
    NK_KEY_SCROLL_DOWN,
    NK_KEY_SCROLL_UP,
    NK_KEY_MAX
};

enum nk_buttons {
    NK_BUTTON_LEFT,
    NK_BUTTON_MIDDLE,
    NK_BUTTON_RIGHT,
    NK_BUTTON_DOUBLE,
    NK_BUTTON_MAX
};

enum nk_collapse_states {NK_MINIMIZED = 0, NK_MAXIMIZED = 1};
enum nk_show_states     {NK_HIDDEN = 0, NK_SHOWN = 1};
enum nk_layout_format   {NK_DYNAMIC, NK_STATIC};
enum nk_tree_type       {NK_TREE_NODE, NK_TREE_TAB};

enum nk_panel_flags {
    NK_WINDOW_BORDER            = 1,
    NK_WINDOW_MOVABLE           = 2,
    NK_WINDOW_SCALABLE          = 4,
    NK_WINDOW_CLOSABLE          = 8,
    NK_WINDOW_MINIMIZABLE       = 16,
    NK_WINDOW_NO_SCROLLBAR      = 32,
    NK_WINDOW_TITLE             = 64,
    NK_WINDOW_SCROLL_AUTO_HIDE  = 128,
    NK_WINDOW_BACKGROUND        = 256,
    NK_WINDOW_SCALE_LEFT        = 512,
    NK_WINDOW_NO_INPUT          = 1024
};

enum nk_widget_layout_states {
    NK_WIDGET_INVALID, /* The widget cannot be seen and is completely out of view */
    NK_WIDGET_VALID, /* The widget is completely inside the window and can be updated and drawn */
    NK_WIDGET_ROM /* The widget is partially visible and cannot be updated */
};
enum nk_widget_states {
    NK_WIDGET_STATE_MODIFIED    = 2,
    NK_WIDGET_STATE_INACTIVE    = 4, /* widget is neither active nor hovered */
    NK_WIDGET_STATE_ENTERED     = 8, /* widget has been hovered on the current frame */
    NK_WIDGET_STATE_HOVER       = 16, /* widget is being hovered */
    NK_WIDGET_STATE_ACTIVED     = 32,/* widget is currently activated */
    NK_WIDGET_STATE_LEFT        = 64, /* widget is from this frame on not hovered anymore */
    NK_WIDGET_STATE_HOVERED     = 18, /* widget is being hovered */
    NK_WIDGET_STATE_ACTIVE      = 34 /* widget is currently activated */
};

enum nk_text_align {
    NK_TEXT_ALIGN_LEFT        = 0x01,
    NK_TEXT_ALIGN_CENTERED    = 0x02,
    NK_TEXT_ALIGN_RIGHT       = 0x04,
    NK_TEXT_ALIGN_TOP         = 0x08,
    NK_TEXT_ALIGN_MIDDLE      = 0x10,
    NK_TEXT_ALIGN_BOTTOM      = 0x20
};
enum nk_text_alignment {
    NK_TEXT_LEFT        = 17,
    NK_TEXT_CENTERED    = 18,
    NK_TEXT_RIGHT       = 20
};

enum nk_edit_flags {
    NK_EDIT_DEFAULT                 = 0,
    NK_EDIT_READ_ONLY               = 1,
    NK_EDIT_AUTO_SELECT             = 2,
    NK_EDIT_SIG_ENTER               = 4,
    NK_EDIT_ALLOW_TAB               = 8,
    NK_EDIT_NO_CURSOR               = 16,
    NK_EDIT_SELECTABLE              = 32,
    NK_EDIT_CLIPBOARD               = 64,
    NK_EDIT_CTRL_ENTER_NEWLINE      = 128,
    NK_EDIT_NO_HORIZONTAL_SCROLL    = 256,
    NK_EDIT_ALWAYS_INSERT_MODE      = 512,
    NK_EDIT_MULTILINE               = 1024,
    NK_EDIT_GOTO_END_ON_ACTIVATE    = 2048
};
enum nk_edit_types {
    NK_EDIT_SIMPLE  = 512,
    NK_EDIT_FIELD   = 608,
    NK_EDIT_BOX     = 1640,
    NK_EDIT_EDITOR  = 1128
};
enum nk_edit_events {
    NK_EDIT_ACTIVE      = 1, /* edit widget is currently being modified */
    NK_EDIT_INACTIVE    = 2, /* edit widget is not active and is not being modified */
    NK_EDIT_ACTIVATED   = 4, /* edit widget went from state inactive to state active */
    NK_EDIT_DEACTIVATED = 8, /* edit widget went from state active to state inactive */
    NK_EDIT_COMMITED    = 16 /* edit widget has received an enter and lost focus */
};

enum nk_style_colors {
    NK_COLOR_TEXT,
    NK_COLOR_WINDOW,
    NK_COLOR_HEADER,
    NK_COLOR_BORDER,
    NK_COLOR_BUTTON,
    NK_COLOR_BUTTON_HOVER,
    NK_COLOR_BUTTON_ACTIVE,
    NK_COLOR_TOGGLE,
    NK_COLOR_TOGGLE_HOVER,
    NK_COLOR_TOGGLE_CURSOR,
    NK_COLOR_SELECT,
    NK_COLOR_SELECT_ACTIVE,
    NK_COLOR_SLIDER,
    NK_COLOR_SLIDER_CURSOR,
    NK_COLOR_SLIDER_CURSOR_HOVER,
    NK_COLOR_SLIDER_CURSOR_ACTIVE,
    NK_COLOR_PROPERTY,
    NK_COLOR_EDIT,
    NK_COLOR_EDIT_CURSOR,
    NK_COLOR_COMBO,
    NK_COLOR_CHART,
    NK_COLOR_CHART_COLOR,
    NK_COLOR_CHART_COLOR_HIGHLIGHT,
    NK_COLOR_SCROLLBAR,
    NK_COLOR_SCROLLBAR_CURSOR,
    NK_COLOR_SCROLLBAR_CURSOR_HOVER,
    NK_COLOR_SCROLLBAR_CURSOR_ACTIVE,
    NK_COLOR_TAB_HEADER,
    NK_COLOR_COUNT
};
enum nk_style_cursor {
    NK_CURSOR_ARROW,
    NK_CURSOR_TEXT,
    NK_CURSOR_MOVE,
    NK_CURSOR_RESIZE_VERTICAL,
    NK_CURSOR_RESIZE_HORIZONTAL,
    NK_CURSOR_RESIZE_TOP_LEFT_DOWN_RIGHT,
    NK_CURSOR_RESIZE_TOP_RIGHT_DOWN_LEFT,
    NK_CURSOR_COUNT
};

enum nk_style_item_type {
    NK_STYLE_ITEM_COLOR,
    NK_STYLE_ITEM_IMAGE,
    NK_STYLE_ITEM_NINE_SLICE
};

union nk_style_item_data {
    struct nk_color color;
    struct nk_image image;
    struct nk_nine_slice slice;
};

struct nk_style_item {
    enum nk_style_item_type type;
    //union nk_style_item_data data;
    struct nk_nine_slice data;
};

// Context
int nk_init_default(struct nk_context*, const struct nk_user_font*);
int nk_init_fixed(struct nk_context*, void *memory, nk_size size, const struct nk_user_font*);
int nk_init(struct nk_context*, struct nk_allocator*, const struct nk_user_font*);
int nk_init_custom(struct nk_context*, struct nk_buffer *cmds, struct nk_buffer *pool, const struct nk_user_font*);
void nk_clear(struct nk_context*);
void nk_free(struct nk_context*);
// Input
void nk_input_begin(struct nk_context*);
void nk_input_motion(struct nk_context*, int x, int y);
void nk_input_key(struct nk_context*, enum nk_keys, int down);
void nk_input_button(struct nk_context*, enum nk_buttons, int x, int y, int down);
void nk_input_scroll(struct nk_context*, struct nk_vec2 val);
void nk_input_char(struct nk_context*, char);
void nk_input_glyph(struct nk_context*, const nk_glyph);
void nk_input_unicode(struct nk_context*, nk_rune);
void nk_input_end(struct nk_context*);
// Window
int nk_begin(struct nk_context *ctx, const char *title, struct nk_rect bounds, nk_flags flags);
int nk_begin_titled(struct nk_context *ctx, const char *name, const char *title, struct nk_rect bounds, nk_flags flags);
void nk_end(struct nk_context *ctx);
struct nk_window *nk_window_find(struct nk_context *ctx, const char *name);
struct nk_rect nk_window_get_bounds(const struct nk_context *ctx);
struct nk_vec2 nk_window_get_position(const struct nk_context *ctx);
struct nk_vec2 nk_window_get_size(const struct nk_context*);
float nk_window_get_width(const struct nk_context*);
float nk_window_get_height(const struct nk_context*);
struct nk_panel* nk_window_get_panel(struct nk_context*);
struct nk_rect nk_window_get_content_region(struct nk_context*);
struct nk_vec2 nk_window_get_content_region_min(struct nk_context*);
struct nk_vec2 nk_window_get_content_region_max(struct nk_context*);
struct nk_vec2 nk_window_get_content_region_size(struct nk_context*);
struct nk_command_buffer* nk_window_get_canvas(struct nk_context*);
void nk_window_get_scroll(struct nk_context*, nk_uint *offset_x, nk_uint *offset_y);
int nk_window_has_focus(const struct nk_context*);
int nk_window_is_hovered(struct nk_context*);
int nk_window_is_collapsed(struct nk_context *ctx, const char *name);
int nk_window_is_closed(struct nk_context*, const char*);
int nk_window_is_hidden(struct nk_context*, const char*);
int nk_window_is_active(struct nk_context*, const char*);
int nk_window_is_any_hovered(struct nk_context*);
int nk_item_is_any_active(struct nk_context*);
void nk_window_set_bounds(struct nk_context*, const char *name, struct nk_rect bounds);
void nk_window_set_position(struct nk_context*, const char *name, struct nk_vec2 pos);
void nk_window_set_size(struct nk_context*, const char *name, struct nk_vec2);
void nk_window_set_focus(struct nk_context*, const char *name);
void nk_window_set_scroll(struct nk_context*, nk_uint offset_x, nk_uint offset_y);
void nk_window_close(struct nk_context *ctx, const char *name);
void nk_window_collapse(struct nk_context*, const char *name, enum nk_collapse_states state);
void nk_window_collapse_if(struct nk_context*, const char *name, enum nk_collapse_states, int cond);
void nk_window_show(struct nk_context*, const char *name, enum nk_show_states);
void nk_window_show_if(struct nk_context*, const char *name, enum nk_show_states, int cond);
// Layout
void nk_layout_set_min_row_height(struct nk_context*, float height);
void nk_layout_reset_min_row_height(struct nk_context*);
struct nk_rect nk_layout_widget_bounds(struct nk_context*);
float nk_layout_ratio_from_pixel(struct nk_context*, float pixel_width);
void nk_layout_row_dynamic(struct nk_context *ctx, float height, int cols);
void nk_layout_row_static(struct nk_context *ctx, float height, int item_width, int cols);
void nk_layout_row_begin(struct nk_context *ctx, enum nk_layout_format fmt, float row_height, int cols);
void nk_layout_row_push(struct nk_context*, float value);
void nk_layout_row_end(struct nk_context*);
void nk_layout_row(struct nk_context*, enum nk_layout_format, float height, int cols, const float *ratio);
void nk_layout_row_template_begin(struct nk_context*, float row_height);
void nk_layout_row_template_push_dynamic(struct nk_context*);
void nk_layout_row_template_push_variable(struct nk_context*, float min_width);
void nk_layout_row_template_push_static(struct nk_context*, float width);
void nk_layout_row_template_end(struct nk_context*);
void nk_layout_space_begin(struct nk_context*, enum nk_layout_format, float height, int widget_count);
void nk_layout_space_push(struct nk_context*, struct nk_rect bounds);
void nk_layout_space_end(struct nk_context*);
struct nk_rect nk_layout_space_bounds(struct nk_context*);
struct nk_vec2 nk_layout_space_to_screen(struct nk_context*, struct nk_vec2);
struct nk_vec2 nk_layout_space_to_local(struct nk_context*, struct nk_vec2);
struct nk_rect nk_layout_space_rect_to_screen(struct nk_context*, struct nk_rect);
struct nk_rect nk_layout_space_rect_to_local(struct nk_context*, struct nk_rect);
void nk_spacer(struct nk_context*);
// Groups
int nk_group_begin(struct nk_context*, const char *title, nk_flags);
int nk_group_begin_titled(struct nk_context*, const char *name, const char *title, nk_flags);
void nk_group_end(struct nk_context*);
int nk_group_scrolled_offset_begin(struct nk_context*, nk_uint *x_offset, nk_uint *y_offset, const char *title, nk_flags flags);
int nk_group_scrolled_begin(struct nk_context*, struct nk_scroll *off, const char *title, nk_flags);
void nk_group_scrolled_end(struct nk_context*);
void nk_group_get_scroll(struct nk_context*, const char *id, nk_uint *x_offset, nk_uint *y_offset);
void nk_group_set_scroll(struct nk_context*, const char *id, nk_uint x_offset, nk_uint y_offset);
// Tree
int nk_tree_push_hashed(struct nk_context*, enum nk_tree_type, const char *title, enum nk_collapse_states initial_state, const char *hash, int len,int seed);
int nk_tree_image_push_hashed(struct nk_context*, enum nk_tree_type, struct nk_image, const char *title, enum nk_collapse_states initial_state, const char *hash, int len,int seed);
void nk_tree_pop(struct nk_context*);
int nk_tree_state_push(struct nk_context*, enum nk_tree_type, const char *title, enum nk_collapse_states *state);
int nk_tree_state_image_push(struct nk_context*, enum nk_tree_type, struct nk_image, const char *title, enum nk_collapse_states *state);
void nk_tree_state_pop(struct nk_context*);
// List view
int nk_list_view_begin(struct nk_context*, struct nk_list_view *out, const char *id, nk_flags, int row_height, int row_count);
void nk_list_view_end(struct nk_list_view*);
// Widget
enum nk_widget_layout_states nk_widget(struct nk_rect*, const struct nk_context*);
enum nk_widget_layout_states nk_widget_fitting(struct nk_rect*, struct nk_context*, struct nk_vec2);
struct nk_rect nk_widget_bounds(struct nk_context*);
struct nk_vec2 nk_widget_position(struct nk_context*);
struct nk_vec2 nk_widget_size(struct nk_context*);
float nk_widget_width(struct nk_context*);
float nk_widget_height(struct nk_context*);
int nk_widget_is_hovered(struct nk_context*);
int nk_widget_is_mouse_clicked(struct nk_context*, enum nk_buttons);
int nk_widget_has_mouse_click_down(struct nk_context*, enum nk_buttons, int down);
void nk_spacing(struct nk_context*, int cols);
// Text
void nk_text(struct nk_context*, const char*, int, nk_flags);
void nk_text_colored(struct nk_context*, const char*, int, nk_flags, struct nk_color);
void nk_text_wrap(struct nk_context*, const char*, int);
void nk_text_wrap_colored(struct nk_context*, const char*, int, struct nk_color);
void nk_label(struct nk_context*, const char*, nk_flags align);
void nk_label_colored(struct nk_context*, const char*, nk_flags align, struct nk_color);
void nk_label_wrap(struct nk_context*, const char*);
void nk_label_colored_wrap(struct nk_context*, const char*, struct nk_color);
void nk_image_color(struct nk_context*, struct nk_image, struct nk_color);
// Button
int nk_button_text(struct nk_context*, const char *title, int len);
int nk_button_label(struct nk_context*, const char *title);
int nk_button_color(struct nk_context*, struct nk_color);
int nk_button_symbol(struct nk_context*, enum nk_symbol_type);
int nk_button_image(struct nk_context*, struct nk_image img);
int nk_button_symbol_label(struct nk_context*, enum nk_symbol_type, const char*, nk_flags text_alignment);
int nk_button_symbol_text(struct nk_context*, enum nk_symbol_type, const char*, int, nk_flags alignment);
int nk_button_image_label(struct nk_context*, struct nk_image img, const char*, nk_flags text_alignment);
int nk_button_image_text(struct nk_context*, struct nk_image img, const char*, int, nk_flags alignment);
int nk_button_text_styled(struct nk_context*, const struct nk_style_button*, const char *title, int len);
int nk_button_label_styled(struct nk_context*, const struct nk_style_button*, const char *title);
int nk_button_symbol_styled(struct nk_context*, const struct nk_style_button*, enum nk_symbol_type);
int nk_button_image_styled(struct nk_context*, const struct nk_style_button*, struct nk_image img);
int nk_button_symbol_text_styled(struct nk_context*,const struct nk_style_button*, enum nk_symbol_type, const char*, int, nk_flags alignment);
int nk_button_symbol_label_styled(struct nk_context *ctx, const struct nk_style_button *style, enum nk_symbol_type symbol, const char *title, nk_flags align);
int nk_button_image_label_styled(struct nk_context*,const struct nk_style_button*, struct nk_image img, const char*, nk_flags text_alignment);
int nk_button_image_text_styled(struct nk_context*,const struct nk_style_button*, struct nk_image img, const char*, int, nk_flags alignment);
void nk_button_set_behavior(struct nk_context*, enum nk_button_behavior);
int nk_button_push_behavior(struct nk_context*, enum nk_button_behavior);
int nk_button_pop_behavior(struct nk_context*);
// XXX
// Checkbox
int nk_check_label(struct nk_context*, const char*, int active);
int nk_check_text(struct nk_context*, const char*, int,int active);
unsigned nk_check_flags_label(struct nk_context*, const char*, unsigned int flags, unsigned int value);
unsigned nk_check_flags_text(struct nk_context*, const char*, int, unsigned int flags, unsigned int value);
int nk_checkbox_label(struct nk_context*, const char*, int *active);
int nk_checkbox_text(struct nk_context*, const char*, int, int *active);
int nk_checkbox_flags_label(struct nk_context*, const char*, unsigned int *flags, unsigned int value);
int nk_checkbox_flags_text(struct nk_context*, const char*, int, unsigned int *flags, unsigned int value);
// Radio button
int nk_radio_label(struct nk_context*, const char*, int *active);
int nk_radio_text(struct nk_context*, const char*, int, int *active);
int nk_option_label(struct nk_context*, const char*, int active);
int nk_option_text(struct nk_context*, const char*, int, int active);
// Selectable
int nk_selectable_label(struct nk_context*, const char*, nk_flags align, int *value);
int nk_selectable_text(struct nk_context*, const char*, int, nk_flags align, int *value);
int nk_selectable_image_label(struct nk_context*,struct nk_image,  const char*, nk_flags align, int *value);
int nk_selectable_image_text(struct nk_context*,struct nk_image, const char*, int, nk_flags align, int *value);
int nk_selectable_symbol_label(struct nk_context*,enum nk_symbol_type,  const char*, nk_flags align, int *value);
int nk_selectable_symbol_text(struct nk_context*,enum nk_symbol_type, const char*, int, nk_flags align, int *value);
int nk_select_label(struct nk_context*, const char*, nk_flags align, int value);
int nk_select_text(struct nk_context*, const char*, int, nk_flags align, int value);
int nk_select_image_label(struct nk_context*, struct nk_image,const char*, nk_flags align, int value);
int nk_select_image_text(struct nk_context*, struct nk_image,const char*, int, nk_flags align, int value);
int nk_select_symbol_label(struct nk_context*,enum nk_symbol_type,  const char*, nk_flags align, int value);
int nk_select_symbol_text(struct nk_context*,enum nk_symbol_type, const char*, int, nk_flags align, int value);
// Slider
float nk_slide_float(struct nk_context*, float min, float val, float max, float step);
int nk_slide_int(struct nk_context*, int min, int val, int max, int step);
int nk_slider_float(struct nk_context*, float min, float *val, float max, float step);
int nk_slider_int(struct nk_context*, int min, int *val, int max, int step);
// Progressbar
int nk_progress(struct nk_context*, nk_size *cur, nk_size max, int modifyable);
nk_size nk_prog(struct nk_context*, nk_size cur, nk_size max, int modifyable);
// Color picker
struct nk_colorf nk_color_picker(struct nk_context*, struct nk_colorf, enum nk_color_format);
int nk_color_pick(struct nk_context*, struct nk_colorf*, enum nk_color_format);
// Properties
void nk_property_int(struct nk_context*, const char *name, int min, int *val, int max, int step, float inc_per_pixel);
void nk_property_float(struct nk_context*, const char *name, float min, float *val, float max, float step, float inc_per_pixel);
void nk_property_double(struct nk_context*, const char *name, double min, double *val, double max, double step, float inc_per_pixel);
int nk_propertyi(struct nk_context*, const char *name, int min, int val, int max, int step, float inc_per_pixel);
float nk_propertyf(struct nk_context*, const char *name, float min, float val, float max, float step, float inc_per_pixel);
double nk_propertyd(struct nk_context*, const char *name, double min, double val, double max, double step, float inc_per_pixel);
// XXX
// Text edit
nk_flags nk_edit_string(struct nk_context*, nk_flags, char *buffer, int *len, int max, nk_plugin_filter);
nk_flags nk_edit_string_zero_terminated(struct nk_context*, nk_flags, char *buffer, int max, nk_plugin_filter);
nk_flags nk_edit_buffer(struct nk_context*, nk_flags, struct nk_text_edit*, nk_plugin_filter);
void nk_edit_focus(struct nk_context*, nk_flags flags);
void nk_edit_unfocus(struct nk_context*);
// Chart
int nk_chart_begin(struct nk_context*, enum nk_chart_type, int num, float min, float max);
int nk_chart_begin_colored(struct nk_context*, enum nk_chart_type, struct nk_color, struct nk_color active, int num, float min, float max);
void nk_chart_add_slot(struct nk_context *ctx, const enum nk_chart_type, int count, float min_value, float max_value);
void nk_chart_add_slot_colored(struct nk_context *ctx, const enum nk_chart_type, struct nk_color, struct nk_color active, int count, float min_value, float max_value);
nk_flags nk_chart_push(struct nk_context*, float);
nk_flags nk_chart_push_slot(struct nk_context*, float, int);
void nk_chart_end(struct nk_context*);
void nk_plot(struct nk_context*, enum nk_chart_type, const float *values, int count, int offset);
void nk_plot_function(struct nk_context*, enum nk_chart_type, void *userdata, float(*value_getter)(void* user, int index), int count, int offset);
// Popup
int nk_popup_begin(struct nk_context*, enum nk_popup_type, const char*, nk_flags, struct nk_rect bounds);
void nk_popup_close(struct nk_context*);
void nk_popup_end(struct nk_context*);
void nk_popup_get_scroll(struct nk_context*, nk_uint *offset_x, nk_uint *offset_y);
void nk_popup_set_scroll(struct nk_context*, nk_uint offset_x, nk_uint offset_y);
// Combobox
int nk_combo(struct nk_context*, const char **items, int count, int selected, int item_height, struct nk_vec2 size);
int nk_combo_separator(struct nk_context*, const char *items_separated_by_separator, int separator, int selected, int count, int item_height, struct nk_vec2 size);
int nk_combo_string(struct nk_context*, const char *items_separated_by_zeros, int selected, int count, int item_height, struct nk_vec2 size);
int nk_combo_callback(struct nk_context*, void(*item_getter)(void*, int, const char**), void *userdata, int selected, int count, int item_height, struct nk_vec2 size);
void nk_combobox(struct nk_context*, const char **items, int count, int *selected, int item_height, struct nk_vec2 size);
void nk_combobox_string(struct nk_context*, const char *items_separated_by_zeros, int *selected, int count, int item_height, struct nk_vec2 size);
void nk_combobox_separator(struct nk_context*, const char *items_separated_by_separator, int separator,int *selected, int count, int item_height, struct nk_vec2 size);
void nk_combobox_callback(struct nk_context*, void(*item_getter)(void*, int, const char**), void*, int *selected, int count, int item_height, struct nk_vec2 size);
// Abstract combox
int nk_combo_begin_text(struct nk_context*, const char *selected, int, struct nk_vec2 size);
int nk_combo_begin_label(struct nk_context*, const char *selected, struct nk_vec2 size);
int nk_combo_begin_color(struct nk_context*, struct nk_color color, struct nk_vec2 size);
int nk_combo_begin_symbol(struct nk_context*,  enum nk_symbol_type,  struct nk_vec2 size);
int nk_combo_begin_symbol_label(struct nk_context*, const char *selected, enum nk_symbol_type, struct nk_vec2 size);
int nk_combo_begin_symbol_text(struct nk_context*, const char *selected, int, enum nk_symbol_type, struct nk_vec2 size);
int nk_combo_begin_image(struct nk_context*, struct nk_image img,  struct nk_vec2 size);
int nk_combo_begin_image_label(struct nk_context*, const char *selected, struct nk_image, struct nk_vec2 size);
int nk_combo_begin_image_text(struct nk_context*,  const char *selected, int, struct nk_image, struct nk_vec2 size);
int nk_combo_item_label(struct nk_context*, const char*, nk_flags alignment);
int nk_combo_item_text(struct nk_context*, const char*,int, nk_flags alignment);
int nk_combo_item_image_label(struct nk_context*, struct nk_image, const char*, nk_flags alignment);
int nk_combo_item_image_text(struct nk_context*, struct nk_image, const char*, int,nk_flags alignment);
int nk_combo_item_symbol_label(struct nk_context*, enum nk_symbol_type, const char*, nk_flags alignment);
int nk_combo_item_symbol_text(struct nk_context*, enum nk_symbol_type, const char*, int, nk_flags alignment);
void nk_combo_close(struct nk_context*);
void nk_combo_end(struct nk_context*);
// Contextual
int nk_contextual_begin(struct nk_context*, nk_flags, struct nk_vec2, struct nk_rect trigger_bounds);
int nk_contextual_item_text(struct nk_context*, const char*, int,nk_flags align);
int nk_contextual_item_label(struct nk_context*, const char*, nk_flags align);
int nk_contextual_item_image_label(struct nk_context*, struct nk_image, const char*, nk_flags alignment);
int nk_contextual_item_image_text(struct nk_context*, struct nk_image, const char*, int len, nk_flags alignment);
int nk_contextual_item_symbol_label(struct nk_context*, enum nk_symbol_type, const char*, nk_flags alignment);
int nk_contextual_item_symbol_text(struct nk_context*, enum nk_symbol_type, const char*, int, nk_flags alignment);
void nk_contextual_close(struct nk_context*);
void nk_contextual_end(struct nk_context*);
// Tooltip
void nk_tooltip(struct nk_context*, const char*);
int nk_tooltip_begin(struct nk_context*, float width);
void nk_tooltip_end(struct nk_context*);
// Menu
void nk_menubar_begin(struct nk_context*);
void nk_menubar_end(struct nk_context*);
int nk_menu_begin_text(struct nk_context*, const char* title, int title_len, nk_flags align, struct nk_vec2 size);
int nk_menu_begin_label(struct nk_context*, const char*, nk_flags align, struct nk_vec2 size);
int nk_menu_begin_image(struct nk_context*, const char*, struct nk_image, struct nk_vec2 size);
int nk_menu_begin_image_text(struct nk_context*, const char*, int,nk_flags align,struct nk_image, struct nk_vec2 size);
int nk_menu_begin_image_label(struct nk_context*, const char*, nk_flags align,struct nk_image, struct nk_vec2 size);
int nk_menu_begin_symbol(struct nk_context*, const char*, enum nk_symbol_type, struct nk_vec2 size);
int nk_menu_begin_symbol_text(struct nk_context*, const char*, int,nk_flags align,enum nk_symbol_type, struct nk_vec2 size);
int nk_menu_begin_symbol_label(struct nk_context*, const char*, nk_flags align,enum nk_symbol_type, struct nk_vec2 size);
int nk_menu_item_text(struct nk_context*, const char*, int,nk_flags align);
int nk_menu_item_label(struct nk_context*, const char*, nk_flags alignment);
int nk_menu_item_image_label(struct nk_context*, struct nk_image, const char*, nk_flags alignment);
int nk_menu_item_image_text(struct nk_context*, struct nk_image, const char*, int len, nk_flags alignment);
int nk_menu_item_symbol_text(struct nk_context*, enum nk_symbol_type, const char*, int, nk_flags alignment);
int nk_menu_item_symbol_label(struct nk_context*, enum nk_symbol_type, const char*, nk_flags alignment);
void nk_menu_close(struct nk_context*);
void nk_menu_end(struct nk_context*);
// Style
void nk_style_default(struct nk_context*);
void nk_style_from_table(struct nk_context*, const struct nk_color*);
void nk_style_load_cursor(struct nk_context*, enum nk_style_cursor, const struct nk_cursor*);
void nk_style_load_all_cursors(struct nk_context*, struct nk_cursor*);
const char* nk_style_get_color_by_name(enum nk_style_colors);
void nk_style_set_font(struct nk_context*, const struct nk_user_font*);
int nk_style_set_cursor(struct nk_context*, enum nk_style_cursor);
void nk_style_show_cursor(struct nk_context*);
void nk_style_hide_cursor(struct nk_context*);
int nk_style_push_font(struct nk_context*, const struct nk_user_font*);
int nk_style_push_float(struct nk_context*, float*, float);
int nk_style_push_vec2(struct nk_context*, struct nk_vec2*, struct nk_vec2);
int nk_style_push_style_item(struct nk_context*, struct nk_style_item*, struct nk_style_item);
int nk_style_push_flags(struct nk_context*, nk_flags*, nk_flags);
int nk_style_push_color(struct nk_context*, struct nk_color*, struct nk_color);
int nk_style_pop_font(struct nk_context*);
int nk_style_pop_float(struct nk_context*);
int nk_style_pop_vec2(struct nk_context*);
int nk_style_pop_style_item(struct nk_context*);
int nk_style_pop_flags(struct nk_context*);
int nk_style_pop_color(struct nk_context*);
// Color
struct nk_color nk_rgb(int r, int g, int b);
struct nk_color nk_rgb_iv(const int *rgb);
struct nk_color nk_rgb_bv(const nk_byte* rgb);
struct nk_color nk_rgb_f(float r, float g, float b);
struct nk_color nk_rgb_fv(const float *rgb);
struct nk_color nk_rgb_cf(struct nk_colorf c);
struct nk_color nk_rgb_hex(const char *rgb);
struct nk_color nk_rgba(int r, int g, int b, int a);
struct nk_color nk_rgba_u32(nk_uint);
struct nk_color nk_rgba_iv(const int *rgba);
struct nk_color nk_rgba_bv(const nk_byte *rgba);
struct nk_color nk_rgba_f(float r, float g, float b, float a);
struct nk_color nk_rgba_fv(const float *rgba);
struct nk_color nk_rgba_cf(struct nk_colorf c);
struct nk_color nk_rgba_hex(const char *rgb);
struct nk_colorf nk_hsva_colorf(float h, float s, float v, float a);
struct nk_colorf nk_hsva_colorfv(float *c);
void nk_colorf_hsva_f(float *out_h, float *out_s, float *out_v, float *out_a, struct nk_colorf in);
void nk_colorf_hsva_fv(float *hsva, struct nk_colorf in);
struct nk_color nk_hsv(int h, int s, int v);
struct nk_color nk_hsv_iv(const int *hsv);
struct nk_color nk_hsv_bv(const nk_byte *hsv);
struct nk_color nk_hsv_f(float h, float s, float v);
struct nk_color nk_hsv_fv(const float *hsv);
struct nk_color nk_hsva(int h, int s, int v, int a);
struct nk_color nk_hsva_iv(const int *hsva);
struct nk_color nk_hsva_bv(const nk_byte *hsva);
struct nk_color nk_hsva_f(float h, float s, float v, float a);
struct nk_color nk_hsva_fv(const float *hsva);
void nk_color_f(float *r, float *g, float *b, float *a, struct nk_color);
void nk_color_fv(float *rgba_out, struct nk_color);
struct nk_colorf nk_color_cf(struct nk_color);
void nk_color_d(double *r, double *g, double *b, double *a, struct nk_color);
void nk_color_dv(double *rgba_out, struct nk_color);
nk_uint nk_color_u32(struct nk_color);
void nk_color_hex_rgba(char *output, struct nk_color);
void nk_color_hex_rgb(char *output, struct nk_color);
void nk_color_hsv_i(int *out_h, int *out_s, int *out_v, struct nk_color);
void nk_color_hsv_b(nk_byte *out_h, nk_byte *out_s, nk_byte *out_v, struct nk_color);
void nk_color_hsv_iv(int *hsv_out, struct nk_color);
void nk_color_hsv_bv(nk_byte *hsv_out, struct nk_color);
void nk_color_hsv_f(float *out_h, float *out_s, float *out_v, struct nk_color);
void nk_color_hsv_fv(float *hsv_out, struct nk_color);
void nk_color_hsva_i(int *h, int *s, int *v, int *a, struct nk_color);
void nk_color_hsva_b(nk_byte *h, nk_byte *s, nk_byte *v, nk_byte *a, struct nk_color);
void nk_color_hsva_iv(int *hsva_out, struct nk_color);
void nk_color_hsva_bv(nk_byte *hsva_out, struct nk_color);
void nk_color_hsva_f(float *out_h, float *out_s, float *out_v, float *out_a, struct nk_color);
void nk_color_hsva_fv(float *hsva_out, struct nk_color);
// Image
nk_handle nk_handle_ptr(void*);
nk_handle nk_handle_id(int);
struct nk_image nk_image_handle(nk_handle);
struct nk_image nk_image_ptr(void*);
struct nk_image nk_image_id(int);
int nk_image_is_subimage(const struct nk_image* img);
struct nk_image nk_subimage_ptr(void*, unsigned short w, unsigned short h, struct nk_rect sub_region);
struct nk_image nk_subimage_id(int, unsigned short w, unsigned short h, struct nk_rect sub_region);
struct nk_image nk_subimage_handle(nk_handle, unsigned short w, unsigned short h, struct nk_rect sub_region);
// 9-Slice
struct nk_nine_slice nk_nine_slice_handle(nk_handle, unsigned short l, unsigned short t, unsigned short r, unsigned short b);
struct nk_nine_slice nk_nine_slice_ptr(void*, unsigned short l, unsigned short t, unsigned short r, unsigned short b);
struct nk_nine_slice nk_nine_slice_id(int, unsigned short l, unsigned short t, unsigned short r, unsigned short b);
int nk_nine_slice_is_sub9slice(const struct nk_nine_slice* img);
struct nk_nine_slice nk_sub9slice_ptr(void*, unsigned short w, unsigned short h, struct nk_rect sub_region, unsigned short l, unsigned short t, unsigned short r, unsigned short b);
struct nk_nine_slice nk_sub9slice_id(int, unsigned short w, unsigned short h, struct nk_rect sub_region, unsigned short l, unsigned short t, unsigned short r, unsigned short b);
struct nk_nine_slice nk_sub9slice_handle(nk_handle, unsigned short w, unsigned short h, struct nk_rect sub_region, unsigned short l, unsigned short t, unsigned short r, unsigned short b);
// Math
nk_hash nk_murmur_hash(const void *key, int len, nk_hash seed);
void nk_triangle_from_direction(struct nk_vec2 *result, struct nk_rect r, float pad_x, float pad_y, enum nk_heading);
struct nk_vec2 nk_vec2i(int x, int y);
struct nk_vec2 nk_vec2v(const float *xy);
struct nk_vec2 nk_vec2iv(const int *xy);
struct nk_rect nk_get_null_rect(void);
struct nk_rect nk_recti(int x, int y, int w, int h);
struct nk_rect nk_recta(struct nk_vec2 pos, struct nk_vec2 size);
struct nk_rect nk_rectv(const float *xywh);
struct nk_rect nk_rectiv(const int *xywh);
struct nk_vec2 nk_rect_pos(struct nk_rect);
struct nk_vec2 nk_rect_size(struct nk_rect);
// String
int nk_strfilter(const char *text, const char *regexp);
int nk_strmatch_fuzzy_string(char const *str, char const *pattern, int *out_score);
int nk_strmatch_fuzzy_text(const char *txt, int txt_len, const char *pattern, int *out_score);
// UTF-8
int nk_utf_decode(const char*, nk_rune*, int);
int nk_utf_encode(nk_rune, char*, int);
int nk_utf_len(const char*, int byte_len);
const char* nk_utf_at(const char *buffer, int length, int index, nk_rune *unicode, int *len);
// Text editor
int nk_filter_default(const struct nk_text_edit*, nk_rune unicode);
int nk_filter_ascii(const struct nk_text_edit*, nk_rune unicode);
int nk_filter_float(const struct nk_text_edit*, nk_rune unicode);
int nk_filter_decimal(const struct nk_text_edit*, nk_rune unicode);
int nk_filter_hex(const struct nk_text_edit*, nk_rune unicode);
int nk_filter_oct(const struct nk_text_edit*, nk_rune unicode);
int nk_filter_binary(const struct nk_text_edit*, nk_rune unicode);
void nk_textedit_init_default(struct nk_text_edit*);
void nk_textedit_init(struct nk_text_edit*, struct nk_allocator*, nk_size size);
void nk_textedit_init_fixed(struct nk_text_edit*, void *memory, nk_size size);
void nk_textedit_free(struct nk_text_edit*);
void nk_textedit_text(struct nk_text_edit*, const char*, int total_len);
void nk_textedit_delete(struct nk_text_edit*, int where, int len);
void nk_textedit_delete_selection(struct nk_text_edit*);
void nk_textedit_select_all(struct nk_text_edit*);
int nk_textedit_cut(struct nk_text_edit*);
int nk_textedit_paste(struct nk_text_edit*, char const*, int len);
void nk_textedit_undo(struct nk_text_edit*);
void nk_textedit_redo(struct nk_text_edit*);
// Drawing
void nk_stroke_line(struct nk_command_buffer *b, float x0, float y0, float x1, float y1, float line_thickness, struct nk_color);
void nk_stroke_curve(struct nk_command_buffer*, float, float, float, float, float, float, float, float, float line_thickness, struct nk_color);
void nk_stroke_rect(struct nk_command_buffer*, struct nk_rect, float rounding, float line_thickness, struct nk_color);
void nk_stroke_circle(struct nk_command_buffer*, struct nk_rect, float line_thickness, struct nk_color);
void nk_stroke_arc(struct nk_command_buffer*, float cx, float cy, float radius, float a_min, float a_max, float line_thickness, struct nk_color);
void nk_stroke_triangle(struct nk_command_buffer*, float, float, float, float, float, float, float line_thichness, struct nk_color);
void nk_stroke_polyline(struct nk_command_buffer*, float *points, int point_count, float line_thickness, struct nk_color col);
void nk_stroke_polygon(struct nk_command_buffer*, float*, int point_count, float line_thickness, struct nk_color);
void nk_fill_rect(struct nk_command_buffer*, struct nk_rect, float rounding, struct nk_color);
void nk_fill_rect_multi_color(struct nk_command_buffer*, struct nk_rect, struct nk_color left, struct nk_color top, struct nk_color right, struct nk_color bottom);
void nk_fill_circle(struct nk_command_buffer*, struct nk_rect, struct nk_color);
void nk_fill_arc(struct nk_command_buffer*, float cx, float cy, float radius, float a_min, float a_max, struct nk_color);
void nk_fill_triangle(struct nk_command_buffer*, float x0, float y0, float x1, float y1, float x2, float y2, struct nk_color);
void nk_fill_polygon(struct nk_command_buffer*, float*, int point_count, struct nk_color);
void nk_draw_image(struct nk_command_buffer*, struct nk_rect, const struct nk_image*, struct nk_color);
void nk_draw_nine_slice(struct nk_command_buffer*, struct nk_rect, const struct nk_nine_slice*, struct nk_color);
void nk_draw_text(struct nk_command_buffer*, struct nk_rect, const char *text, int len, const struct nk_user_font*, struct nk_color, struct nk_color);
void nk_push_scissor(struct nk_command_buffer*, struct nk_rect);
void nk_push_custom(struct nk_command_buffer*, struct nk_rect, nk_command_custom_callback, nk_handle usr);
// Input
int nk_input_has_mouse_click(const struct nk_input*, enum nk_buttons);
int nk_input_has_mouse_click_in_rect(const struct nk_input*, enum nk_buttons, struct nk_rect);
int nk_input_has_mouse_click_down_in_rect(const struct nk_input*, enum nk_buttons, struct nk_rect, int down);
int nk_input_is_mouse_click_in_rect(const struct nk_input*, enum nk_buttons, struct nk_rect);
int nk_input_is_mouse_click_down_in_rect(const struct nk_input *i, enum nk_buttons id, struct nk_rect b, int down);
int nk_input_any_mouse_click_in_rect(const struct nk_input*, struct nk_rect);
int nk_input_is_mouse_prev_hovering_rect(const struct nk_input*, struct nk_rect);
int nk_input_is_mouse_hovering_rect(const struct nk_input*, struct nk_rect);
int nk_input_mouse_clicked(const struct nk_input*, enum nk_buttons, struct nk_rect);
int nk_input_is_mouse_down(const struct nk_input*, enum nk_buttons);
int nk_input_is_mouse_pressed(const struct nk_input*, enum nk_buttons);
int nk_input_is_mouse_released(const struct nk_input*, enum nk_buttons);
int nk_input_is_key_pressed(const struct nk_input*, enum nk_keys);
int nk_input_is_key_released(const struct nk_input*, enum nk_keys);
int nk_input_is_key_down(const struct nk_input*, enum nk_keys);
// GUI
struct nk_style_item nk_style_item_color(struct nk_color);
struct nk_style_item nk_style_item_image(struct nk_image img);
struct nk_style_item nk_style_item_nine_slice(struct nk_nine_slice slice);
struct nk_style_item nk_style_item_hide(void);

// Allegro backend
struct nk_context*     nk_allegro5_init(NkAllegro5Font *font, ALLEGRO_DISPLAY *dsp,
                                  unsigned int width, unsigned int height);
int                    nk_allegro5_handle_event(ALLEGRO_EVENT *ev);
void                   nk_allegro5_shutdown(void);
void                   nk_allegro5_render(void);
struct nk_image*       nk_allegro5_create_image(const char* file_name);
void                   nk_allegro5_del_image(struct nk_image* image);
NkAllegro5Font*        nk_allegro5_font_create_from_file(const char *file_name, int font_size, int flags);
void                   nk_allegro5_font_del(NkAllegro5Font *font);
void                   nk_allegro5_font_set_font(NkAllegro5Font *font);
