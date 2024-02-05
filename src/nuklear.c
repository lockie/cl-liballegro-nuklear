#define NK_IMPLEMENTATION
#define NK_INCLUDE_DEFAULT_ALLOCATOR
#define NK_INCLUDE_FIXED_TYPES
#define NK_INCLUDE_STANDARD_IO
#define NK_BUTTON_TRIGGER_ON_RELEASE

#define NK_ALLEGRO5_IMPLEMENTATION

#include <stdio.h>
#include <allegro5/allegro_native_dialog.h>

static void (*break_callback)(const char*);
static void (*abort_callback)(void);

static void nk_allegro5_handle_assertion_failure(
    const char* expr, const char* file, int line, const char* func)
{
    int r;
    static char buffer[4096];
    snprintf(buffer, sizeof(buffer),
             "Debug assertion failed in cl-liballegro-nuklear!\n\n"
             "File: %s\n"
             "Line: %d\n"
             "Function: %s\n"
             "Expression: %s\n",
             file, line, func, expr);

    r = al_show_native_message_box(
        NULL, "Error", "", buffer, "Abort|Ignore|Debug",
        ALLEGRO_MESSAGEBOX_ERROR | ALLEGRO_MESSAGEBOX_OK_CANCEL);
    if(r < 2)
    {
        if(abort_callback)
            abort_callback();
        else
            abort();
    }
    if(r == 3 && break_callback)
        break_callback(buffer);
}

#define NK_ASSERT(x) ((void)((x) || (nk_allegro5_handle_assertion_failure( \
                                         #x, __FILE__, __LINE__, __func__), 0)))

#include "nuklear.h"
#include "nuklear_allegro5.h"

NK_API void nk_allegro5_setup_assert(void (*_break)(const char*),
                                     void (*_abort)(void))
{
    break_callback = _break;
    abort_callback = _abort;
}
