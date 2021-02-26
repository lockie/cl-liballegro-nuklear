lib_extension=
lib_prefix=
exe_extension=
ifeq ($(OS),Windows_NT)
	lib_extension=dll
	exe_extension=.exe
else
	lib_prefix=lib
	uname := $(shell uname -s)
	ifeq ($(uname),Darwin)
		lib_extension=dylib
	else
		lib_extension=so
	endif
endif

dir=$(dir $(abspath $(lastword $(MAKEFILE_LIST))))

CFLAGS=`pkg-config --cflags allegro-5 allegro_font-5 allegro_image-5 allegro_primitives-5 allegro_ttf-5`
LDFLAGS=`pkg-config --libs allegro-5 allegro_font-5 allegro_image-5 allegro_primitives-5 allegro_ttf-5`

all: $(dir)interface.lisp $(dir)offsets.lisp $(dir)$(lib_prefix)allegro_nuklear.$(lib_extension)

$(dir)interface.lisp: $(dir)interface.i
	swig -cffi $^
	sed -i 's/swig-lispify "nk_\([a-z0-9_]*\)"/swig-lispify "\1"/gi' $@
	sed -i 's/"prog"/"prog_"/' $@
	sed -i '/defcstruct/b; /defcunion/b; /style_item_data/b; s/\(#\.(swig-lispify "[a-zA-Z0-9_]*" '\''classname)\)/(:struct \1)/g' $@
	sed -i 's/:pointer :count 4/:uint64/g' $@
	sed -i 's/defcstruct \(.*\)/defcstruct (\1 :class \1)/' $@
	sed -i 's/"allegro5/"allegro/g' $@

$(dir)offsets.lisp: $(dir)gen-offsets$(exe_extension)
	$^ > $@

$(dir)gen-offsets$(exe_extension): $(dir)gen-offsets.c
	$(CC) $(CFLAGS) $< -std=c99 -O2 -pipe -fPIC $(LDFLAGS) -o $@

$(dir)$(lib_prefix)allegro_nuklear.$(lib_extension): $(dir)nuklear.c $(dir)nuklear.h $(dir)nuklear_allegro5.h
	$(CC) $(CFLAGS) $< -std=c99 -O2 -pipe -fPIC -shared $(LDFLAGS) -o $@

clean:
	rm -rf interface.lisp offsets.lisp *.dll *.dylib *.so *.exe gen-offsets core

.PHONY: clean
