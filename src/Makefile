lib_extension=
lib_prefix=
ifeq ($(OS),Windows_NT)
	lib_extension=dll
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

all: $(dir)interface.lisp $(dir)$(lib_prefix)allegro_nuklear.$(lib_extension)

$(dir)interface.lisp: $(dir)interface.i
	swig -cffi $^
	sed -i 's/swig-lispify "nk_\([a-z0-9_]*\)"/swig-lispify "\1"/gi' $@
	sed -i 's/"prog"/"prog_"/' $@
	sed -i '/defcstruct/b; /defcunion/b; /style_item_data/b; s/\(#\.(swig-lispify "[a-zA-Z0-9_]*" '\''classname)\)/(:struct \1)/g' $@
	sed -i 's/:pointer :count 4/:uint64/g' $@
	sed -i 's/defcstruct \(.*\)/defcstruct (\1 :class \1)/' $@
	sed -i 's/"allegro5/"allegro/g' $@

$(dir)$(lib_prefix)allegro_nuklear.$(lib_extension): $(dir)nuklear.c $(dir)nuklear.h $(dir)nuklear_allegro5.h
	gcc $< -std=c99 -O2 -pipe -fPIC -shared -lallegro -lallegro_font -lallegro_image -lallegro_primitives -lallegro_ttf -o $@

clean:
	rm -rf interface.lisp *.dll *.dylib *.so

.PHONY: clean
