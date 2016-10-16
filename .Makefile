DEBUG_OPTION=-g
SOURCE_DIR=src/
BUILD_DIR=debug/
BUILD=ocamlbuild \
	  -build-dir "$(BUILD_DIR)" \
	  -cflags "$(DEBUG_OPTION)" \
	  -lflags "lablgtk.cmxa" \
	  $(SOURCE_DIR)ide.native

default:
	$(BUILD)
release:
	DEBUG_OPTION=
	BUILD_DIR=release/
	$(BUILD)

clean:
	ocamlbuild -clean
