DEBUG_OPTION=-g
BUILD_DIR=src/
BUILD=ocamlbuild -cflags $(DEBUG_OPTION) $(BUILD_DIR)main.native

default : 
	$(BUILD)
release: 
	DEBUG_OPTION=""
	$(BUILD)




