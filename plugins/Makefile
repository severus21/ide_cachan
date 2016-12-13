
DIRS=$(shell ls -d */)
PLUGINS=$(addsuffix plugin.cmxa,$(DIRS))

default: $(PLUGINS)

%/plugin.cmxa:
	cd $* && make
	-rm $*.cmxs
	ln -s $@ $*.cmxs

clean_%:
	cd $* && make clean

clean: $(addprefix clean_,$(DIRS))
	-rm *.cmxs
