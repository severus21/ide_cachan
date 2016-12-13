
DIRS=$(shell ls -d */)
PLUGINS=$(addsuffix plugin.cmxs,$(DIRS))

default: $(PLUGINS)

%/plugin.cmxs:
	cd $* && make
	-rm $*.cmxs
	cp $@ $*.cmxs

clean_%:
	cd $* && make clean

clean: $(addprefix clean_,$(DIRS))
	-rm *.cmxs
