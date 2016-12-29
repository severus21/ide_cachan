
DIRS=$(shell ls -d */)
PLUGINS=$(addsuffix plugin.cmxs,$(DIRS))

default: $(PLUGINS)

%/plugin.cmxs:
	cd $* && make
	-rm $*.cmxs
	cp $@ $*.cmxs

clean_%:
	cd $* && make clean

tests: 
	cd $* && make tests

clean: $(addprefix clean_,$(DIRS))
	-rm *.cmxs

doc_%:
	cd $* && rm -rf doc && make doc

doc: $(addprefix doc_,$(DIRS))
