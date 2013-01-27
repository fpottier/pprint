include Makefile

# Copying the pprint library into the Mezzo source tree.

.PHONY: mezzo

MEZZO := $(HOME)/dev/mezzo/src/pprint

mezzo: all
	rm -rf $(MEZZO)
	mkdir $(MEZZO)
	cp -f README AUTHORS LICENSE *.ml *.mli $(MEZZO)
