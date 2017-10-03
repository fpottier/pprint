.PHONY: all install uninstall reinstall clean doc test

all install uninstall reinstall clean doc test:
	$(MAKE) -C src $@
