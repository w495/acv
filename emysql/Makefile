# default

all:
	$(call make_ebin);

release:
	$(call make_ebin);
debug:
	(cd src && $(MAKE) debug) || exit 1;

clean:
	(cd src;$(MAKE) clean)

make_ebin =  \
	(cd src && $(MAKE) $(1)) || exit 1

