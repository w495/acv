# default

release:
	$(call make_ebin); \
	$(call make_js,$(1),$(2),build);


debug:
	(cd src && $(MAKE) debug) || exit 1; \
	(cd js && ./generate.py source) || exit 1

clean:
	(cd src;$(MAKE) clean)

make_ebin =  \
	(cd src && $(MAKE) $(1)) || exit 1

make_js =                                                \
	(cd js &&                                              \
	 ./generate.py $(3)) || exit 1
