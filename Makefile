DEPS	= \
	emysql \
	erlxslt \
	gen_smtp \
	mochiweb \
	sha2_erlang 

SUBDIRS	= web

release: git_submodule build_deps
	@set -e ; \
	  for d in $(SUBDIRS) ; do \
	    if [ -f $$d/Makefile ]; then \
	      ( echo "\n=============== Building RELEASE version $$d ===============" && \
	        cd $$d && \
	        $(MAKE) --no-print-directory $@ ) || exit 1 ; fi ; \
	  done

debug: git_submodule build_deps
	@set -e ; \
	  for d in $(SUBDIRS) ; do \
	    if [ -f $$d/Makefile ]; then \
	      ( echo "\n=============== Building DEBUG version $$d ===============" && \
	        cd $$d && \
	        $(MAKE) debug --no-print-directory $@ ) || exit 1 ; fi ; \
	  done

clean: clean_deps
	@set -e ; \
	  for d in $(SUBDIRS) ; do \
	    if [ -f $$d/Makefile ]; then \
	      ( echo "\n=============== Cleaning $$d ===============" && \
	        cd $$d && \
	        $(MAKE) clean --no-print-directory $@ ) || exit 1 ; fi ; \
	  done

build_deps:
	@set -e ; \
	  for d in $(DEPS) ; do \
	    if [ -f $$d/Makefile ]; then \
	      ( echo "\n=============== Building deps $$d =============== " && \
	        cd $$d && \
	        $(MAKE)) || exit 1 ; fi ; \
	  done

clean_deps:
	@set -e ; \
	  for d in $(DEPS) ; do \
	    if [ -f $$d/Makefile ]; then \
	      ( echo "\n=============== Cleaning deps $$d ===============" && \
	        cd $$d && \
	        $(MAKE)) || exit 1 ; fi ; \
	  done

git_submodule:
	git submodule init
	git submodule update

