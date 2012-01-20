include ../support/include.mk

all: ${EBIN_DIRS} $(EBIN_FILES)
debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(ERL_OBJECTS_WITHDIR)
#	echo $(ERL_OBJECTS_WITHDIR)


${EBIN_DIRS}:
	mkdir -p  $@

#checkdirs:
#	foreach dir ${EBIN_DIRS} mkdir ${dir}
