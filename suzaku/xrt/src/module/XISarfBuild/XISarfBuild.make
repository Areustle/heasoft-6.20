# XISarfBuild.make

XISarfBuild_DIR = $(XRT_MODULE_DIR)/XISarfBuild/1.1
XISarfBuild = XISarfBuild.o

XISarfBuild.o: $(XISarfBuild_DIR)/XISarfBuild.c
	if [ "$(SOURCE_LINK)" = "yes" ]; then \
		rm -f $*; ln -fs $(XISarfBuild_DIR) $*; \
	fi
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(XISarfBuild_DIR)/XISarfBuild.c

clean::
	rm -f XISarfBuild
