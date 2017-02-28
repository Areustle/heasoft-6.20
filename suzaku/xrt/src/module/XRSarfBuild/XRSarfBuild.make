# XRSarfBuild.make

XRSarfBuild_DIR = $(XRT_MODULE_DIR)/XRSarfBuild/1.1
XRSarfBuild = XRSarfBuild.o

XRSarfBuild.o: $(XRSarfBuild_DIR)/XRSarfBuild.c
	if [ "$(SOURCE_LINK)" = "yes" ]; then \
		rm -f $*; ln -fs $(XRSarfBuild_DIR) $*; \
	fi
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(XRSarfBuild_DIR)/XRSarfBuild.c

clean::
	rm -f XRSarfBuild
