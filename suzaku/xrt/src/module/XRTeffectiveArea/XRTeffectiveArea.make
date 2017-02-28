# XRTeffectiveArea.make

XRTeffectiveArea_DIR = $(XRT_MODULE_DIR)/XRTeffectiveArea/1.2
XRTeffectiveArea = XRTeffectiveArea.o

XRTeffectiveArea.o: $(XRTeffectiveArea_DIR)/XRTeffectiveArea.c
	if [ "$(SOURCE_LINK)" = "yes" ]; then \
		rm -f $*; ln -fs $(XRTeffectiveArea_DIR) $*; \
	fi
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(XRTeffectiveArea_DIR)/XRTeffectiveArea.c

clean::
	rm -f XRTeffectiveArea
