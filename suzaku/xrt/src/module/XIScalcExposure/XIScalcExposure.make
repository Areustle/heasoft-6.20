# XIScalcExposure.make

XIScalcExposure_DIR = $(XRT_MODULE_DIR)/XIScalcExposure/1.2
XIScalcExposure = XIScalcExposure.o

XIScalcExposure.o: $(XIScalcExposure_DIR)/XIScalcExposure.c
	if [ "$(SOURCE_LINK)" = "yes" ]; then \
		rm -f $*; ln -fs $(XIScalcExposure_DIR) $*; \
	fi
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(XIScalcExposure_DIR)/XIScalcExposure.c

clean::
	rm -f XIScalcExposure
