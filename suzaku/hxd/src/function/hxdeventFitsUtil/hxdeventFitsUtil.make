hxdeventFitsUtil_DIR = $(HXD_FUNCTION_DIR)/hxdeventFitsUtil/2.0.4

hxdeventFitsUtil = hxdeventFitsUtil.o

hxdeventFitsUtil.o: $(hxdeventFitsUtil_DIR)/hxdeventFitsUtil.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdeventFitsUtil_DIR)/hxdeventFitsUtil.c
