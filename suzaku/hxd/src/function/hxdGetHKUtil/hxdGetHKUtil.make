hxdGetHKUtil_DIR = $(HXD_FUNCTION_DIR)/hxdGetHKUtil/0.0.6

hxdGetHKUtil = hxdGetHKUtil.o

hxdGetHKUtil.o: $(hxdGetHKUtil_DIR)/hxdGetHKUtil.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdGetHKUtil_DIR)/hxdGetHKUtil.c
