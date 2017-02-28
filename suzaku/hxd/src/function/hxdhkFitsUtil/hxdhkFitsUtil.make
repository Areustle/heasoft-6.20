hxdhkFitsUtil_DIR = $(HXD_FUNCTION_DIR)/hxdhkFitsUtil/0.1.0

hxdhkFitsUtil = hxdhkFitsUtil.o

hxdhkFitsUtil.o: $(hxdhkFitsUtil_DIR)/hxdhkFitsUtil.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdhkFitsUtil_DIR)/hxdhkFitsUtil.c
