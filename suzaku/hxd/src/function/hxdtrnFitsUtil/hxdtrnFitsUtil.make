hxdtrnFitsUtil_DIR = $(HXD_FUNCTION_DIR)/hxdtrnFitsUtil/0.2.4

hxdtrnFitsUtil = hxdtrnFitsUtil.o

hxdtrnFitsUtil.o: $(hxdtrnFitsUtil_DIR)/hxdtrnFitsUtil.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdtrnFitsUtil_DIR)/hxdtrnFitsUtil.c
