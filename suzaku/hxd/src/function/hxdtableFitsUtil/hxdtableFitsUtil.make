hxdtableFitsUtil_DIR = $(HXD_FUNCTION_DIR)/hxdtableFitsUtil/0.0.9

hxdtableFitsUtil = hxdtableFitsUtil.o

hxdtableFitsUtil.o: $(hxdtableFitsUtil_DIR)/hxdtableFitsUtil.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdtableFitsUtil_DIR)/hxdtableFitsUtil.c
