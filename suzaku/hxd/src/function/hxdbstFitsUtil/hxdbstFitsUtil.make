hxdbstFitsUtil_DIR = $(HXD_FUNCTION_DIR)/hxdbstFitsUtil/2.1.1

hxdbstFitsUtil = hxdbstFitsUtil.o

hxdbstFitsUtil.o: $(hxdbstFitsUtil_DIR)/hxdbstFitsUtil.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdbstFitsUtil_DIR)/hxdbstFitsUtil.c
