hxdphaUtil_DIR = $(HXD_FUNCTION_DIR)/hxdphaUtil/0.0.1

hxdphaUtil = hxdphaUtil.o

hxdphaUtil.o: $(hxdphaUtil_DIR)/hxdphaUtil.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdphaUtil_DIR)/hxdphaUtil.c




