hxdgradeUtil_DIR = $(HXD_FUNCTION_DIR)/hxdgradeUtil/2.0.4

hxdgradeUtil = hxdgradeUtil.o

hxdgradeUtil.o: $(hxdgradeUtil_DIR)/hxdgradeUtil.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdgradeUtil_DIR)/hxdgradeUtil.c
