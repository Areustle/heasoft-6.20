hxdtrngradeUtil_DIR = $(HXD_FUNCTION_DIR)/hxdtrngradeUtil/0.0.2

hxdtrngradeUtil = hxdtrngradeUtil.o

hxdtrngradeUtil.o: $(hxdtrngradeUtil_DIR)/hxdtrngradeUtil.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdtrngradeUtil_DIR)/hxdtrngradeUtil.c
