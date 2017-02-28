hxdarfUtil_DIR = $(HXD_FUNCTION_DIR)/hxdarfUtil/0.5.1

hxdarfUtil = hxdarfUtil.o

hxdarfUtil.o: $(hxdarfUtil_DIR)/hxdarfUtil.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdarfUtil_DIR)/hxdarfUtil.c

