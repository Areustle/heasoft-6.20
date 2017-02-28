hxdTimeUtil_DIR = $(HXD_FUNCTION_DIR)/hxdTimeUtil/2.0.2

hxdTimeUtil = 	hxdTimeUtil.o \
		hxdWelTimeUtil.o \
		hxdTrnTimeUtil.o \
		hxdBstTimeUtil.o \
		hxdSclTimeUtil.o

hxdTimeUtil.o: $(hxdTimeUtil_DIR)/hxdTimeUtil.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdTimeUtil_DIR)/hxdTimeUtil.c

hxdWelTimeUtil.o: $(hxdTimeUtil_DIR)/hxdWelTimeUtil.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdTimeUtil_DIR)/hxdWelTimeUtil.c

hxdTrnTimeUtil.o: $(hxdTimeUtil_DIR)/hxdTrnTimeUtil.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdTimeUtil_DIR)/hxdTrnTimeUtil.c

hxdBstTimeUtil.o: $(hxdTimeUtil_DIR)/hxdBstTimeUtil.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdTimeUtil_DIR)/hxdBstTimeUtil.c

hxdSclTimeUtil.o: $(hxdTimeUtil_DIR)/hxdSclTimeUtil.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdTimeUtil_DIR)/hxdSclTimeUtil.c

