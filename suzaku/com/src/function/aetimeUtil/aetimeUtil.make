# aetimeUtil.make

aetimeUtil_DIR = $(ASTE_FUNCTION_DIR)/aetimeUtil/2.2
aetimeUtil = aetimeUtil.o

aetimeUtil.o: $(aetimeUtil_DIR)/aetimeUtil.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(aetimeUtil_DIR)/aetimeUtil.c
