# aeFitsHeaderUtil.make

aeFitsHeaderUtil_DIR = $(ASTE_FUNCTION_DIR)/aeFitsHeaderUtil/3.4
aeFitsHeaderUtil = aeFitsHeaderUtil.o

aeFitsHeaderUtil.o: $(aeFitsHeaderUtil_DIR)/aeFitsHeaderUtil.c
	$(CC) -I$(aeFitsHeaderUtil_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(aeFitsHeaderUtil_DIR)/aeFitsHeaderUtil.c
