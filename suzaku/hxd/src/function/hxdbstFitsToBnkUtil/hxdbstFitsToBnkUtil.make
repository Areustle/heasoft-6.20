hxdbstFitsToBnkUtil_DIR = $(HXD_FUNCTION_DIR)/hxdbstFitsToBnkUtil/0.2.2

hxdbstFitsToBnkUtil = hxdbstFitsToBnkUtil.o

hxdbstFitsToBnkUtil.o: $(hxdbstFitsToBnkUtil_DIR)/hxdbstFitsToBnkUtil.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdbstFitsToBnkUtil_DIR)/hxdbstFitsToBnkUtil.c
