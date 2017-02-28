hxdeventFitsToBnkUtil_DIR = $(HXD_FUNCTION_DIR)/hxdeventFitsToBnkUtil/2.0.0

hxdeventFitsToBnkUtil = hxdeventFitsToBnkUtil.o

hxdeventFitsToBnkUtil.o: $(hxdeventFitsToBnkUtil_DIR)/hxdeventFitsToBnkUtil.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdeventFitsToBnkUtil_DIR)/hxdeventFitsToBnkUtil.c
