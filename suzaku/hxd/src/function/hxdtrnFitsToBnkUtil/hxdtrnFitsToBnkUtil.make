hxdtrnFitsToBnkUtil_DIR = $(HXD_FUNCTION_DIR)/hxdtrnFitsToBnkUtil/0.2.1

hxdtrnFitsToBnkUtil = hxdtrnFitsToBnkUtil.o

hxdtrnFitsToBnkUtil.o: $(hxdtrnFitsToBnkUtil_DIR)/hxdtrnFitsToBnkUtil.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdtrnFitsToBnkUtil_DIR)/hxdtrnFitsToBnkUtil.c
