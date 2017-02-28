hxdtrnpiUtil_DIR = $(HXD_FUNCTION_DIR)/hxdtrnpiUtil/0.0.4

hxdtrnpiUtil = hxdtrnpiUtil.o hxdtrnpiDEUtil.o hxdtrnpiGainUtil.o

hxdtrnpiUtil.o: $(hxdtrnpiUtil_DIR)/hxdtrnpiUtil.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdtrnpiUtil_DIR)/hxdtrnpiUtil.c
hxdtrnpiDEUtil.o: $(hxdtrnpiUtil_DIR)/hxdtrnpiDEUtil.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdtrnpiUtil_DIR)/hxdtrnpiDEUtil.c
hxdtrnpiGainUtil.o: $(hxdtrnpiUtil_DIR)/hxdtrnpiGainUtil.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdtrnpiUtil_DIR)/hxdtrnpiGainUtil.c


