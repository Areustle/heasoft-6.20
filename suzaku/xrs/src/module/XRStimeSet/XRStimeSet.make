# XRStimeSet.make

XRStimeSet_DIR = $(XRS_MODULE_DIR)/XRStimeSet/1.9
XRStimeSet = XRStimeSet.o xrs_tick2ti.o

XRStimeSet.o: $(XRStimeSet_DIR)/XRStimeSet.c
	$(CC) -I$(XRStimeSet_DIR) -I$(aste_ti2time_DIR) -I$(xrsFitsHeaderUtil_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(XRStimeSet_DIR)/XRStimeSet.c

xrs_tick2ti.o: $(XRStimeSet_DIR)/xrs_tick2ti.c
	$(CC) -I$(XRStimeSet_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(XRStimeSet_DIR)/xrs_tick2ti.c
