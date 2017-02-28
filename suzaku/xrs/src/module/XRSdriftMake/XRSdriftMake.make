# XRSdriftMake.make

XRSdriftMake_DIR = $(XRS_MODULE_DIR)/XRSdriftMake/1.7
XRSdriftMake = XRSdriftMake.o

XRSdriftMake.o: $(XRSdriftMake_DIR)/XRSdriftMake.c
	$(CC) -I$(aeFitsHeaderUtil_DIR) -I$(xrsFitsHeaderUtil_DIR) -I$(xrs_gain_DIR) -I$(XRSdriftMake_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(XRSdriftMake_DIR)/XRSdriftMake.c
