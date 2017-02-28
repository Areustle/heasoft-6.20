# XRTthermalShield.make

XRTthermalShield_DIR = $(XRT_MODULE_DIR)/XRTthermalShield/1.4
XRTthermalShield = XRTthermalShield.o

XRTthermalShield.o: $(XRTthermalShield_DIR)/XRTthermalShield.c
	$(CC) -I$(aste_caldb_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(XRTthermalShield_DIR)/XRTthermalShield.c
