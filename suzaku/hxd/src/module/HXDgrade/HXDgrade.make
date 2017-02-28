HXDgrade_DIR = ${HXD_MODULE_DIR}/HXDgrade/2.0.4
HXDgrade = HXDgrade.o
HXDgradeRPT = HXDgradeRPT.o
HXDgradeFITS = HXDgradeFITS.o

HXDgrade.o: $(HXDgrade_DIR)/HXDgrade.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDgrade_DIR)/HXDgrade.c
HXDgradeRPT.o: $(HXDgrade_DIR)/HXDgradeRPT.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDgrade_DIR)/HXDgradeRPT.c
HXDgradeFITS.o: $(HXDgrade_DIR)/HXDgradeFITS.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDgrade_DIR)/HXDgradeFITS.c
