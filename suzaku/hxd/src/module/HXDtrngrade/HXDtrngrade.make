HXDtrngrade_DIR = ${HXD_MODULE_DIR}/HXDtrngrade/0.1.0
HXDtrngrade = HXDtrngrade.o
HXDtrngradeRPT = HXDtrngradeRPT.o
HXDtrngradeFITS = HXDtrngradeFITS.o

HXDtrngrade.o: $(HXDtrngrade_DIR)/HXDtrngrade.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDtrngrade_DIR)/HXDtrngrade.c
HXDtrngradeRPT.o: $(HXDtrngrade_DIR)/HXDtrngradeRPT.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDtrngrade_DIR)/HXDtrngradeRPT.c
HXDtrngradeFITS.o: $(HXDtrngrade_DIR)/HXDtrngradeFITS.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDtrngrade_DIR)/HXDtrngradeFITS.c
