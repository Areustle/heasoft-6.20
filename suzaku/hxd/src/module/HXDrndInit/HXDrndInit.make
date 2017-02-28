HXDrndInit_DIR = ${HXD_MODULE_DIR}/HXDrndInit/0.2.0
HXDrndInit = HXDrndInit.o

HXDrndInit.o: $(HXDrndInit_DIR)/HXDrndInit.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDrndInit_DIR)/HXDrndInit.c
