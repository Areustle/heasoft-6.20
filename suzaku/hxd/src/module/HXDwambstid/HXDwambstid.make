HXDwambstid_DIR = ${HXD_MODULE_DIR}/HXDwambstid/0.0.5
HXDwambstid = HXDwambstid.o

HXDwambstid.o: $(HXDwambstid_DIR)/HXDwambstid.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDwambstid_DIR)/HXDwambstid.c
