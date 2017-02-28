HXDHKFitsWrite_DIR = ${HXD_MODULE_DIR}/HXDHKFitsWrite/2.0.0

HXDHKFitsWrite = HXDHKFitsWrite.o

HXDHKFitsWrite.o: $(HXDHKFitsWrite_DIR)/HXDHKFitsWrite.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDHKFitsWrite_DIR)/HXDHKFitsWrite.c

