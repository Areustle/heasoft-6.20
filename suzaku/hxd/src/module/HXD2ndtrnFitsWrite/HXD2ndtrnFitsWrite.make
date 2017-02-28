HXD2ndtrnFitsWrite_DIR = ${HXD_MODULE_DIR}/HXD2ndtrnFitsWrite/2.0.1
HXD2ndtrnFitsWrite = HXD2ndtrnFitsWrite.o

HXD2ndtrnFitsWrite.o: $(HXD2ndtrnFitsWrite_DIR)/HXD2ndtrnFitsWrite.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXD2ndtrnFitsWrite_DIR)/HXD2ndtrnFitsWrite.c
