HXD2ndbstFitsWrite_DIR = ${HXD_MODULE_DIR}/HXD2ndbstFitsWrite/2.0.5
HXD2ndbstFitsWrite = HXD2ndbstFitsWrite.o

HXD2ndbstFitsWrite.o: $(HXD2ndbstFitsWrite_DIR)/HXD2ndbstFitsWrite.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXD2ndbstFitsWrite_DIR)/HXD2ndbstFitsWrite.c
