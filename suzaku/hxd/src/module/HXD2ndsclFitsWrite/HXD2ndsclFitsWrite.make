HXD2ndsclFitsWrite_DIR = ${HXD_MODULE_DIR}/HXD2ndsclFitsWrite/0.2.1
HXD2ndsclFitsWrite = HXD2ndsclFitsWrite.o

HXD2ndsclFitsWrite.o: $(HXD2ndsclFitsWrite_DIR)/HXD2ndsclFitsWrite.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXD2ndsclFitsWrite_DIR)/HXD2ndsclFitsWrite.c
