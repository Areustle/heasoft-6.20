HXD2ndeventFitsWrite_DIR = ${HXD_MODULE_DIR}/HXD2ndeventFitsWrite/2.0.6
HXD2ndeventFitsWrite = HXD2ndeventFitsWrite.o

HXD2ndeventFitsWrite.o: $(HXD2ndeventFitsWrite_DIR)/HXD2ndeventFitsWrite.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXD2ndeventFitsWrite_DIR)/HXD2ndeventFitsWrite.c
