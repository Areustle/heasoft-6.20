HXDtableFitsWrite_DIR = ${HXD_MODULE_DIR}/HXDtableFitsWrite/0.0.7

HXDtableFitsWrite = HXDtableFitsWrite.o

HXDtableFitsWrite.o: $(HXDtableFitsWrite_DIR)/HXDtableFitsWrite.c
	$(CC) -o $@ $(CFLAGS) $(ANLCFLAGS) -c $(HXDtableFitsWrite_DIR)/HXDtableFitsWrite.c
