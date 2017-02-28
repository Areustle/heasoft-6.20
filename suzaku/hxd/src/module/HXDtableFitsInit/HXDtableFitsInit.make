HXDtableFitsInit_DIR = ${HXD_MODULE_DIR}/HXDtableFitsInit/0.1.0
HXDtableFitsInit = HXDtableFitsInit.o

HXDtableFitsInit.o: $(HXDtableFitsInit_DIR)/HXDtableFitsInit.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDtableFitsInit_DIR)/HXDtableFitsInit.c
