HXDgethkInit_DIR = ${HXD_MODULE_DIR}/HXDgethkInit/0.1.0
HXDgethkInit = HXDgethkInit.o

HXDgethkInit.o: $(HXDgethkInit_DIR)/HXDgethkInit.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDgethkInit_DIR)/HXDgethkInit.c
