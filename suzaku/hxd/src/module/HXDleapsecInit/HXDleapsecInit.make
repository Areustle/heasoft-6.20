HXDleapsecInit_DIR = ${HXD_MODULE_DIR}/HXDleapsecInit/2.0.1
HXDleapsecInit = HXDleapsecInit.o

HXDleapsecInit.o: $(HXDleapsecInit_DIR)/HXDleapsecInit.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDleapsecInit_DIR)/HXDleapsecInit.c
