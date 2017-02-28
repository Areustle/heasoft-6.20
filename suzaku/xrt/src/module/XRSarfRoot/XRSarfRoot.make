# XRSarfRoot.make

XRSarfRoot_DIR = $(XRT_MODULE_DIR)/XRSarfRoot/1.4
XRSarfRoot = XRSarfRoot.o

XRSarfRoot.o: $(XRSarfRoot_DIR)/XRSarfRoot.c
	$(CC) -I$(aste_caldb_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(XRSarfRoot_DIR)/XRSarfRoot.c
