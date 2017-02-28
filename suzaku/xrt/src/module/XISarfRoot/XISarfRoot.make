# XISarfRoot.make

XISarfRoot_DIR = $(XRT_MODULE_DIR)/XISarfRoot/1.4
XISarfRoot = XISarfRoot.o

XISarfRoot.o: $(XISarfRoot_DIR)/XISarfRoot.c
	$(CC) -I$(aste_caldb_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(XISarfRoot_DIR)/XISarfRoot.c
