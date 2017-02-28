HXDarf_DIR = ${HXD_MODULE_DIR}/HXDarf/2.0.0

HXDarfInit  = HXDarfInit.o
HXDarf      = HXDarf.o
HXDarfMerge = HXDarfMerge.o
HXDarfout   = HXDarfout.o 


HXDarfInit.o: $(HXDarf_DIR)/HXDarfInit.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDarf_DIR)/HXDarfInit.c

HXDarf.o: $(HXDarf_DIR)/HXDarf.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDarf_DIR)/HXDarf.c

HXDarfMerge.o: $(HXDarf_DIR)/HXDarfMerge.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDarf_DIR)/HXDarfMerge.c

HXDarfout.o: $(HXDarf_DIR)/HXDarfout.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDarf_DIR)/HXDarfout.c
