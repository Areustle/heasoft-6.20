HXDmkgainhist_DIR = ${HXD_MODULE_DIR}/HXDmkgainhist/2.0.1

HXDmkgainhist  = HXDmkgainhistWriteGHF.o HXDmkgainhistWritePHA.o

HXDmkgainhistWriteGHF.o: $(HXDmkgainhist_DIR)/HXDmkgainhistWriteGHF.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDmkgainhist_DIR)/HXDmkgainhistWriteGHF.c

HXDmkgainhistWritePHA.o: $(HXDmkgainhist_DIR)/HXDmkgainhistWritePHA.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDmkgainhist_DIR)/HXDmkgainhistWritePHA.c
