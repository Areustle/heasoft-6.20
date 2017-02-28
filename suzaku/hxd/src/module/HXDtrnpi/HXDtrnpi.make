HXDtrnpi_DIR = ${HXD_MODULE_DIR}/HXDtrnpi/2.0.1
HXDtrnpi = HXDtrnpi.o

HXDtrnpi.o: $(HXDtrnpi_DIR)/HXDtrnpi.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDtrnpi_DIR)/HXDtrnpi.c
