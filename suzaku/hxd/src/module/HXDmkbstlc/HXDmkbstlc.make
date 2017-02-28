HXDmkbstlc_DIR = ${HXD_MODULE_DIR}/HXDmkbstlc/2.2.4
HXDmkbstlc = HXDmkbstlc.o

HXDmkbstlc.o: $(HXDmkbstlc_DIR)/HXDmkbstlc.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDmkbstlc_DIR)/HXDmkbstlc.c
