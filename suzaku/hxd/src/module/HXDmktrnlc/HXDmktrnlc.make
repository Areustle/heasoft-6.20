HXDmktrnlc_DIR = ${HXD_MODULE_DIR}/HXDmktrnlc/2.0.1
HXDmktrnlc = HXDmktrnlc.o

HXDmktrnlc.o: $(HXDmktrnlc_DIR)/HXDmktrnlc.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDmktrnlc_DIR)/HXDmktrnlc.c

