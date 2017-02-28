# XRSphaToPI.make

XRSphaToPI_DIR = $(XRS_MODULE_DIR)/XRSphaToPI/1.7
XRSphaToPI = XRSphaToPI.o

XRSphaToPI.o: $(XRSphaToPI_DIR)/XRSphaToPI.c
	$(CC) -I$(xrs_gain_DIR) -I$(aeFitsHeaderUtil_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(XRSphaToPI_DIR)/XRSphaToPI.c
