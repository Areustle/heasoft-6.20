# XRSpixelCoincidence.make

XRSpixelCoincidence_DIR = $(XRS_MODULE_DIR)/XRSpixelCoincidence/2.1
XRSpixelCoincidence = XRSpixelCoincidence.o

XRSpixelCoincidence.o: $(XRSpixelCoincidence_DIR)/XRSpixelCoincidence.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(XRSpixelCoincidence_DIR)/XRSpixelCoincidence.c
