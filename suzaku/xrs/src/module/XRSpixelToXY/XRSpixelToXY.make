# XRSpixelToXY.make

XRSpixelToXY_DIR = $(XRS_MODULE_DIR)/XRSpixelToXY/1.7
XRSpixelToXY = XRSpixelToXY.o

XRSpixelToXY.o: $(XRSpixelToXY_DIR)/XRSpixelToXY.c
	$(CC) -I$(aeFitsHeaderUtil_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(XRSpixelToXY_DIR)/XRSpixelToXY.c
