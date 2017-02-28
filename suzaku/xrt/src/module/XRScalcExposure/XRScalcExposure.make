# XRScalcExposure.make

XRScalcExposure_DIR = $(XRT_MODULE_DIR)/XRScalcExposure/2.0
XRScalcExposure = XRScalcExposure.o

XRScalcExposure.o: $(XRScalcExposure_DIR)/XRScalcExposure.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(XRScalcExposure_DIR)/XRScalcExposure.c
