# XRSeventFitsRDWT.make

XRSeventFitsRDWT_DIR = $(XRS_MODULE_DIR)/XRSeventFitsRDWT/2.1
XRSeventFitsRDWT = XRSeventFitsRDWT.o

XRSeventFitsRDWT.o: $(XRSeventFitsRDWT_DIR)/XRSeventFitsRDWT.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(XRSeventFitsRDWT_DIR)/XRSeventFitsRDWT.c

clean::
	rm -f XRSeventFitsRDWT
