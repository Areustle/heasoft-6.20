# xrsqlp.make

xrsqlp_DIR = $(XRS_MODULE_DIR)/xrsqlp/2.9
xrsqlp = xrsqlp.o

xrsqlp.o: $(xrsqlp_DIR)/xrsqlp.f
	$(FC) $(FFLAGS) $(ANLFFLAGS) -c $(xrsqlp_DIR)/xrsqlp.f
