# circreg_f.make

CIRCREG_F_DIR = $(ANL_MODULE_DIR)/circreg_f/1.0
CIRCREG_F = circreg_f.o

circreg_f.o: $(CIRCREG_F_DIR)/circreg_f.f
	$(FC) $(FFLAGS) $(ANLFFLAGS) -c $(CIRCREG_F_DIR)/circreg_f.f
