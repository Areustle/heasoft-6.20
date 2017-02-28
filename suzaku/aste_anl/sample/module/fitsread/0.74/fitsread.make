# fitsread.make

FITSREAD_DIR = $(ANL_MODULE_DIR)/fitsread/0.74
FITSREAD = fitsread.o

fitsread.o: $(FITSREAD_DIR)/fitsread.f
	$(FC) $(FFLAGS) $(ANLFFLAGS) -c $(FITSREAD_DIR)/fitsread.f
