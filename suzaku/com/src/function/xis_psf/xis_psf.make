# xis_psf.make

xis_psf_DIR = $(ASTE_FUNCTION_DIR)/xis_psf/1.6
xis_psf = xis_psf.o

xis_psf.o: $(xis_psf_DIR)/xis_psf.c
	$(CC) -I$(xis_psf_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(xis_psf_DIR)/xis_psf.c
