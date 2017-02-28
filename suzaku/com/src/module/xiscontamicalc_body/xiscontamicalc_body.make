# xiscontamicalc_body.make

xiscontamicalc_body_DIR = $(ASTE_MODULE_DIR)/xiscontamicalc_body/2.1
xiscontamicalc_body = xiscontamicalc_body.o

xiscontamicalc_body.o: $(xiscontamicalc_body_DIR)/xiscontamicalc_body.c
	$(CC) -I$(xis_contami_DIR) -I$(aste_caldb_DIR) -I$(aeFitsHeaderUtil_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(xiscontamicalc_body_DIR)/xiscontamicalc_body.c
