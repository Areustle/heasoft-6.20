# aste_gti.make

aste_gti_DIR = $(ASTE_FUNCTION_DIR)/aste_gti/1.1
aste_gti = aste_gti.o

aste_gti.o: $(aste_gti_DIR)/aste_gti.c
	$(CC) -I$(aste_gti_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(aste_gti_DIR)/aste_gti.c
