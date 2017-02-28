# aste_caldb.make

aste_caldb_DIR = $(ASTE_FUNCTION_DIR)/aste_caldb/1.3
aste_caldb = aste_caldb.o

aste_caldb.o: $(aste_caldb_DIR)/aste_caldb.c
	$(CC) -I$(aste_caldb_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(aste_caldb_DIR)/aste_caldb.c
