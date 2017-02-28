# aste_gethk.make

aste_gethk_DIR = $(ASTE_FUNCTION_DIR)/aste_gethk/2.5
aste_gethk = aste_gethk.o

# obsolete
ASTE_GETHK_DIR = $(aste_gethk_DIR)
ASTE_GETHK = $(aste_gethk)

aste_gethk.o: $(ASTE_GETHK_DIR)/aste_gethk.c
	$(CC) -I$(ASTE_GETHK_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(ASTE_GETHK_DIR)/aste_gethk.c
