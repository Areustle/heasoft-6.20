# xis_contami.make

xis_contami_DIR = $(ASTE_FUNCTION_DIR)/xis_contami/1.3
xis_contami = xis_contami.o

xis_contami.o: $(xis_contami_DIR)/xis_contami.c
	$(CC) -I$(xis_contami_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(xis_contami_DIR)/xis_contami.c
