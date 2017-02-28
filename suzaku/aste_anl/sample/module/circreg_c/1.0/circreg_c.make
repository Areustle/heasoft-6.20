# circreg_c.make

CIRCREG_C_DIR = $(ANL_MODULE_DIR)/circreg_c/1.0
CIRCREG_C = circreg_c.o

circreg_c.o: $(CIRCREG_C_DIR)/circreg_c.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(CIRCREG_C_DIR)/circreg_c.c
