# SimASTE_PhotonGen.make

SimASTE_PhotonGen_DIR = $(ASTE_MODULE_DIR)/SimASTE_PhotonGen/2.4
SimASTE_PhotonGen = SimASTE_PhotonGen.o

SimASTE_PhotonGen.o: $(SimASTE_PhotonGen_DIR)/SimASTE_PhotonGen.c
	$(CC) -I$(SimASTE_Root_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(SimASTE_PhotonGen_DIR)/SimASTE_PhotonGen.c
