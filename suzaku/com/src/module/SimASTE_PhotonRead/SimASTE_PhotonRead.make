# SimASTE_PhotonRead.make

SimASTE_PhotonRead_DIR = $(ASTE_MODULE_DIR)/SimASTE_PhotonRead/2.3
SimASTE_PhotonRead = SimASTE_PhotonRead.o

SimASTE_PhotonRead.o: $(SimASTE_PhotonRead_DIR)/SimASTE_PhotonRead.c
	$(CC) -I$(SimASTE_Root_DIR) -I$(aste_gti_DIR) -I$(aeFitsHeaderUtil_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(SimASTE_PhotonRead_DIR)/SimASTE_PhotonRead.c
