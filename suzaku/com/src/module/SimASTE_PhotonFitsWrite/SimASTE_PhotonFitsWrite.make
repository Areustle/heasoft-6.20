# SimASTE_PhotonFitsWrite.make

SimASTE_PhotonFitsWrite_DIR = $(ASTE_MODULE_DIR)/SimASTE_PhotonFitsWrite/2.4
SimASTE_PhotonFitsWrite = SimASTE_PhotonFitsWrite.o

SimASTE_PhotonFitsWrite.o: $(SimASTE_PhotonFitsWrite_DIR)/SimASTE_PhotonFitsWrite.c
	$(CC) -I$(SimASTE_Root_DIR) -I$(aeFitsHeaderUtil_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(SimASTE_PhotonFitsWrite_DIR)/SimASTE_PhotonFitsWrite.c
