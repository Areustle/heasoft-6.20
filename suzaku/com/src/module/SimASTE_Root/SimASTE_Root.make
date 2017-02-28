# SimASTE_Root.make

SimASTE_Root_DIR = $(ASTE_MODULE_DIR)/SimASTE_Root/2.5
SimASTE_Root = SimASTE_Root.o SimASTE_Util.o

SimASTE_Root.o: $(SimASTE_Root_DIR)/SimASTE_Root.c
	$(CC) -I$(SimASTE_Root_DIR) -I$(aste_caldb_DIR) $(CFLAGS) $(ANLCFLAGS) -c ${SimASTE_Root_DIR}/SimASTE_Root.c

SimASTE_Util.o: $(SimASTE_Root_DIR)/SimASTE_Util.c
	$(CC) -I$(SimASTE_Root_DIR) -I$(aeFitsHeaderUtil_DIR) -I$(aste_gti_DIR) $(CFLAGS) $(ANLCFLAGS) -c ${SimASTE_Root_DIR}/SimASTE_Util.c
