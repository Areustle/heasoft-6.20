# SimASTE_PhotonGen.oSimASTE_XISarfGen.make

SimASTE_XISarfGen_DIR = $(ASTE_MODULE_DIR)/SimASTE_XISarfGen/2.9
SimASTE_XISarfGen = SimASTE_XISarfGen.o

SimASTE_XISarfGen.o: $(SimASTE_XISarfGen_DIR)/SimASTE_XISarfGen.c
	$(CC) -I$(SimASTE_Root_DIR) -I$(aeFitsHeaderUtil_DIR) -I$(xis_contami_DIR) -I${xisSciUtil_DIR} -I${xisPixelQuality_DIR} -I$(aste_gti_DIR) -I$(aste_caldb_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(SimASTE_XISarfGen_DIR)/SimASTE_XISarfGen.c
