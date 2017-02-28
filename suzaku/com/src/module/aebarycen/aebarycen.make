# aebarycen.make

aebarycen_DIR = ${ASTE_MODULE_DIR}/aebarycen/1.6

aebarycen = aebarycen_body.o 

aebarycen_body.o: $(aebarycen_DIR)/aebarycen_body.c
	$(CC) -I$(axBary_DIR) -I$(aste_caldb_DIR) -I$(aste_orbit_DIR) -I$(aeFitsHeaderUtil_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(aebarycen_DIR)/aebarycen_body.c
