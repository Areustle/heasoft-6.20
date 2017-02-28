# aste_orbit.make

aste_orbit_DIR = $(ASTE_FUNCTION_DIR)/aste_orbit/1.2
aste_orbit = aste_orbit.o

aste_orbit.o: $(aste_orbit_DIR)/aste_orbit.c
	$(CC) -I$(aste_orbit_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(aste_orbit_DIR)/aste_orbit.c
