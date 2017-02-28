# aste_aspect.make

aste_aspect_DIR = $(ASTE_FUNCTION_DIR)/aste_aspect/1.8
aste_aspect = aste_aspect.o

aste_aspect.o: $(aste_aspect_DIR)/aste_aspect.c
	$(CC) -I$(aeFitsHeaderUtil_DIR) -I$(aste_aspect_DIR) $(CFLAGS) $(ANLCFLAGS) -c ${aste_aspect_DIR}/aste_aspect.c
