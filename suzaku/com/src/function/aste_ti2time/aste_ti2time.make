# aste_ti2time.make

aste_ti2time_DIR = $(ASTE_FUNCTION_DIR)/aste_ti2time/4.1
aste_ti2time = aste_ti2time.o

aste_ti2time.o: $(aste_ti2time_DIR)/aste_ti2time.c
	$(CC) -I$(aste_ti2time_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(aste_ti2time_DIR)/aste_ti2time.c
