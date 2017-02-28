# xrs_gain.make

xrs_gain_DIR = $(XRS_FUNCTION_DIR)/xrs_gain/1.7
xrs_gain = xrs_gain.o

xrs_gain.o: $(xrs_gain_DIR)/xrs_gain.c
	$(CC) -I$(xrs_gain_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(xrs_gain_DIR)/xrs_gain.c
