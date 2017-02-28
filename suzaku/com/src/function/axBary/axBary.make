# axBary.make

axBary_DIR = ${ASTE_FUNCTION_DIR}/axBary/1.3

axBary = bary.o dpleph.o clock.o ctatv.o scorbit.o xCC.o

bary.o: $(axBary_DIR)/bary.c
	$(CC) -I$(axBary_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(axBary_DIR)/bary.c

dpleph.o: $(axBary_DIR)/dpleph.c
	$(CC) -I$(axBary_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(axBary_DIR)/dpleph.c

ctatv.o: $(axBary_DIR)/ctatv.c
	$(CC) -I$(axBary_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(axBary_DIR)/ctatv.c

clock.o: $(axBary_DIR)/clock.c
	$(CC) -I$(axBary_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(axBary_DIR)/clock.c

scorbit.o: $(axBary_DIR)/scorbit.c
	$(CC) -I$(axBary_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(axBary_DIR)/scorbit.c

xCC.o: $(axBary_DIR)/xCC.c
	$(CC) -I$(axBary_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(axBary_DIR)/xCC.c
