# xis_effarea.make

xis_effarea_DIR = $(ASTE_FUNCTION_DIR)/xis_effarea/1.6
xis_effarea = xis_effarea.o

xis_effarea.o: $(xis_effarea_DIR)/xis_effarea.c
	$(CC) -I$(xis_effarea_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(xis_effarea_DIR)/xis_effarea.c
