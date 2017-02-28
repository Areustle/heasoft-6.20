HXDsclFitsRead_DIR = ${HXD_MODULE_DIR}/HXDsclFitsRead/0.4.8
HXDsclFitsRead = HXDsclFitsRead.o

HXDsclFitsRead.o: $(HXDsclFitsRead_DIR)/HXDsclFitsRead.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDsclFitsRead_DIR)/HXDsclFitsRead.c
