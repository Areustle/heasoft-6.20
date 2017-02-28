HXDtrnFitsRead_DIR = ${HXD_MODULE_DIR}/HXDtrnFitsRead/0.4.3
HXDtrnFitsRead = HXDtrnFitsRead.o

HXDtrnFitsRead.o: $(HXDtrnFitsRead_DIR)/HXDtrnFitsRead.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDtrnFitsRead_DIR)/HXDtrnFitsRead.c
