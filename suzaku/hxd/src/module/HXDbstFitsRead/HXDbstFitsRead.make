HXDbstFitsRead_DIR = ${HXD_MODULE_DIR}/HXDbstFitsRead/2.0.0
HXDbstFitsRead = HXDbstFitsRead.o

HXDbstFitsRead.o: $(HXDbstFitsRead_DIR)/HXDbstFitsRead.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDbstFitsRead_DIR)/HXDbstFitsRead.c
