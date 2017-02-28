HXDeventFitsRead_DIR = ${HXD_MODULE_DIR}/HXDeventFitsRead/2.0.3
HXDeventFitsRead = HXDeventFitsRead.o

HXDeventFitsRead.o: $(HXDeventFitsRead_DIR)/HXDeventFitsRead.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDeventFitsRead_DIR)/HXDeventFitsRead.c
