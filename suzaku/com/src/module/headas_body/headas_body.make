# headas_body.make

headas_body_DIR = $(ASTE_MODULE_DIR)/headas_body/1.81
headas_body = headas_body.o

headas_body.o: $(headas_body_DIR)/headas_body.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(headas_body_DIR)/headas_body.c
