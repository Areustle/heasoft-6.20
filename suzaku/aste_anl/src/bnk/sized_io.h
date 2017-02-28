/* sized_io.c */

int sized_read(int fd, void *buffer, int maxbytes);
int sized_read_skip(int fd, void *buffer, int maxbytes, int *databytes);
int sized_write(int fd, void *buffer, int nbytes);

/* stream.c */
int initport(int porttype, char *port_name, int port_number, int role, int sockettype, char *host_in);
int select_server_stream(int connection_socket, fd_set *readers);
void print_address(struct sockaddr_in *x);
