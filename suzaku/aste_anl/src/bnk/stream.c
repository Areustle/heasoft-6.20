#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <netdb.h>
#include <errno.h>
#include "inet.h"

#define MAXHOSTNAMELENGTH 255
static char hostname[MAXHOSTNAMELENGTH];

extern int errno;
static int maxfds;
static struct timeval zerotime;

#define server (role == SERVER)
#define client (role == CLIENT)

int		/* returns a socket, or -1 for failure */
initport(porttype,port_name,port_number,role,sockettype,host_in)
int porttype;
char *port_name;
u_short port_number;
int role;
int sockettype;
char *host_in;			/* host to provide service */
{
    int s;		/* the socket */
    struct sockaddr_in sin;
    struct hostent *h = NULL;
    struct servent *sp = NULL;	/* used by getservbyname */

    maxfds = getdtablesize();	/* for future reference */
    /* prevent narrow fd_set binaries from blowing up with with */
    /* large getdtablesize() */
#ifdef FD_SET_SIZE
    maxfds = ((FD_SET_SIZE > maxfds)?maxfds:FD_SET_SIZE);
#endif

    zerotime.tv_sec = zerotime.tv_usec = 0L;

    if (client) {
	if (host_in && strcmp(host_in,"")) strcpy(hostname,host_in);
	else {
	    if (gethostname(hostname,MAXHOSTNAMELENGTH)) {
		perror("initport: gethostname");
		return(-1);
	    }
	}

	if (!(h = gethostbyname(hostname))) {
	    fprintf(stderr,"initport: gethostbyname(%s): failed\n",hostname);
	    return(-1);
	}
    }

    if (porttype == PORT_TYPE_NAME) {
	if (!(sp = getservbyname(port_name,(char *)0))) {
            fprintf(stderr,"initport: getservbyname(%s): failed\n",port_name);
            exit(-1);
	}
    }

    if (-1 == (s = socket(AF_INET,sockettype,0))) {
	perror("initport: socket");
	return(-1);
    }

    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = (server?INADDR_ANY:*(u_long *) h->h_addr);
    sin.sin_port = (porttype == PORT_TYPE_NAME?sp->s_port:htons(port_number));

    if (client) {
	if (connect(s,(struct sockaddr *)&sin,sizeof(struct sockaddr_in))) {
	    perror("initport: connect");
	    return(-1);
	}
    } else {
	/* bind the socket */
	/* following line allows reuse immediately after previous user */
	/* otherwise you have to wait a minute or two */
	setsockopt(s,SOL_SOCKET,SO_REUSEADDR,(char *)0,0);
	if (-1 == (bind(s,(struct sockaddr *)&sin,sizeof(sin)))) {
		perror("initport: bind");
		return(-1);
	}
	if (listen(s,1)) {
		perror("initport: listen");
		return(-1);
	}
    }
    return(s);
}

int
select_server_stream(connection_socket,readers)
int connection_socket;
fd_set *readers;	/* file descriptors of client sockets */
{
    struct sockaddr_in from;
    int fromlen;
    static int fd;	/* next file descriptor to look at */
    int c;
    fd_set readfds;
    int user;

    /* how do you get sockets to block?  there is some hint (recv(2)) */
    /* in the manual that you can but I can't find the reference! */

    /* select does not like bogus file descriptors, so keep track of */
    /* them by hand */
    FD_SET(connection_socket,readers);

restart:
    do {
	/* save readers because select wipes them out */
	readfds = *readers;
	c = select(maxfds,&readfds,(fd_set *)0,(fd_set *)0,
						(struct timeval *)0);
	if (c == -1) {
	    if (errno == EBADF) {
		int i;
		/* someone augered in, lets forget about'm */
		for (i=0;i<maxfds;i++) {
		    fd_set suspect;
		    FD_ZERO(&suspect);
		    if (FD_ISSET(i,readers)) {
			/* use a temporary for the suspect because select() */
			/* requires an address */
			FD_SET(i,&suspect);
			if (-1 == select(maxfds,&suspect,
						(fd_set *)0,(fd_set *)0,
						&zerotime)) {
			    /* found a reader who closed his socket */
			    /* so get rid of him */
			    FD_CLR(i,readers);
			}
			FD_CLR(i,&suspect);
		    }
		}
	    } else {
		/* lets hope it was a recoverable interrupt and try again */
		perror("select_server_stream: select");
		exit(-1);
	    }
	}
    } while (c == -1);
    /* given the set of ready file descriptors pick one out that is ready */
    /* start from where we left off, so as to give everyone service */
    while (TRUE) {
	fd = (1+fd)%maxfds;
	if (FD_ISSET(fd,&readfds)) break;
    }

    if (fd == connection_socket) {	/* check for new connections */
	fromlen = sizeof(from);
	user = accept(connection_socket,(struct sockaddr *)&from,&fromlen);
	FD_SET(user,readers);
	goto restart;
    }

    return(fd);
}

void
print_address(x)
struct sockaddr_in *x;
{
	printf("x->sin_family = %d\n",(int)x->sin_family);
	printf("x->sin_port = %d\n",(int)x->sin_port);
	printf("x->sin_addr.s_addr = %d\n",(int)x->sin_port);
	printf("x->sin_zero[0] = %c\n",x->sin_zero[0]);
}
