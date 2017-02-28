/* sized_io.c - Preserve message boundaries in stream io.

These two routines enable us to have use stream io, but still detect end of
record marks.  Each call to sized_read() returns a complete buffer, that is
what was written by one call to sized_write().

Notes:

The IPC system seems to be a confusing mess.  I.e. unusual conditions are
handled in all different ways.  Specifically,

While we are reading, if the writer goes away, we sometimes get a read()
== -1 && errno == ECONNRESET.  Sometimes we get a read() == 0.  Why the
difference?

While we are writing, if the reader goes away, we get a signal (SIGPIPE).


Don Libes
National Institute of Standards and Technology
(301) 975-3535
libes@cme.nist.gov
...!uunet!cme-durer!libes

*/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
extern int errno;
#include <unistd.h>
#include <sys/types.h>		/* defines u_long */
#include <netinet/in.h>		/* defines htonl(), etc */

/* read exactly this many bytes */
/* We may be forced to loop doing reads, since occasionally reads return */
/* only partial buffers.  Surprisingly, this happens even when reading */
/* tiny (4-byte) buffers. */
/* should probably make an exact_write() also */
static int
exact_read(fd,buffer,size)
int fd;
char *buffer;
int size;
{
	int rembytes = size;
	int cc;

	while (rembytes) {
		if (-1 == (cc = read(fd,buffer,rembytes))) {
			fprintf(stderr,"sized_read(,,%d) = read(,,%d) = %d\n",
							size,rembytes,cc);
			if (errno != ECONNRESET) perror("read");
			return(-1);
		}

		if (0 == cc) {	/* EOF - process died */
			return(-1);
		}

#ifdef DEBUG
		if (rembytes != cc)
			fprintf(stderr,"sized_read(,,%d) = read(,,%d) = %d\n",
							size,rembytes,cc);
#endif
		/* read() returned more bytes than requested!?!?!?! */
		/* this can't happen, but appears to be anyway */
		if (cc > rembytes) {
			fprintf(stderr,"sized_read(,,%d) = read(,,%d) = %d!?!?!\n",
							size,rembytes,cc);
			fprintf(stderr,"read() returned more chars than requested!  Aborting program.\n");
			abort();
		}
		buffer += cc;
		rembytes -= cc;
	}
	return(size);
}


/* skip exactly this many bytes */
static int
exact_skip(fd,size)
int fd;
int size;
{
	int rembytes = size;
	int cc;

	while (rembytes) {
		int readbytes;
		char buffer[1024];

		readbytes = rembytes;
		if ( sizeof(buffer) < readbytes ) {
			readbytes = sizeof(buffer);
		}

		if (-1 == (cc = read(fd,buffer,readbytes))) {
			fprintf(stderr,"sized_read(,,%d) = read(,,%d) = %d\n",
							size,rembytes,cc);
			if (errno != ECONNRESET) perror("read");
			return(-1);
		}

		if (0 == cc) {	/* EOF - process died */
			return(-1);
		}

#ifdef DEBUG
		if (rembytes != cc)
			fprintf(stderr,"sized_read(,,%d) = read(,,%d) = %d\n",
							size,rembytes,cc);
#endif
		/* read() returned more bytes than requested!?!?!?! */
		/* this can't happen, but appears to be anyway */
		if (cc > rembytes) {
			fprintf(stderr,"sized_read(,,%d) = read(,,%d) = %d!?!?!\n",
							size,rembytes,cc);
			fprintf(stderr,"read() returned more chars than requested!  Aborting program.\n");
			abort();
		}
		/*buffer += cc;*/
		rembytes -= cc;
	}
	return(size);
}


int	/* returns number of bytes read or -1 if error (i.e. EOF) */
sized_read(fd,buffer,maxbytes)
int fd;
char *buffer;
int maxbytes;	/* unlike read(), this parameter is the maximum size of */
		/* the buffer */
{
	int size;	/* size of incoming packet */
	/*u_long netlong;*/	/* network byte ordered length */
	u_int netlong;	/* network byte ordered length */

	/* read header */
	if (-1 == exact_read(fd,(char *)&netlong,sizeof(netlong))) return(-1);
	size = ntohl(netlong);

	/* read data */
	if (size == 0) return(0);
	else if (size > maxbytes) {
		fprintf(stderr,"sized_read: buffer too small.  ");
		fprintf(stderr,"buffer size was %d  actual size was %d\n",
			maxbytes,size);
		return(-1);
	}
	return(exact_read(fd,buffer,size));
}


int	/* returns number of bytes read or -1 if error (i.e. EOF) */
sized_read_skip(fd,buffer,maxbytes,databytes)
int fd;
char *buffer;
int maxbytes;	/* unlike read(), this parameter is the maximum size of */
		/* the buffer */
int *databytes;	/* length of the data packet */
{
	int size;	/* size of incoming packet */
	/*u_long netlong;*/	/* network byte ordered length */
	u_int netlong;	/* network byte ordered length */

	/* read header */
	if (-1 == exact_read(fd,(char *)&netlong,sizeof(netlong))) {
		*databytes = -1;
		return(-1);
	}
	*databytes = size = ntohl(netlong);

	/* read data */
	if (size == 0) {
		return(0);
	} else if (size > maxbytes) {
		int len = exact_read(fd,buffer,maxbytes);
		if ( 0 <= len ) {
			exact_skip(fd,size-len);
		}
		return len;
	}

	return(exact_read(fd,buffer,size));
}


int	/* returns number of data bytes written or -1 if error */
sized_write(fd,buffer,nbytes)
int fd;
char *buffer;
int nbytes;
{
	int cc;
	int rembytes;
	/*u_long netlong;*/	/* network byte ordered length */
	u_int netlong;	/* network byte ordered length */

	/* write header */
	netlong = htonl(nbytes);
	if (sizeof(nbytes) != (cc = write(fd,(char *)&netlong,
							sizeof(netlong)))) {
#ifdef DEBUG
		/* this can never happen (SIGPIPE will always occur first) */
		fprintf(stderr,"sized_write: tried to write buffer size but only wrote %d chars\n",cc);
#endif
		if (cc == -1) perror("write");
		return(-1);
	}

	/* write data */
	if (nbytes == 0) return(0);

	rembytes = nbytes;
	while (rembytes) {
		if (-1 == (cc = write(fd,buffer,rembytes))) {
		      fprintf(stderr,"sized_write(,,%d) = write(,,%d) = %d\n",
							nbytes,rembytes,cc);
			perror("write");
			return(-1);
		}
#ifdef DEBUG
		if (rembytes != cc) 
		      fprintf(stderr,"sized_write(,,%d) = write(,,%d) = %d\n",
							nbytes,rembytes,cc);
#endif
		buffer += cc;
		rembytes -= cc;
	}
	return(nbytes);
}
