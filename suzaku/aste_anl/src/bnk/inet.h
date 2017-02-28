#ifndef TRUE
#define TRUE		1
#define FALSE		0
#endif

#define PORT_TYPE_NAME		1
#define PORT_TYPE_NUMBER	2

#define PORT_NAME(name)		PORT_TYPE_NAME,name,(u_short)0
#define PORT_NUMBER(number)	PORT_TYPE_NUMBER,(char *)0,(u_short)number

#define SERVER		1
#define CLIENT		2

#ifdef FD_SET_SIZE
/* 4.3BSD and SunOS 4.0(based on 4.2BSD) use wide bit sets in calls to
 * select() and provide the following macros (with different
 * implementations).  In order to keep the source reasonable, I
 * provide the following definitions. */
#ifdef FD_SET
#undef FD_SET
#undef FD_CLR
#undef FD_ZERO
#undef FD_ISSET
#endif
#define FD_SET(fd,fdset)	(fdset)->fds_bits[0] |= (1<<(fd))
#define FD_CLR(fd,fdset)	(fdset)->fds_bits[0] &= ~(1<<(fd))
#define FD_ZERO(fdset)		(fdset)->fds_bits[0] = 0
#define FD_ISSET(fd,fdset)	(((fdset)->fds_bits[0]) & (1<<(fd)))

#endif
