/*
 filename: hmkcaldir.c
 purpose:  c - wrapper for host mkcaldir task
 author:   Ron Zellar 
*/

#include <stdio.h>

#ifdef unix
#define MKCALR mkcalr_
#endif
#ifdef vms
#define MKCALR mkcalr
#endif
#ifdef unix
#define RETURN 0
#endif
#ifdef vms
#define RETURN 1
#endif

int MAIN_; /* work around SunOS 4.1.3 bug */

main (argc,argv)
int argc;
char **argv;
{
 OpenDefaultPF(argc, argv);
 MKCALR();
 CloseDefaultPF();
 return(RETURN);
}
