/*
 filename: hfmodhead.c
 purpose:  c - wrapper for host FMODHEAD task
 author:   Vidya Sagar 
*/

#include <stdio.h>

#ifdef unix
#define FMODHEAD fmodhd_
#endif
#ifdef vms
#define FMODHEAD fmodhd
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
 FMODHEAD();
 CloseDefaultPF();
 return(RETURN);
}
