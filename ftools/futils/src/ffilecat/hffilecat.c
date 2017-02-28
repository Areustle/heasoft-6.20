/*
 filename: ffilecat.c
 purpose:  c - wrapper for host FFILECAT task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FFILECAT ffilet_
#endif
#ifdef vms
#define FFILECAT ffilet
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
 FFILECAT();
 CloseDefaultPF();
 return(RETURN);
}
