/*
 filename: hmaketime.c
 purpose:  c - wrapper for host maketime task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define MAKETIME makete_
#endif
#ifdef vms
#define MAKETIME makete 
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
 MAKETIME();
 CloseDefaultPF();
 return(RETURN);
}
