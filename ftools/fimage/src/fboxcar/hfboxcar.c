/*
 filename: hfboxcar.c
 purpose:  c - wrapper for host FBOXCAR task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FBOXCAR fboxcr_
#endif
#ifdef vms
#define FBOXCAR fboxcr 
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
 FBOXCAR();
 CloseDefaultPF();
 return(RETURN);
}
