/*
 filename: hquzcif.c
 purpose:  c - wrapper for host QUZCIF task
 author:   Ron Zellar 
*/

#include <stdio.h>

#ifdef unix
#define QUZCIF quzcif_
#endif
#ifdef vms
#define QUZCIF quzcif
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
 QUZCIF();
 CloseDefaultPF();
 return(RETURN);
}
