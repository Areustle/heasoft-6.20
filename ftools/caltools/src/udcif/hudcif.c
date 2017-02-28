/*
 filename: hudcif.c
 purpose:  c - wrapper for host UDCIF task
 author:   Ron Zellar 
*/

#include <stdio.h>

#ifdef unix
#define UDCIF udcif_
#endif
#ifdef vms
#define UDCIF udcif
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
 UDCIF();
 CloseDefaultPF();
 return(RETURN);
}
