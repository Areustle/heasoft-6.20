/*
 filename: hmudcif.c
 purpose:  c - wrapper for host MUDCIF task
 author:   L. Breedon
*/

#include <stdio.h>

#ifdef unix
#define MUDCIF mudcif_
#endif
#ifdef vms
#define MUDCIF mudcif
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
 MUDCIF();
 CloseDefaultPF();
 return(RETURN);
}
