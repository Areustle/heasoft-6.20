/*
 filename: hpcaclrsp.c
 purpose:  c - wrapper for host PCACLRSP task
 author:   Jim Lochner
*/

#include <stdio.h>

#ifdef unix
#define PCACLRSP pcaclp_
#endif
#ifdef vms
#define PCACLRSP pcaclp 
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
 PCACLRSP();
 CloseDefaultPF();
 return(RETURN);
}
