/*
 filename: hdsdlcurve.c
 purpose:  c - wrapper for host DSDLCURVE task
 author:   Lorraine Breedon
*/

#include <stdio.h>

#ifdef unix
#define DSDLCURVE dsdlce_
#endif
#ifdef vms
#define DSDLCURVE dsdlce
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
 DSDLCURVE();
 CloseDefaultPF();
 return(RETURN);
}
