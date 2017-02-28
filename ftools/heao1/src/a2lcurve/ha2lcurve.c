/*
 filename: ha2lcurve.c
 purpose:  c - wrapper for host A2LCURVE task
 author:   Lorraine Breedon
*/

#include <stdio.h>

#ifdef unix
#define A2LCURVE a2lcue_
#endif
#ifdef vms
#define A2LCURVE a2lcue
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
 A2LCURVE();
 CloseDefaultPF();
 return(RETURN);
}
