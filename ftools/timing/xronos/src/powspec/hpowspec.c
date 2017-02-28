/*
 filename: hpowspec.c
 purpose:  c - wrapper for host XRONOS POWSPEC task
 author:   Lawrence E Brown
*/

#include <stdio.h>

#ifdef unix
#define POWSPEC powspc_
#endif
#ifdef vms
#define POWSPEC powspc
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
 POWSPEC();
 CloseDefaultPF();
 return(RETURN);
}
