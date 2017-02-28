/*
 filename: hearth2sun.c
 purpose:  c - wrapper for host XRONOS EARTH2SUN task
 author:   Lawrence E Brown
*/

#include <stdio.h>

#ifdef unix
#define EARTH2SUN earthn_
#endif
#ifdef vms
#define EARTH2SUN earthn
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
 EARTH2SUN();
 CloseDefaultPF();
 return(RETURN);
}
