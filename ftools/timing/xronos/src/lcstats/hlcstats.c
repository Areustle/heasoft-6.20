/*
 filename: hlcstats.c
 purpose:  c - wrapper for host XRONOS LCSTATS task
 author:   Lawrence E Brown
*/

#include <stdio.h>

#ifdef unix
#define LCSTATS lcstas_
#endif
#ifdef vms
#define LCSTATS lcstas
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
 LCSTATS();
 CloseDefaultPF();
 return(RETURN);
}
