/*
 filename: hSEBITMASK.c
 purpose:  c - wrapper for host SEBITMASK task
 author:   Brian K. Elza
*/

#include <stdio.h>

#ifdef unix
#define SEBITMASK sebitk_
#endif
#ifdef vms
#define SEBITMASK sebitk
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
 SEBITMASK();
 CloseDefaultPF();
 return(RETURN);
}
