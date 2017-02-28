/*
 filename: hlcmath.c
 purpose:  c - wrapper for host LCMATH task
 author:   Dr. Emily A. Greene
*/

#include <stdio.h>

#ifdef unix
#define LCMATH lcmath_
#endif
#ifdef vms
#define LCMATH lcmath 
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
 LCMATH();
 CloseDefaultPF();
 return(RETURN);
}
