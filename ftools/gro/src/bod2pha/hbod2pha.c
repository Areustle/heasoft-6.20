/*
 filename: hbod2pha.c
 purpose: c - wrapper for host BOD2PHA task
 author/date: Peter J.T. Leonard, October, 1997
*/

#include <stdio.h>

#ifdef unix
#define BOD2PHA bod2pa_
#endif
#ifdef vms
#define BOD2PHA bod2pa
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
 BOD2PHA();
 CloseDefaultPF();
 return(RETURN);
}
