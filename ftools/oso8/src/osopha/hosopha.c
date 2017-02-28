/*
 filename: hosopha.c
 purpose:  c - wrapper for host osopha task
 author:   David Dawson
*/

#include <stdio.h>

#ifdef unix
#define OSOPHA osopha_
#endif
#ifdef vms
#define OSOPHA osopha 
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
 OSOPHA();
 CloseDefaultPF();
 return(RETURN);
}
