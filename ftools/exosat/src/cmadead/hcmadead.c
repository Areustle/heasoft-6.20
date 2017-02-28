/*
 filename: hcmadead.c
 purpose:  c - wrapper for host cmadead task
*/

#include <stdio.h>

#ifdef unix
#define CMADEAD cmadead_
#endif
#ifdef vms
#define CMADEAD cmadead
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
 CMADEAD();
 CloseDefaultPF();
 return(RETURN);
}
