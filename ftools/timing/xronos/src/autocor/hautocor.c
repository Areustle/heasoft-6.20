/*
 filename: hautocor.c
 purpose:  c - wrapper for host XRONOS AUTOCOR task
 author:   Lawrence E Brown
*/

#include <stdio.h>

#ifdef unix
#define AUTOCOR autocr_
#endif
#ifdef vms
#define AUTOCOR autocr
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
 AUTOCOR();
 CloseDefaultPF();
 return(RETURN);
}
