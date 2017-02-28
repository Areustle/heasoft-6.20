/*
 filename: hcrosscor.c
 purpose:  c - wrapper for host XRONOS CROSSCOR task
 author:   Lawrence E Brown
*/

#include <stdio.h>

#ifdef unix
#define CROSSCOR crossr_
#endif
#ifdef vms
#define CROSSCOR crossr
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
 CROSSCOR();
 CloseDefaultPF();
 return(RETURN);
}
