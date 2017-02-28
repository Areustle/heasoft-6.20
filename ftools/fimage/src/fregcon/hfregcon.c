/*
 filename: hfregcon.c
 purpose:  c - wrapper for host FREGCON task
 author:   Srilal Weera
*/

#include <stdio.h>

#ifdef unix
#define FREGCON fregcn_
#endif
#ifdef vms
#define FREGCON fregcn
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
 FREGCON();
 CloseDefaultPF();
 return(RETURN);
}
