/*
filename : hrpsfqdp.c
purpose  : c wrapper for host rpsfqdp task
author   : Rehana Yusaf
*/
#include <stdio.h>
#ifdef unix
#define RPSFQP rpsfqp_
#endif
#ifdef vms
#define RPSFQP rpsfqp
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
OpenDefaultPF(argc,argv);
RPSFQP();
CloseDefaultPF();
return(RETURN);
}
