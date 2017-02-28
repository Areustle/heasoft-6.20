/*
filename : hpcexpmap.f
purpose  : c wrapper for host pcexpmap task
author   : Rehana Yusaf
*/
#include <stdio.h>
#ifdef unix
#define PCEXPMAP pcexpp_
#endif
#ifdef vms
#define PCEXPMAP pcexpp
#endif
#ifdef unix
#define RETURN 0
#endif
#ifdef vms
#define RETURN 1
#endif

int MAIN_;

main (argc,argv)
int argc;
char **argv;
{
OpenDefaultPF(argc,argv);
PCEXPMAP();
CloseDefaultPF();
return(RETURN);
}

