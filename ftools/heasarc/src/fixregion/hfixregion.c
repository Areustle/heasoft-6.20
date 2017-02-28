/*
filename : fixregion.f
purpose  : c wrapper for host fixregion task
author   : Rehana Yusaf
*/
#include <stdio.h>
#ifdef unix
#define FIXREGION fixren_
#endif
#ifdef vms
#define FIXREGION fixren
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
FIXREGION();
CloseDefaultPF();
return(RETURN);
}

