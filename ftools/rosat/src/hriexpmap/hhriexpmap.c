/*
filename : hhriexpmap.f
purpose  : c wrapper for host hriexpmap task
author   : Rehana Yusaf
*/
#include <stdio.h>
#ifdef unix
#define HRIEXPMAP hriexp_
#endif
#ifdef vms
#define HRIEXPMAP hriexp
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
HRIEXPMAP();
CloseDefaultPF();
return(RETURN);
}

