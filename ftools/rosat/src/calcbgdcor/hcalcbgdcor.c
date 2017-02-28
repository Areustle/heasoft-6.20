/*
filename : calcbgdcor.f
purpose  : c wrapper for host calcbgdcor task
author   : Banashree Mitra Seifert
*/
#include <stdio.h>
#ifdef unix
#define CALCBGDCOR calcbr_
#endif
#ifdef vms
#define CALCBGDCOR calcbr
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
CALCBGDCOR();
CloseDefaultPF();
return(RETURN);
}

