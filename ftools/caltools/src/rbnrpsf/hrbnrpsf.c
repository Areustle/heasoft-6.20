/*
filename : hrbnrpsf.f
purpose  : c wrapper for host rbnrpsf task
author   : Rehana Yusaf
*/
#include <stdio.h>
#ifdef unix
#define RBNRPSF rbnrpf_ 
#endif
#ifdef vms
#define RBNRPSF rbnrpf
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
RBNRPSF();
CloseDefaultPF();
return(RETURN);
}

