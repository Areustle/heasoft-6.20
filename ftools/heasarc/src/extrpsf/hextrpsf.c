/*
filename : extrpsf.f
purpose  : c wrapper for host xrpsf task
author   : Rehana Yusaf
*/
#include <stdio.h>
#ifdef unix
#define EXTRPSF extrpf_
#endif
#ifdef vms
#define EXTRPSF extrpf
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
EXTRPSF();
CloseDefaultPF();
return(RETURN);
}

