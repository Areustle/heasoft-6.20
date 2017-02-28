/*
filename : hhrirpsf.f
purpose  : c wrapper for host hrirpsf task
author   : Rehana Yusaf
*/
#include <stdio.h>
#ifdef unix
#define HRIRPSF hrirpf_
#endif
#ifdef vms
#define HRIRPSF hrirpf
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
HRIRPSF();
CloseDefaultPF();
return(RETURN);
}

