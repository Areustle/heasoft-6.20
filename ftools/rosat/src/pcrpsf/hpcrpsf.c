/*
filename : hpcrpsf.f
purpose  : c wrapper for host pcrpsf task
author   : Rehana Yusaf
*/
#include <stdio.h>
#ifdef unix
#define PCRPSF pcrpsf_
#endif
#ifdef vms
#define PCRPSF pcrpsf
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
PCRPSF();
CloseDefaultPF();
return(RETURN);
}

