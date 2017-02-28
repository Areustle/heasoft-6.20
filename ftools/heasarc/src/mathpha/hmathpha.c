/*
filename : hmathpha.c
purpose  : c wrapper for host mathpha task
author   : Ian M. George 
*/
#include <stdio.h>
#ifdef unix
#define MATHPHA mathpa_
#endif
#ifdef vms
#define MATHPHA mathpa
#endif
#ifdef unix
#define RETURN 0
#endif
#ifdef vms
#define RETURN 1
#endif

#define DEFAULT_PFILE "mathpha.par"

int MAIN_;

main (argc,argv)
int argc;
char **argv;
{
OpenDefaultPF(argc,argv,DEFAULT_PFILE);
MATHPHA();
CloseDefaultPF();
return(RETURN);
}

