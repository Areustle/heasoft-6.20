/*
filename : hpcparpha.c
purpose  : c wrapper for host pcparpha task
author   : Ian M. George 
*/
#include <stdio.h>
#ifdef unix
#define PCPARPHA pcpara_
#endif
#ifdef vms
#define PCPARPHA pcpara
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
PCPARPHA();
CloseDefaultPF();
return(RETURN);
}

