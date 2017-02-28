/*
filename : hmarfrmf.c
purpose  : c wrapper for host marfrmf task
author   : Ian M. George 
*/
#include <stdio.h>
#ifdef unix
#define MARFRMF marfrf_ 
#endif
#ifdef vms
#define MARFRMF marfrf 
#endif
#ifdef unix
#define RETURN 0
#endif
#ifdef vms
#define RETURN 1
#endif

#define DEFAULT_PFILE "marfrmf.par"

int MAIN_;

main (argc,argv)
int argc;
char **argv;
{
OpenDefaultPF(argc,argv,DEFAULT_PFILE);
MARFRMF();
CloseDefaultPF();
return(RETURN);
}

