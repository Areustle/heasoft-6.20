/*
filename : hcol2img.c
purpose  : c wrapper for host col2img task
author   : Ian M. George 
*/
#include <stdio.h>
#ifdef unix
#define COL2IMG col2ig_ 
#endif
#ifdef vms
#define COL2IMG col2ig 
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
COL2IMG();
CloseDefaultPF();
return(RETURN);
}

