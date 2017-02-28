/*
filename : hchkpha.c
purpose  : c wrapper for host chkpha task
author   : Banashree M Seifert
*/
#include <stdio.h>

#define CHKPHA chkpha_
#define RETURN 0

int MAIN_;

main (argc,argv)
int argc;
char **argv;
{
OpenDefaultPF(argc,argv);
CHKPHA();
CloseDefaultPF();
return(RETURN);
}
