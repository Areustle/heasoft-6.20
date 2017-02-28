/*
 filename: hfkeypar.c
 purpose:  c - wrapper for host FKEYPAR task
 author:   James Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FKEYPAR fkeypr_
#endif
#ifdef vms
#define FKEYPAR fkeypr 
#endif
#ifdef unix
#define RETURN 0
#endif
#ifdef vms
#define RETURN 1
#endif

int MAIN_; /* work around SunOS 4.1.3 bug */

main (argc,argv)
int argc;
char **argv;
{
 OpenDefaultPF(argc, argv);
 FKEYPAR();
 CloseDefaultPF();
 return(RETURN);
}