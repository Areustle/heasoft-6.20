/*
 filename: hfimgcreate.c
 purpose:  c - wrapper for host FIMGCREATE task
*/

#include <stdio.h>

#ifdef unix
#define FIMGCREATE fimgce_
#endif
#ifdef vms
#define FIMGCREATE fimgce 
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
 FIMGCREATE();
 CloseDefaultPF();
 return(RETURN);
}
