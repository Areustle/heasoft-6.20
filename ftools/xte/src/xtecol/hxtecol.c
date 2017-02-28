/*
 filename: hxtecol.c
 purpose:  c - wrapper for host xtecol task
 author:   Jim Lochner
*/

#include <stdio.h>

#ifdef unix
#define XTECOL xtecol_
#endif
#ifdef vms
#define XTECOL xtecol 
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
 XTECOL();
 CloseDefaultPF();
 return(RETURN);
}
