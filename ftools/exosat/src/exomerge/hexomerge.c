/*
 filename: hexomerge.c
 purpose:  c - wrapper for host exomerge task
*/

#include <stdio.h>

#ifdef unix
#define EXOMERGE exomerge_
#endif
#ifdef vms
#define EXOMERGE exomerge
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
 EXOMERGE();
 CloseDefaultPF();
 return(RETURN);
}
