/*
 filename: hmkcolor.c
 purpose:  c - wrapper for host mkcolor task
*/

#include <stdio.h>

#ifdef unix
#define MKCOLOR mkcolor_
#endif
#ifdef vms
#define MKCOLOR mkcolor
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
 MKCOLOR();
 CloseDefaultPF();
 return(RETURN);
}
