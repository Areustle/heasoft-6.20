/*
 filename: hdeadtime.c
 purpose:  c - wrapper for host DEADTIME task
 author:   Emily A. Greene
*/

#include <stdio.h>

#ifdef unix
#define DEADTIME deadte_
#endif
#ifdef vms
#define DEADTIME deadte 
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
 DEADTIME();
 CloseDefaultPF();
 return(RETURN);
}
