/*
 filename: hfltime.c
 purpose:  c - wrapper for host fltime task
 author:   Janice Tarrant
*/

#include <stdio.h>

#ifdef unix
#define FLTIME fltime_
#endif
#ifdef vms
#define FLTIME fltime 
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
 FLTIME();
 CloseDefaultPF();
 return(RETURN);
}
