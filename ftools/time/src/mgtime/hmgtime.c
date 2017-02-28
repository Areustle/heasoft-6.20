/*
 filename: hmgtime.c
 purpose:  c - wrapper for host mgtime task
 author:   Janice Tarrant
*/

#include <stdio.h>

#ifdef unix
#define MGTIME mgtime_
#endif
#ifdef vms
#define MGTIME mgtime 
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
 MGTIME();
 CloseDefaultPF();
 return(RETURN);
}
