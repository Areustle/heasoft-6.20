/*
 filename: hcktime.c
 purpose:  c - wrapper for host CKTIME task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define CKTIME cktime_
#endif
#ifdef vms
#define CKTIME cktime 
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
 CKTIME();
 CloseDefaultPF();
 return(RETURN);
}
