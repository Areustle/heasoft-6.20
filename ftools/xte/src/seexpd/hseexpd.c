/*
 filename: hseexpd.c
 purpose:  c - wrapper for host SEEXPD task
 author:   Brian K. Elza
*/

#include <stdio.h>

#ifdef unix
#define SEEXPD seexpd_
#endif
#ifdef vms
#define SEEXPD seexpd 
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
 SEEXPD();
 CloseDefaultPF();
 return(RETURN);
}
