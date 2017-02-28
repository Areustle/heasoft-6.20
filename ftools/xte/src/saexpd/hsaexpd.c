/*
 filename: hsaexpd.c
 purpose:  c - wrapper for host SAEXPD task
 author:   Brian K. Elza
*/

#include <stdio.h>

#ifdef unix
#define SAEXPD saexpd_
#endif
#ifdef vms
#define SAEXPD saexpd 
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
 SAEXPD();
 CloseDefaultPF();
 return(RETURN);
}
