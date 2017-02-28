/*
 filename: hstcal.c
 purpose:  c - wrapper for host stcal task
 author:   Ron Zellar 
*/

#include <stdio.h>

#ifdef unix
#define STCAL stcal_
#endif
#ifdef vms
#define STCAL stcal
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
 STCAL();
 CloseDefaultPF();
 return(RETURN);
}
