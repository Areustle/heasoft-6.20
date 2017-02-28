/*
 filename: hcrcif.c
 purpose:  c - wrapper for host CRCIF task
 author:   Ron Zellar 
*/

#include <stdio.h>

#ifdef unix
#define CRCIF crcif_
#endif
#ifdef vms
#define CRCIF crcif
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
 CRCIF();
 CloseDefaultPF();
 return(RETURN);
}
