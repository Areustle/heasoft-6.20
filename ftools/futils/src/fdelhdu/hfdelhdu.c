/*
 filename: hfdelhdu.c
 purpose:  c - wrapper for host FDELHDU task
*/

#include <stdio.h>


#ifdef unix
#define FDELHDU Fdelhdu
#endif
#ifdef vms
#define FDELHDU Fdelhdu
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
 void FDELHDU();

 OpenDefaultPF(argc, argv);
 FDELHDU();
 CloseDefaultPF();
 return(RETURN);
}
