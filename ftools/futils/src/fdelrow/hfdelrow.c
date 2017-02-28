/*
 filename: hfdelrow.c
 purpose:  c - wrapper for host FDELROW task
*/

#include <stdio.h>


#ifdef unix
#define FDELROW Fdelrow
#endif
#ifdef vms
#define FDELROW Fdelrow
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
 void FDELROW();

 OpenDefaultPF(argc, argv);
 FDELROW();
 CloseDefaultPF();
 return(RETURN);
}
