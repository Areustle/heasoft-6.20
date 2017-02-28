/*
 filename: hfinterp.c
 purpose:  c - wrapper for host APPHKC task
 author:   James Peachey
*/

#include <stdio.h>


#ifdef unix
#define FINTERP Finterp
#endif
#ifdef vms
#define FINTERP Finterp
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
 void FINTERP();

 OpenDefaultPF(argc, argv);
 FINTERP();
 CloseDefaultPF();
 return(RETURN);
}
