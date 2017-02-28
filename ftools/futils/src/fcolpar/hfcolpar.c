/*
 filename: hfcolpar.c
 purpose:  c - wrapper for host FCOLPAR task
 author:   Srilal Weera
*/

#include <stdio.h>

#ifdef unix
#define FCOLPAR fcolpr_
#endif
#ifdef vms
#define FCOLPAR fcolpr
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
 FCOLPAR();
 CloseDefaultPF();
 return(RETURN);
}
