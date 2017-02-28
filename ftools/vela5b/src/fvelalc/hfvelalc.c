/*
 filename: hfvelalc.c
 purpose:  c - wrapper for host FVELALC task
 author:   Kent Blackburn (Modified for FVELALC by Jesse Allen)
*/

#include <stdio.h>

#ifdef unix
#define FVELALC fvelac_
#endif
#ifdef vms
#define FVELALC fvelac
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
 FVELALC();
 CloseDefaultPF();
 return(RETURN);
}
