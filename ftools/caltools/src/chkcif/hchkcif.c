/*
 filename: hchkcif.c
 purpose:  c - wrapper for host CHKCIF task
 author:   M. Tripicco
*/

#include <stdio.h>

#ifdef unix
#define CHKCIF chkcif_
#endif
#ifdef vms
#define CHKCIF chkcif
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
 CHKCIF();
 CloseDefaultPF();
 return(RETURN);
}
