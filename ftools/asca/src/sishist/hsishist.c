/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/sishist/hsishist.c,v 3.6 1996/04/16 23:35:32 dunfee Exp $   */
/*                   */
/*
 filename: hsishist.c
 purpose:  c - wrapper for host SISHIST task
 author:   Emily A. Greene
*/

#include <stdio.h>

#ifdef unix
#define SISHIST sishit_
#endif
#ifdef vms
#define SISHIST sishit 
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
 SISHIST();
 CloseDefaultPF();
 return(RETURN);
}
