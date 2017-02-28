/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/mkdtime/hmkdtime.c,v 3.6 1996/04/16 23:34:38 dunfee Exp $   */
/*                   */
/*
 filename: hmkdtime.c
 purpose:  c - wrapper for host MKDTIME task
 author:   Emily A. Greene
*/

#include <stdio.h>

#ifdef unix
#define MKDTIME mkdtie_
#endif
#ifdef vms
#define MKDTIME mkdtie 
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
 MKDTIME();
 CloseDefaultPF();
 return(RETURN);
}
