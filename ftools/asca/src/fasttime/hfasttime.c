/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/fasttime/hfasttime.c,v 3.6 1996/04/16 23:30:42 dunfee Exp $   */
/*                   */
/*
 filename: hfasttime.c
 purpose:  c - wrapper for host FASTTIME task
 author:   Emily A. Greene
*/

#include <stdio.h>

#ifdef unix
#define FASTTIME fastte_
#endif
#ifdef vms
#define FASTTIME fastte 
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
 FASTTIME();
 CloseDefaultPF();
 return(RETURN);
}
