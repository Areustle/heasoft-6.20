/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/attitude/hattitude.c,v 3.6 1996/04/16 23:20:26 dunfee Exp $   */
/*                   */
/*
 filename: hattitude.c
 purpose:  c - wrapper for host FTOOL task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FTOOL attite_
#endif
#ifdef vms
#define FTOOL attite
#endif
#ifdef unix
#define RETURN 0
#endif
#ifdef vms
#define RETURN 1
#endif

int MAIN_; /* work around SunOS 4.1.3 bug */

main (argc, argv)
int argc;
char **argv;
{
 OpenDefaultPF(argc, argv);
 FTOOL();
 CloseDefaultPF();
 return(RETURN);
}
