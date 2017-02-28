/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/ascalin/hascalin.c,v 3.6 1996/04/16 23:19:56 dunfee Exp $   */
/*                   */
/*
 filename: hascalin.c
 purpose:  c - wrapper for host FTOOL task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FTOOL ascaln_
#endif
#ifdef vms
#define FTOOL ascaln
#endif
#ifdef unix
#define RETURN 0
#endif
#ifdef vms
#define RETURN 1
#endif

int MAIN_;   /* work around SunOS 4.1.3 bug */

main (argc, argv)
int argc;
char **argv;
{
 OpenDefaultPF(argc, argv);
 FTOOL();
 CloseDefaultPF();
 return(RETURN);
}
