/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/ghkdump/hghkdump.c,v 3.6 1996/04/16 23:33:44 dunfee Exp $   */
/*                   */
/*
 filename: hghkdump.c
 purpose:  c - wrapper for host GHKDUMP task
 author:   Ken Ebisawa/Kent Blackburn
 data  : 1993/03/17
*/

#include <stdio.h>

#ifdef unix
#define GHKDUMP ghkdup_
#endif
#ifdef vms
#define GHKDUMP ghkdup
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
 GHKDUMP();
 CloseDefaultPF();
 return(RETURN);
}
