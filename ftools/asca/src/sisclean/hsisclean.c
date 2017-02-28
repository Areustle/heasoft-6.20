/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/sisclean/hsisclean.c,v 3.6 1996/04/16 23:21:26 dunfee Exp $   */
/*                   */
/*
 filename: hsisclean.c
 purpose:  c - wrapper for host SISCLEAN task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define SISCLEAN siscln_
#endif
#ifdef vms
#define SISCLEAN siscln
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
 SISCLEAN();
 CloseDefaultPF();
 return(RETURN);
}

