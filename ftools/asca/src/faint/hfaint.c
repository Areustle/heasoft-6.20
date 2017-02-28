/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/faint/hfaint.c,v 3.6 1996/04/16 23:26:52 dunfee Exp $   */
/*                   */
/*
 filename: hfaint.c
 purpose:  c - wrapper for host faint task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FAINT faint_
#endif
#ifdef vms
#define FAINT faint 
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
 FAINT();
 CloseDefaultPF();
 return(RETURN);
}
