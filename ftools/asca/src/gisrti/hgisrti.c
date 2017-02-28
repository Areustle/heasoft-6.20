/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/gisrti/hgisrti.c,v 3.6 1996/04/16 23:34:13 dunfee Exp $   */
/*                   */
/*
 filename: hgisrti.c
 purpose:  c - wrapper for host FTOOL task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FTOOL gisrti_
#endif
#ifdef vms
#define FTOOL gisrti
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
}
