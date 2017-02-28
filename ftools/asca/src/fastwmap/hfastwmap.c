/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/fastwmap/hfastwmap.c,v 3.6 1996/04/16 23:31:27 dunfee Exp $   */
/*                   */
/*
 filename: hfastwmap.c
 purpose:  c - wrapper for host FTOOL task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FTOOL fastwp_
#endif
#ifdef vms
#define FTOOL fastwp
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
