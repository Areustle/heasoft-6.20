/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/ghkcurve/hghkcurve.c,v 3.6 1996/04/16 23:33:17 dunfee Exp $   */
/*                   */
/*
 filename: hghkcurve.c
 purpose:  c - wrapper for host GHKCURVE task
 author:   Jeff Guerber
 date:     Feb. 28, 1996
 Based on hghkdump.c.
*/

#include <stdio.h>

#ifdef unix
#define GHKCURVE ghkcue_
#endif
#ifdef vms
#define GHKCURVE ghkcue
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
 GHKCURVE();
 CloseDefaultPF();
 return(RETURN);
}
