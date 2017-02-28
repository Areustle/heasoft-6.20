/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/dfe2mkf/hdfe2mkf.c,v 3.6 1996/04/16 23:25:54 dunfee Exp $   */
/*                   */
/*
 filename: hdfe2mkf.c
 purpose:  c - wrapper for host DFE2MKF task
 author:   Emily A. Greene
*/

#include <stdio.h>

#ifdef unix
#define DFE2MKF dfe2mf_
#endif
#ifdef vms
#define DFE2MKF dfe2mf 
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
 DFE2MKF();
 CloseDefaultPF();
 return(RETURN);
}
