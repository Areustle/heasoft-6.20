/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/timeconv/htimeconv.c,v 3.6 1996/04/16 23:44:27 dunfee Exp $   */
/*                   */
/*
 filename: htimeconv.c
 purpose:  c - wrapper for host FTOOL task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define TIMECONV timecv_
#endif
#ifdef vms
#define TIMECONV timecv
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
 TIMECONV();
 CloseDefaultPF();
 return(RETURN);
}
