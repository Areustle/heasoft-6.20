/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/gqaplot/hgqaplot.c,v 3.6 1996/04/16 23:34:28 dunfee Exp $   */
/*                   */
/*
  filename: hgqaplot.c
  purpose:  c-wrapper for the host GQAPLOT task
  author:   Ken Ebisawa 
  data  : 1993/04/28
  */

#include <stdio.h>

#ifdef unix
#define GQAPLT gqaplt_
#endif
#ifdef vms
#define GQAPLT gqaplt
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
  GQAPLT();
  CloseDefaultPF();
  return(RETURN);
}
