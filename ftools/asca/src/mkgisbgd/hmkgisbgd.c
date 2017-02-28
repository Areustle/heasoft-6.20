/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/mkgisbgd/hmkgisbgd.c,v 1.1 1999/10/08 21:06:03 peachey Exp $   */
/*                   */
/*
  filename: hmkgisbgd.c
  purpose:  c-wrapper for the host MKGISBGD task
  author:   Ken Ebisawa 
  data  : 1999/09/24
  */

#include <stdio.h>

#ifdef unix
#define MKGISBGD mkgisbgd_
#endif
#ifdef vms
#define MKGISBGD mkgisbgd
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
  MKGISBGD();
  CloseDefaultPF();
  return(RETURN);
}
