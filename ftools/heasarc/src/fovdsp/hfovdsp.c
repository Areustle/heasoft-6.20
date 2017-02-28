/*
  filename: hfovdsp.c
  purpose:  c-wrapper for the host FOVDSP task
  author:   Ken Ebisawa 
  data  : 2004/08/20
  */

#include <stdio.h>

#ifdef unix
#define FOVDSP fovdsp_
#endif
#ifdef vms
#define FOVDSP fovdsp
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
  FOVDSP();
  CloseDefaultPF();
  return(RETURN);
}
