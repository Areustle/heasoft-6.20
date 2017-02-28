/*
 filename: htrans2fits.c
 purpose:  c - wrapper for host TRANS2FITS task
 author:   Brian K. Elza
*/

#include <stdio.h>

#ifdef unix
#define TRANS2FITS Trans2fits
#endif
#ifdef vms
#define TRANS2FITS Trans2fits
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
  void TRANS2FITS();

  OpenDefaultPF(argc, argv);
  TRANS2FITS();
  CloseDefaultPF();
  return(RETURN);
}
