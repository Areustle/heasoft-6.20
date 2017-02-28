/*
 filename: hxenon2fits.c
 purpose:  c - wrapper for host XENON2FITS task
 author:   Brian K. Elza
*/

#include <stdio.h>

#ifdef unix
#define XENON2FITS Xenon2fits
#endif
#ifdef vms
#define XENON2FITS Xenon2fits
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
  void XENON2FITS();

  OpenDefaultPF(argc, argv);
  XENON2FITS();
  CloseDefaultPF();
  return(RETURN);
}
