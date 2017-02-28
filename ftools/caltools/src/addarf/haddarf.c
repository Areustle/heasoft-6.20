 /*
  filename: haddarf.c
  purpose: c - wrapper for host ADDARF task
 */

#include <stdio.h>
#ifdef unix
#define ADDARF addarf_
#endif
#ifdef vms
#define ADDARF addarf
#endif
#ifdef unix
#define RETURN 0
#endif
#ifdef vms
#define RETURN 1
#endif

#define DEFAULT_PFILE "addarf.par"

int MAIN_; /* work around SunOS 4.1.3 bug */

main (argc,argv)
int argc;
char **argv;
{
  OpenDefaultPF(argc, argv, DEFAULT_PFILE);
  ADDARF();
  CloseDefaultPF();
  return(RETURN);
}


