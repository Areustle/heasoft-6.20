 /*
  filename: hgenrsp.c
  purpose: c - wrapper for host GENRSP task
  author:  Keith Arnaud
 */

#include <stdio.h>
#ifdef unix
#define GENRSP genrsp_
#endif
#ifdef vms
#define GENRSP genrsp
#endif
#ifdef unix
#define RETURN 0
#endif
#ifdef vms
#define RETURN 1
#endif

#define DEFAULT_PFILE "genrsp.par"

int MAIN_; /* work around SunOS 4.1.3 bug */

main (argc,argv)
int argc;
char **argv;
{
  OpenDefaultPF(argc, argv, DEFAULT_PFILE);
  GENRSP();
  CloseDefaultPF();
  return(RETURN);
}


