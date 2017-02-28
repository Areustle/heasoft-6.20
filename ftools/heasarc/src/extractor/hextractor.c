 /*
  purpose: c - wrapper for host EXTRACTOR task
  filename: hextractor.c
  author:  Keith Arnaud
 */

#include <stdio.h>
#ifdef unix
#define EXTRACTOR extrct_
#endif
#ifdef vms
#define EXTRACTOR extrct
#endif
#ifdef unix
#define RETURN 0
#endif
#ifdef vms
#define RETURN 1
#endif

#define DEFAULT_PFILE "extractor.par"

int MAIN_; /* work around SunOS 4.1.3 bug */

main (argc,argv)
int argc;
char **argv;
{
  OpenDefaultPF(argc, argv, DEFAULT_PFILE);
  EXTRACTOR();
  CloseDefaultPF();
  return(RETURN);
}
