 /*
  filename: haddrmf.c
  purpose: c - wrapper for host ADDRMF task
  author:  Keith Arnaud
 */

#include <stdio.h>
#ifdef unix
#define ADDRMF addrmf_
#endif
#ifdef vms
#define ADDRMF addrmf
#endif
#ifdef unix
#define RETURN 0
#endif
#ifdef vms
#define RETURN 1
#endif

#define DEFAULT_PFILE "addrmf.par"

int MAIN_; /* work around SunOS 4.1.3 bug */

main (argc,argv)
int argc;
char **argv;
{
  OpenDefaultPF(argc, argv, DEFAULT_PFILE);
  ADDRMF();
  CloseDefaultPF();
  return(RETURN);
}


