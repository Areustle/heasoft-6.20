 /*
  filename: hascaarf.c
  purpose: c - wrapper for host ASCAARF task
  author:  Keith Arnaud
  based on Ron Zellar and Yan Fernandez' code
 */

#include <stdio.h>
#ifdef unix
#define ASCAEFFMAP ascaep_
#endif
#ifdef vms
#define ASCAEFFMAP ascaep
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
  ASCAEFFMAP();
  CloseDefaultPF();
  return(RETURN);
}
