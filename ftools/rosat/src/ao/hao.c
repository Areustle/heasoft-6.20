 /*
  filename: hao.c
  purpose: c - wrapper for host AO task
 */

#include <stdio.h>
#ifdef unix
#define AO ao_
#endif
#ifdef vms
#define AO ao
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
  AO();
  CloseDefaultPF();
  return(RETURN);
}


