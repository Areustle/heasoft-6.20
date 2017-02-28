/*
 filename: hfcollect.c
 purpose:  c - wrapper for host FCOLLECT task
 author:   Brian K. Elza
*/

#include <stdio.h>

#ifdef unix
#define FCOLLECT Fcollect
#endif
#ifdef vms
#define FCOLLECT Fcollect
#endif
#ifdef unix
#define RETURN 0
#endif
#ifdef vms
#define RETURN 1
#endif

int MAIN_; /* work around SunOS 4.1.3 bug */

int main (argc,argv)
int argc;
char **argv;
{
  void FCOLLECT();
  void OpenDefaultPF(int, char **);
  void CloseDefaultPF(void);

  OpenDefaultPF(argc, argv);
  FCOLLECT();
  CloseDefaultPF();
  return(RETURN);
}
