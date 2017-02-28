#include <stdio.h>
 
#ifdef unix
#define PCABACKEST PCABackEst
#endif
#ifdef vms
#define PCABACKEST PCABackEst
#endif
#ifdef unix
#define RETURN 0
#endif
#ifdef vms
#define RETURN 1
#endif

int MAIN_; /* work around SunOS 4.1.3 bug */

main(argc, argv)
int argc;
char **argv;

/*********************\
*        main         *
*        ----         *
\*********************/

{
  void PCABACKEST();

  OpenDefaultPF(argc, argv);
  PCABACKEST();
  CloseDefaultPF();
  return(RETURN);
}
