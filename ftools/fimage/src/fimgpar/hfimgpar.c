/*
 filename: hfimgpar.c
 purpose:  c - wrapper for host fimgpar task
 author:  Alex Muslimov
*/

#include <stdio.h>
#ifdef unix
#define RETURN 0
#endif
#ifdef vms
#define RETURN 1
#endif

int MAIN_; /* work around SunOS 4.1.3 bug */

main (int argc, char **argv)
{
 void fimgpar();

 OpenDefaultPF(argc, argv);
 fimgpar();
 CloseDefaultPF();
 return(RETURN);
}
