/*
 filename: hfimgunzip.c
 purpose:  c - wrapper for host fimgunzip task
 author:   Ning Gan  2/11/00
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
 void fimgunzip();

 OpenDefaultPF(argc, argv);
 fimgunzip();
 CloseDefaultPF();
 return(RETURN);
}

