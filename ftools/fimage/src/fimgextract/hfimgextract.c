/*
 filename: hfimgextract.c
 purpose:  c - wrapper for host fimgextract task
 author:  Peter Wilson (from hcdummyftool.c)
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
 void fimgextract();

 OpenDefaultPF(argc, argv);
 fimgextract();
 CloseDefaultPF();
 return(RETURN);
}
