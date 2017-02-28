/*
 filename: hfexpoadapt.c
 purpose:  c - wrapper for host fexpoadapt task
 author:  James Peachey 
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
 void fexpoadapt();

 OpenDefaultPF(argc, argv);
 fexpoadapt();
 CloseDefaultPF();
 return(RETURN);
}
