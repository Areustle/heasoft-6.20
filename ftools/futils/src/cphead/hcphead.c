/*
 filename: hcphead.c
 purpose:  c - wrapper for host cphead task
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
 void cphead();

 OpenDefaultPF(argc, argv);
 cphead();
 CloseDefaultPF();
 return(RETURN);
}
