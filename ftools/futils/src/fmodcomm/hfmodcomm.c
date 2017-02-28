/*
 filename: hfmodcomm.c
 purpose:  c - wrapper for host FMODCOMM task
 author:   Mike Tripicco 
*/

#include <stdio.h>

#ifdef unix
#define FMODCOMM fmodcm_
#endif
#ifdef vms
#define FMODCOMM fmodcm
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
 FMODCOMM();
 CloseDefaultPF();
 return(RETURN);
}
