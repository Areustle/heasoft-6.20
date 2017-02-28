/*
 filename: hkscale.c
 purpose:  c - wrapper for host hkscale task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define HKSCALE hkscae_
#endif
#ifdef vms
#define HKSCALE hkscae
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
 HKSCALE();
 CloseDefaultPF();
 return(RETURN);
}
