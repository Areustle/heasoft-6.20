/*
 filename: hfimgdmp.c
 purpose:  c - wrapper for host FIMGDMP task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FIMGDMP fimgdp_
#endif
#ifdef vms
#define FIMGDMP fimgdp 
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
 FIMGDMP();
 CloseDefaultPF();
 return(RETURN);
}
