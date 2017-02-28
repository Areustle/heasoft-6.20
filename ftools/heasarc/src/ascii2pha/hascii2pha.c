/*
filename : hascii2pha.c
purpose  : c wrapper for host ascii2pha task
author   : Rehana Yusaf
*/
#include <stdio.h>
#ifdef unix
#define ASCII2PHA asciia_
#endif
#ifdef vms
#define ASCII2PHA asciia
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
OpenDefaultPF(argc,argv);
ASCII2PHA();
CloseDefaultPF();
return(RETURN);
}
