/*
filename : hecd2pha.c
purpose  : c wrapper for host ecd2pha task
author   : Rehana Yusaf
*/
#include <stdio.h>
#ifdef unix
#define ECD2PHA ecd2pa_
#endif
#ifdef vms
#define ECD2PHA ecd2pa
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
ECD2PHA();
CloseDefaultPF();
return(RETURN);
}
