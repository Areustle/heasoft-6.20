/*
 filename:	hsisrmg.c
 purpose:	c - wrapper for host FSISRM task
 author:	Geoffrey B. Crew
 date:		February 1993
 updated:	February 1996
*/

#ifdef unix
# ifndef SISDEBUG
#  define SISRMG sisrmg_
# endif /* SISDEBUG */
#endif
#ifdef vms
# define SISRMG sisrmg
#endif

int MAIN_; /* work around SunOS 4.1.3 bug */

main (argc, argv)
int argc;
char **argv;
{
	int	status;

	OpenDefaultPF(argc, argv);
	status = SISRMG();
	CloseDefaultPF();
#ifdef unix
	return(status);
#else
	return(1);
#endif
}

/*
 *  End of hsisrmg.c
 */
