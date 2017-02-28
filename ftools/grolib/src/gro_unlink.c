#include <errno.h>
/*      $Id: gro_unlink.c,v 3.1 2003/08/19 20:42:13 irby Exp $
 *      $Log: gro_unlink.c,v $
 *      Revision 3.1  2003/08/19 20:42:13  irby
 *      Renamed unlink.c to gro_unlink.c to avoid confusion with the system
 *      unlink routine.
 *
 *      Revision 3.1  2002/04/16 20:32:14  irby
 *      Additions to libgro - previously these codes existed in the following
 *      libraries:
 *
 *        libsenstv
 *        libsysutil
 *        libutil
 *        libftio
 *
 * Revision 1.1  1996/02/20  20:52:01  programs
 * Initial revision
 *
 */

	void gro_unlink_ ( path, status )
	char* path;
	int* status;
{	
	int n;
	
	errno = 0;
	n = unlink ( path );	
	*status = errno;

	return;
}	
