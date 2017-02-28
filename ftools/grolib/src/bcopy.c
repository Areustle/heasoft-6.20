#include <sys/types.h>
/*      $Id: bcopy.c,v 3.1 2002/04/16 20:32:07 irby Exp $
 *      $Log: bcopy.c,v $
 *      Revision 3.1  2002/04/16 20:32:07  irby
 *      Additions to libgro - previously these codes existed in the following
 *      libraries:
 *
 *        libsenstv
 *        libsysutil
 *        libutil
 *        libftio
 *
 * Revision 1.1  1996/02/20  20:47:04  programs
 * Initial revision
 *
 */


	void bcopy_ ( src, dst, len )
	caddr_t *src, *dst;
	int *len;
{
	bcopy ( src, dst, *len );
	return;
}
