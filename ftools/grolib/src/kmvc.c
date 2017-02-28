/* kmvc.c
 * $Id: kmvc.c,v 3.1 2002/04/16 20:32:12 irby Exp $
 *
 * Sun replacement for the routine KMVC available on IBM.
 * ---------------------------------------------------------------------------
 * Nand Lal - GSFC - January 91
 * E.S.Panduranga - S.T.X. - 04/18/91
 * ---------------------------------------------------------------------------
 * $Log: kmvc.c,v $
 * Revision 3.1  2002/04/16 20:32:12  irby
 * Additions to libgro - previously these codes existed in the following
 * libraries:
 *
 *   libsenstv
 *   libsysutil
 *   libutil
 *   libftio
 *
 * Revision 1.1  1991/10/08  21:31:57  esp
 * Initial revision
 *
 */

static char *rcsid = "$Id: kmvc.c,v 3.1 2002/04/16 20:32:12 irby Exp $";

void kmvc_(d,doff,s,soff,len)
char *d;
int *doff;
char *s;
int *soff;
int *len;
{
	/* bcopy does not work properly if there are overlapping fields	*/
	/* in spite of what the SUN manuals say. Hence we need this fix	*/
	/* E.S.Panduranga. 07/03/91					*/
	if( abs((s+(*soff)) - (d+(*doff))) < *len)
	{
	  int	i = *len;

	  /* Copy *len bytes from the source to the destination */
	  s += (*soff) - 1;
	  d += (*doff) - 1;
	  while(--i > 0)
	    *d++ = *s++;
	}
	else
	  bcopy(s+(*soff)-1,d+(*doff)-1,*len);
	return;
}

int kclc_(d,doff,s,soff,len)
unsigned char *d;
int *doff;
unsigned char *s;
int *soff;
int *len;
{
	int ret_code=0;
	int i;
	for (i= -1; i< (*len)-1; i++)
	{
		if ( *(s+(*soff)+i) == *(d+(*doff)+i) ) continue;
		if ( *(s+(*soff)+i) < *(d+(*doff)+i) )
			ret_code = 2;
		else
			ret_code = 1;
		;
		break;
	}
	return(ret_code);
}
