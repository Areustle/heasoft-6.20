/* $Id: aste_att.h,v 1.13 2007/05/07 16:45:31 ishisaki Exp $ */
/************************************************************************
	aste_att_init()		open attitude file and return ATTFILE structure
	aste_att_close()	close attitude file
	aste_att_ea()		get Eular angles in given time
	aste_att_attfile()	return currently opened ATTFILE structure [obsolete]
	aste_att_euler		get Eular angles in given time [obsolete]

  2006-08-01	Y.ISHISAKI	version 1.80
	add aste_att_ea(), now aste_att_attfile(), aste_att_euler() is obsolete.
************************************************************************/

#ifndef _ASTE_ATT_H_
#define _ASTE_ATT_H_

#include "attfile.h"	/* ATTFILE definition */

#ifdef __cplusplus
extern "C"
{
#endif

ATTFILE *aste_att_init(char *filename);
void aste_att_close(ATTFILE *ap);
int aste_att_ea(ATTFILE *ap, double t, AtEulerAng *ea);
ATTFILE *aste_att_attfile(void);				/* OBSOLETE */
int aste_att_euler(double t, AtEulerAng *ea);	/* OBSOLETE */

#ifdef __cplusplus
}
#endif

#endif	/* _ASTE_ATT_H_ */

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
