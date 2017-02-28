c
c	FTIO system include to support UNIT OPEN STATUS
c
c       $Id: ftiosys.h,v 3.1 2002/04/16 20:32:11 irby Exp $
c       $Log: ftiosys.h,v $
c       Revision 3.1  2002/04/16 20:32:11  irby
c       Additions to libgro - previously these codes existed in the following
c       libraries:
c
c         libsenstv
c         libsysutil
c         libutil
c         libftio
c
c Revision 1.1  1996/03/27  18:51:42  wang
c Initial revision
c
c	This include depends on include ftio.h 

	logical isopen(ftio_max_unit)	

	common /ftio_parms/ isopen
