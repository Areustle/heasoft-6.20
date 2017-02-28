**==uclpsi.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994

* $Id: uclpsi.f,v 3.7 2011/07/13 20:51:13 irby Exp $
* $Log: uclpsi.f,v $
* Revision 3.7  2011/07/13 20:51:13  irby
* Add the appropriate APE_TRAD_SET_[] call to tell APE to set the parameter
* when a call to uclps[b/d/i/l/r/s/t] is made.
*
* Revision 3.6  1999/02/11 18:33:11  guerber
* Changed the (I10.10) format, which can't handle negative numbers, to * as
* in the other uclps* routines.
*
* Revision 3.5.1.1  1996/04/16 01:39:26  dunfee
* Start of pristine ftools CVS...
*
c Revision 1.3  1995/04/13  16:54:18  oneel
c Made sure that the status return returned the error from the internal
c write
c
c Revision 1.2  1995/04/13  16:49:46  oneel
c Changed the write statement so that there are no leading spaces in the
c default value
c

* put a integer parameter to a .par file
      SUBROUTINE UCLPSI(Parname,Buffer,Status)
 
 
* parname : parameter name
* buffer  : value to put
* status  : 0 ok, 5 error
 
      CHARACTER*(*) Parname
      INTEGER*4 Buffer
      INTEGER*4 Status
 
 
      INTEGER*4 TBLFPR
      INTEGER*4 ierr
 
      INCLUDE 'tbl.inc'
 
      INTEGER*4 i

      INTEGER APE_TRAD_SET_INT
 
      IF ( Status.NE.0 ) RETURN
 
      Status = APE_TRAD_SET_INT(Parname, Buffer)
 
      i = TBLFPR(Parname)
 
      IF ( i.EQ.0 ) THEN
         Status = 5
         RETURN
      ENDIF
 
      IF ( i.GT.TBLpcnt ) THEN
         Status = 5
         RETURN
      ENDIF
 
 
      IF ( TBLptype(i).NE.'i' ) THEN
         Status = 5
         RETURN
      ENDIF
 
      WRITE (TBLpdefl(i),*,IOSTAT=ierr) Buffer
 
*      CALL TBSVPR(Tblpfname,ierr)
      Status = ierr
      RETURN
      END
 
