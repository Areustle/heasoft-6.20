**==uclpsx.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
* put a complex parameter to a .par file
      SUBROUTINE UCLPSX(Parname,Buffer,Status)
 
 
* parname : parameter name
* buffer  : value to put
* status  : 0 ok, 5 error
 
      CHARACTER*(*) Parname
      COMPLEX Buffer
      INTEGER*4 Status
 
 
      INTEGER*4 TBLFPR
      INTEGER*4 ierr
 
      INCLUDE 'tbl.inc'
 
      INTEGER*4 i
 
      IF ( Status.NE.0 ) RETURN
 
 
      i = TBLFPR(Parname)
 
      IF ( i.EQ.0 ) THEN
         Status = 5
         RETURN
      ENDIF
 
      IF ( i.GT.TBLpcnt ) THEN
         Status = 5
         RETURN
      ENDIF
 
 
      IF ( TBLptype(i).NE.'x' ) THEN
         Status = 5
         RETURN
      ENDIF
 
      WRITE (TBLpdefl(i),*,IOSTAT=ierr) Buffer
 
*      CALL TBSVPR(Tblpfname,ierr)
      Status = 0
      RETURN
      END
 
