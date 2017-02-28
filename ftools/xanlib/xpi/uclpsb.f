**==uclpsb.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
* put a boolean parameter to a .par file
      SUBROUTINE UCLPSB(Parname,Buffer,Status)
 
 
* parname : parameter name
* buffer  : value to put
* status  : 0 ok, 5 error
 
      CHARACTER*(*) Parname
      LOGICAL*4 Buffer
      INTEGER*4 Status
 
 
      INTEGER*4 TBLFPR
 
      INCLUDE 'tbl.inc'
 
      INTEGER*4 i
 
      INTEGER APE_TRAD_SET_BOOL
 
      IF ( Status.NE.0 ) RETURN
 
      Status = APE_TRAD_SET_BOOL(Parname, Buffer)
 
 
      i = TBLFPR(Parname)
 
      IF ( i.EQ.0 ) THEN
         Status = 5
         RETURN
      ENDIF
 
      IF ( i.GT.TBLpcnt ) THEN
         Status = 5
         RETURN
      ENDIF
 
 
      IF ( TBLptype(i).NE.'b' ) THEN
         Status = 5
         RETURN
      ENDIF
 
      IF ( Buffer ) THEN
         TBLpdefl(i) = 'yes'
      ELSE
         TBLpdefl(i) = 'no'
      ENDIF
 
C      CALL TBSVPR(Tblpfname,ierr)
      Status = 0
      RETURN
      END
 
