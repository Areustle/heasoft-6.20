**==uclpsd.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
* put a double parameter to a .par file
      SUBROUTINE UCLPSD(Parname,Buffer,Status)
 
 
* parname : parameter name
* buffer  : value to put
* status  : 0 ok, 5 error
 
      CHARACTER*(*) Parname
      REAL*8 Buffer
      INTEGER*4 Status
 
 
      INTEGER*4 TBLFPR
      INTEGER*4 ierr
 
      INCLUDE 'tbl.inc'
 
      INTEGER*4 i

      INTEGER APE_TRAD_SET_DOUBLE
 
      IF ( Status.NE.0 ) RETURN
 
      Status = APE_TRAD_SET_DOUBLE(Parname, Buffer)
 
      i = TBLFPR(Parname)
 
      IF ( i.EQ.0 ) THEN
         Status = 5
         RETURN
      ENDIF
 
      IF ( i.GT.TBLpcnt ) THEN
         Status = 5
         RETURN
      ENDIF
 
 
      IF ( TBLptype(i).NE.'d' .AND. TBLptype(i).NE.'r' ) THEN
         Status = 5
         RETURN
      ENDIF
 
      WRITE (TBLpdefl(i),*,IOSTAT=ierr) Buffer
 
*      CALL TBSVPR(Tblpfname,ierr)
      Status = 0
      RETURN
      END
 
