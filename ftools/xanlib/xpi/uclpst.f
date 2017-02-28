**==uclpst.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
* put a text parameter to a .par file
      SUBROUTINE UCLPST(Parname,Buffer,Status)
 
 
* parname : parameter name
* buffer  : value to put
* status  : 0 ok, 5 error
 
      CHARACTER*(*) Parname
      CHARACTER*(*) Buffer
      INTEGER*4 Status
 
 
      INTEGER*4 TBLFPR
 
      INCLUDE 'tbl.inc'
 
      INTEGER*4 i

      INTEGER APE_TRAD_SET_STRING

c     The modification below was necessary to allow for the case where
c uclpst is called to set a non-string value to INDEF, in that case the
c call to uclgs(n) will have returned status==3, so we are allowing a
c status of 3 to not return, but to instead punch a string to non-string
c values.
c     IF ( Status.NE.0 ) RETURN
      IF ( Status.NE.0.and.status.ne.3 ) RETURN

      Status = APE_TRAD_SET_STRING(Parname, Buffer)
 
      i = TBLFPR(Parname)
 
      IF ( i.EQ.0 ) THEN
         Status = 5
         RETURN
      ENDIF
 
      IF ( i.GT.TBLpcnt ) THEN
         Status = 5
         RETURN
      ENDIF
 
c     This modification was made on 4-15-96 to allow for the use of
c INDEF and to allow codes that use that as a valid input for integer,
c real, and double precision parameters to be updated properly. Thus
c if the error status returned by uclgs(n) is 3 (i.e., it read an
c INDEF value) then if uclpst is called you can insert INDEF back into
c that value. But the programmer must specifically call uclpst with
c status==3 in order to be able to do this. BKE
      if(status.ne.3)then
        IF ( TBLptype(i).NE.'s' ) THEN
          Status = 5
          RETURN
        ENDIF
      endif

c     This modification was made on 4-25-96 and is a minor modification
c to the above. In the case that the user calls uclpst with status==3
c and Buffer != INDEF we return an error massage, since this is not
c correct. If the above criteria is met, i.e., status==3 and
c Buffer == INDEF then we set status = 0 so that on the "check" we
c can see that all was well. BKE
      if(status.eq.3.and.Buffer.ne.'INDEF')then
        status=5
        return
      else if(status.eq.3.and.Buffer.eq.'INDEF')then
        status=0
      endif
      
      TBLpdefl(i) = Buffer
      CALL YSTCLN(TBLpdefl(i))
*      CALL TBSVPR(Tblpfname,ierr)
      Status = 0
      RETURN
      END
 
