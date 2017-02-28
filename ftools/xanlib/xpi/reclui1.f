**==reclui1.spg  processed by SPAG 4.50F  at 15:17 on 26 Aug 1994
      SUBROUTINE RECLUI1(N_max,Number,Flag,Status)
c
c read and write and list the past n_max commands
c
c
      INTEGER*4 NQUAL , Status , N_max , Number , Flag
      PARAMETER (NQUAL=1)
      INTEGER*4 lq(NQUAL) , nq , jj
      character(30) quals(NQUAL)
      character(80) qualifier(NQUAL)
      INTEGER*4 ierr
      DATA quals/'MAX'/
      N_max = 20
      Number = -99
      CALL DOQUAL(quals,qualifier,lq,NQUAL,nq,Status)
      IF ( Status.NE.0 ) RETURN
c
c parse the string
c
      CALL UCLGSI('recallwhat',Number,ierr)
      IF ( ierr.NE.0 ) RETURN
C      CALL XGTARG(Zstring,Zparse,Zbeg,Zend,qskip,iflag,idelim,*100,*100,
C     &            *100)
C      IF ( Zend.NE.0 ) THEN
C         char = Zstring(Zbeg:Zend)
C         CALL STRNUM(char,8,real8,ierr)
CC         IF ( ierr.NE.0 ) RETURN
C         Number = real8
C      ENDIF
c
c do the quals
c
      IF ( nq.GT.0 ) THEN
         DO 50 jj = 1 , nq
            Status = 0
            CALL QLF0I(qualifier(jj),'MAX',N_max,Status)
            Status = 0
 50      CONTINUE
      ENDIF
c
      IF ( Number.EQ.-99 ) THEN
         Flag = 1
      ELSE
         Flag = 3
      ENDIF
      RETURN
      END
