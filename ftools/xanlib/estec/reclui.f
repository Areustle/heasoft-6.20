**==RECLUI.spg  processed by SPAG 3.09I  at 09:47 on 20 Aug 1992
      SUBROUTINE RECLUI(N_max,Number,Flag,Status)
c
c read and write and list the past n_max commands
c
      INCLUDE 'estec.inc'
c
      INTEGER*4 NQUAL , Status , N_max , Number , Flag
      REAL*8 real8
      PARAMETER (NQUAL=1)
      INTEGER*4 lq(NQUAL) , nq , jj
      character(30) quals(NQUAL)
      character(80) qualifier(NQUAL) , char
      INTEGER*4 iflag , idelim , ierr
      LOGICAL*4 qskip
      DATA quals/'MAX'/
      N_max = 20
      Number = -99
      CALL DOQUAL(quals,qualifier,lq,NQUAL,nq,Status)
      IF ( Status.NE.0 ) RETURN
c
c parse the string
c
      CALL XGTARG(Zstring,Zparse,Zbeg,Zend,qskip,iflag,idelim,*100,*100,
     &            *100)
      IF ( Zend.NE.0 ) THEN
         char = Zstring(Zbeg:Zend)
         CALL STRNUM(char,8,real8,ierr)
         IF ( ierr.NE.0 ) RETURN
         Number = real8
      ENDIF
c
c do the quals
c
 100  IF ( nq.GT.0 ) THEN
         DO 150 jj = 1 , nq
            Status = 0
            CALL QLF0I(qualifier(jj),'MAX',N_max,Status)
            Status = 0
 150     CONTINUE
      ENDIF
c
      IF ( Number.EQ.-99 ) THEN
         Flag = 1
      ELSE
         Flag = 3
      ENDIF
      RETURN
      END
