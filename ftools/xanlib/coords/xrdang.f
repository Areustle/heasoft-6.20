**==XRDANG.spg  processed by SPAG 3.09I  at 13:46 on 15 Jan 1992
c
      SUBROUTINE XRDANG(String,Id,Im,Rs)
c
c          ! read dd-mm-ss.s from string
c
C     id contains the output value - degrees or hrs
C     im contains the output value - minutes
C     rs contains the output value - seconds (r*4)
C fixed -ve 0 hrs problem 30.7.90 nick
c removed alternate return 14-10-92 nick
      CHARACTER*(*) String
      REAL*4 Rs
      INTEGER*4 Im
      INTEGER*4 Id , nret , idelim , iparse , iflag
c
      Id = 0
      Im = 0
      Rs = 0.0
      nret = 0
      idelim = 0
      iparse = 0
      IF ( String.EQ.' ' ) RETURN
c
      CALL RMVLBK(String)
c
      CALL XGTINT(String,iparse,1,' ',1,Id,nret,iflag,idelim,*100,*100,
     &            *100)
 100  IF ( nret.EQ.0 ) RETURN
      nret = 0
      idelim = 0
      CALL XGTINT(String,iparse,1,' ',1,Im,nret,iflag,idelim,*200,*200,
     &            *200)
 200  IF ( nret.EQ.0 ) THEN
         Im = 0
         Rs = 0.0
      ELSE
         idelim = 0
         nret = 0
         CALL XGTRL4(String,iparse,1,' ',1,Rs,nret,iflag,idelim,*250,
     &               *250,*250)
 250     IF ( nret.EQ.0 ) Rs = 0.
      ENDIF
c
      IF ( String(1:1).EQ.'-' .AND. Id.EQ.0 ) THEN
         IF ( Im.GT.0 ) THEN
            Im = -Im
         ELSEIF ( Rs.GT.0 ) THEN
            Rs = -Rs
         ENDIF
      ENDIF
c
c check for -ve values in the wrong place
c
      IF ( Id.NE.0 .AND. Im.LT.0 ) Im = -Im
      IF ( Id.NE.0 .AND. Im.NE.0 .AND. Rs.LT.0.0 ) Rs = -Rs
c
      RETURN
      END
