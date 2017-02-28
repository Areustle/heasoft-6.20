**==uclgsb.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
      SUBROUTINE UCLGSB(Parname,Buffer,Status)
 
*
* return a boolean value related to parname
*
 
      CHARACTER*(*) Parname
      LOGICAL Buffer
      INTEGER Status
      character(80) buffer1 , line
      INTEGER LENACT , j
 
      INCLUDE 'yaccfor.inc'
 
      IF ( DEBug ) THEN
         line = 'uclgsb called for ' // Parname(1:LENACT(Parname))
         CALL XWRITE(line,5)
      ENDIF
      CALL XPIQUERYPAR(Parname,'b',buffer1,.FALSE.,.FALSE.,.TRUE.,
     &                 Status)
 
      IF ( DEBug ) THEN
         WRITE (line,*,IOSTAT=j) 'status of ' , Status , 
     &                           ' value is ' //
     &          buffer1(:MIN(len(line)-32,LENACT(buffer1)))
         CALL XWRITE(line,5)
      ENDIF
 
 100  IF ( Status.NE.0 ) RETURN
 
      CALL UPC(buffer1)
      Buffer = .FALSE.
      IF ( buffer1(1:1).EQ.'Y' ) Buffer = .TRUE.
      IF ( DEBug ) THEN
         WRITE (line,*,IOSTAT=j) 'final return of ' , Buffer
         CALL XWRITE(line,5)
      ENDIF
      RETURN
 
      ENTRY UCLGSBD(Parname,Buffer,Status)
 
      CALL XPIDEFAULTPAR(Parname,'b',buffer1,Status)
 
      GOTO 100
 
c$$$      IMPLICIT NONE
c$$$      CHARACTER*(*) Parname
c$$$      LOGICAL*4 Buffer
c$$$      INTEGER*4 Status
c$$$
c$$$      INTEGER*4 TBLFPR
c$$$      INTEGER*4 ierr
c$$$      character(80) str1 , str2 , str3 , buffer1
c$$$
c$$$      INCLUDE 'tbl.inc'
c$$$      INCLUDE 'yaccfor.inc'
c$$$
c$$$      INTEGER*4 i
c$$$      INTEGER*4 j
c$$$      INTEGER*4 k
c$$$      LOGICAL*4 on
c$$$      LOGICAL*4 off
c$$$      INTEGER*4 LENACT
c$$$      LOGICAL*4 found
c$$$
c$$$      IF ( Status.NE.0 ) RETURN
c$$$
c$$$
c$$$      buffer1 = ' '
c$$$      i = TBLFPR(Parname)
c$$$
c$$$      IF ( i.EQ.0 ) THEN
c$$$         Status = 1
c$$$         RETURN
c$$$      ENDIF
c$$$
c$$$      IF ( i.GT.TBLpcnt ) THEN
c$$$         Status = 1
c$$$         RETURN
c$$$      ENDIF
c$$$
c$$$
c$$$      IF ( TBLptype(i).NE.'b' ) THEN
c$$$         Status = 2
c$$$         RETURN
c$$$      ENDIF
c$$$
c$$$      str1 = Parname
c$$$      CALL UPC(str1)
c$$$
c$$$      found = .FALSE.
c$$$      Buffer = .FALSE.
c$$$      DO 100 j = 1 , NPArs
c$$$         str2 = SPArs(j)
c$$$         CALL UPC(str2)
c$$$         str3 = SVAl(j)
c$$$         CALL UPC(str3)
c$$$
c$$$
c$$$         k = LENACT(str2)
c$$$         on = .TRUE.
c$$$         off = .FALSE.
c$$$         IF ( str2(k:k).EQ.'+' ) THEN
c$$$            on = .TRUE.
c$$$            str2(k:k) = ' '
c$$$         ENDIF
c$$$         IF ( str2(k:k).EQ.'-' ) THEN
c$$$            off = .TRUE.
c$$$            str2(k:k) = ' '
c$$$         ENDIF
c$$$         IF ( str3.EQ.'ON' ) on = .TRUE.
c$$$         IF ( str3.EQ.'OFF' ) off = .TRUE.
c$$$
c$$$         IF ( str3(1:1).EQ.'Y' ) on = .TRUE.
c$$$         IF ( str3(1:1).EQ.'N' ) off = .TRUE.
c$$$
c$$$
c$$$         IF ( str1.EQ.str2 ) THEN
c$$$            IF ( .NOT.on .AND. .NOT.off ) Buffer = .TRUE.
c$$$            IF ( on ) THEN
c$$$               buffer1 = 'yes'
c$$$               Buffer = .TRUE.
c$$$            ENDIF
c$$$            IF ( off ) THEN
c$$$               buffer1 = 'no'
c$$$               Buffer = .FALSE.
c$$$            ENDIF
c$$$            found = .TRUE.
c$$$            IF ( INDEX(TBLpupd(i),'l').NE.0 ) THEN
c$$$               TBLpdefl(i) = 'no'
c$$$               IF ( Buffer ) TBLpdefl(i) = 'yes'
c$$$*               CALL TBSVPR(Tblpfname,ierr)
c$$$            ENDIF
c$$$         ENDIF
c$$$ 100  CONTINUE
c$$$
c$$$
c$$$
c$$$      IF ( .NOT.found .AND. INDEX(TBLpupd(i),'q').EQ.0 ) THEN
c$$$         buffer1 = TBLpdefl(i)
c$$$         CALL LOCASE(buffer1)
c$$$         found = .TRUE.
c$$$      ENDIF
c$$$      IF ( ((.NOT.found .OR. (.NOT.TBLpstat(i) .and. .not.
c$$$     $     tblstandalone))
c$$$     $     .AND.
c$$$     &     INDEX(TBLpupd(i),'h').EQ.0) ) THEN
c$$$
c$$$         DO WHILE ( .TRUE. )
c$$$            CALL XCREAD(TBLpdesc(i)(1:LENACT(TBLpdesc(i)))
c$$$     &                  //'['//TBLpdefl(i)(1:LENACT(TBLpdefl(i)))//']',
c$$$     &                  buffer1,ierr)
c$$$            IF ( buffer1.EQ.' ' ) buffer1 = TBLpdefl(i)
c$$$            CALL LOCASE(buffer1)
c$$$            IF ( buffer1(1:1).NE.'y' .AND. buffer1(1:1).NE.'n' ) THEN
c$$$               CALL XWRITE(' Please enter a Y or N',5)
c$$$            ELSE
c$$$               TBLpdefl(i) = 'no'
c$$$               IF ( buffer1(1:1).EQ.'y' ) TBLpdefl(i) = 'yes'
c$$$               GOTO 200
c$$$            ENDIF
c$$$         ENDDO
c$$$      ENDIF
c$$$ 200  Buffer = buffer1(1:1).EQ.'y' .OR. buffer1(1:1).EQ.'on'
c$$$      TBLpstat(i) = .FALSE.
c$$$
c$$$      RETURN
c$$$
c$$$* Get the default value
c$$$
c$$$      ENTRY UCLGSBD(Parname,Buffer,Status)
c$$$      IF ( Status.NE.0 ) RETURN
c$$$
c$$$
c$$$      buffer1 = ' '
c$$$      i = TBLFPR(Parname)
c$$$
c$$$      IF ( i.EQ.0 ) THEN
c$$$         Status = 1
c$$$         RETURN
c$$$      ENDIF
c$$$
c$$$      IF ( i.GT.TBLpcnt ) THEN
c$$$         Status = 1
c$$$         RETURN
c$$$      ENDIF
c$$$
c$$$
c$$$      IF ( TBLptype(i).NE.'b' ) THEN
c$$$         Status = 2
c$$$         RETURN
c$$$      ENDIF
c$$$
c$$$      Buffer = .FALSE.
c$$$      Buffer = TBLpdefl(i)(1:1).EQ.'y' .OR. TBLpdefl(i)(1:1).EQ.'on'
c$$$      RETURN
c$$$
c$$$      END
      END
