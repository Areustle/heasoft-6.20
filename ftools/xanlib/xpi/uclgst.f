**==uclgst.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
      SUBROUTINE UCLGST(Parname,Buffer,Status)
 
*
* return a string value related to parname
*
      CHARACTER*(*) Parname
      CHARACTER*(*) Buffer
      INTEGER Status
      character(1000) buffer1
      character(80) line
      INTEGER LENACT , j
 
      INCLUDE 'yaccfor.inc'
 
      IF ( DEBug ) THEN
         line = 'uclgst called for ' // Parname(1:LENACT(Parname))
         CALL XWRITE(line,5)
      ENDIF
      CALL XPIQUERYPAR(Parname,'s',buffer1,.FALSE.,.TRUE.,.FALSE.,
     &                 Status)
 
 
      IF ( DEBug ) THEN
         WRITE (line,*,IOSTAT=j) 'status of ' , Status , 
     &                           ' value is ' //
     &          buffer1(:MIN(len(line)-32,LENACT(buffer1)))
         CALL XWRITE(line,5)
      ENDIF
 
 100  IF ( Status.NE.0 ) RETURN
      Buffer = buffer1
      IF ( DEBug ) THEN
         line = 'final return of ' // Buffer(1:LENACT(Buffer))
         CALL XWRITE(line,5)
      ENDIF
      RETURN
 
      ENTRY UCLGSTD(Parname,Buffer,Status)
 
      CALL XPIDEFAULTPAR(Parname,'s',buffer1,Status)
 
      GOTO 100
c$$$      CHARACTER*(*) Parname
c$$$      CHARACTER*(*) Buffer
c$$$      INTEGER*4 Status
c$$$
c$$$      INTEGER*4 TBLFPR
c$$$      character(80) str1 , str2
c$$$      INTEGER*4 ierr
c$$$
c$$$      INCLUDE 'tbl.inc'
c$$$      INCLUDE 'yaccfor.inc'
c$$$
c$$$      INTEGER*4 i
c$$$      INTEGER*4 j
c$$$      INTEGER*4 inlen
c$$$      INTEGER*4 LENACT
c$$$
c$$$      IF ( Status.NE.0 ) RETURN
c$$$
c$$$      Buffer = ' '
c$$$
c$$$      i = TBLFPR(Parname)
c$$$*
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
c$$$      IF ( TBLptype(i).NE.'s' ) THEN
c$$$         Status = 2
c$$$         RETURN
c$$$      ENDIF
c$$$
c$$$      str1 = Parname
c$$$      CALL UPC(str1)
c$$$
c$$$      DO 100 j = 1 , NPArs
c$$$         str2 = SPArs(j)
c$$$         CALL UPC(str2)
c$$$
c$$$         IF ( str1.EQ.str2 ) THEN
c$$$            Buffer = SVAl(j)
c$$$            IF ( INDEX(TBLpupd(i),'l').NE.0 ) THEN
c$$$               TBLpdefl(i) = Buffer
c$$$               CALL YSTCLQ1(TBLpdefl(i))
c$$$*               CALL TBSVPR(Tblpfname,ierr)
c$$$            ENDIF
c$$$            IF ( TBLpstat(i) ) THEN
c$$$               TBLpstat(i) = .FALSE.
c$$$               RETURN
c$$$            ENDIF
c$$$         ENDIF
c$$$ 100  CONTINUE
c$$$
c$$$      IF ( Buffer.EQ.' ' .AND. INDEX(TBLpupd(i),'q').EQ.0 )
c$$$     &     Buffer = TBLpdefl(i)
c$$$
c$$$      IF ( ((Buffer.EQ.' ' .OR. (.NOT.TBLpstat(i) .and. .not.
c$$$     $     tblstandalone))
c$$$     $     .AND.
c$$$     &     INDEX(TBLpupd(i),'h').EQ.0) ) THEN
c$$$         CALL XCREAD(TBLpdesc(i)(1:LENACT(TBLpdesc(i)))
c$$$     &               //'['//TBLpdefl(i)(1:LENACT(TBLpdefl(i)))//']',
c$$$     &               Buffer,ierr)
c$$$         CALL GTBUFINLEN(inlen)
c$$$         IF ( inlen.EQ.0 .AND. Buffer.EQ.' ' ) Buffer = TBLpdefl(i)
c$$$         IF ( Buffer(1:1).EQ.'"' ) CALL YSTCLQ1(Buffer)
c$$$         IF ( INDEX(TBLpupd(i),'l').NE.0 ) THEN
c$$$            TBLpdefl(i) = Buffer
c$$$            CALL YSTCLQ1(TBLpdefl(i))
c$$$*            CALL TBSVPR(Tblpfname,ierr)
c$$$         ENDIF
c$$$      ENDIF
c$$$      TBLpstat(i) = .FALSE.
c$$$      RETURN
c$$$
c$$$
c$$$* Get the default value
c$$$
c$$$      ENTRY UCLGSTD(Parname,Buffer,Status)
c$$$
c$$$      IF ( Status.NE.0 ) RETURN
c$$$
c$$$
c$$$      Buffer = ' '
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
c$$$      IF ( TBLptype(i).NE.'s' ) THEN
c$$$         Status = 2
c$$$         RETURN
c$$$      ENDIF
c$$$
c$$$      Buffer = TBLpdefl(i)
c$$$
c$$$      RETURN
 
      END
 
 
