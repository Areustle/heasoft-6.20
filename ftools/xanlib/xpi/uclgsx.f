**==uclgsx.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
      SUBROUTINE UCLGSX(Parname,Buffer,Status)
 
*
* return a complex*8 value related to parname
*
      CHARACTER*(*) Parname
      COMPLEX*8 Buffer
      INTEGER Status
      character(80) line, buffer1
      INTEGER LENACT
 
      INCLUDE 'yaccfor.inc'
 
      IF ( DEBug ) then 
         line = 'uclgss called for ' // Parname(1:LENACT(Parname))
         CALL XWRITE(line,5)
      endif
      CALL XPIQUERYPAR(Parname,'r',buffer1,.TRUE.,.FALSE.,.FALSE.,
     &                 Status)
 
      IF ( DEBug ) THEN
         WRITE (line,*,IOSTAT=j) 'status of ' , Status , 
     &                           ' value is ' //
     &          buffer1(:MIN(len(line)-32,LENACT(buffer1)))
         CALL XWRITE(line,5)
      ENDIF
 
 100  IF ( Status.NE.0 ) RETURN
 
      CALL UPC(buffer1)
      IF ( buffer1.EQ.'INDEF' ) THEN
         Status = 3
         RETURN
      ENDIF
      READ (buffer1,*,ERR=200) Buffer
      IF ( DEBug ) THEN
         WRITE (line,*,IOSTAT=j) 'final return of ',Buffer
         CALL XWRITE(line,5)
      ENDIF
      RETURN
 200  Status = 2
      RETURN
 
      ENTRY UCLGSXD(Parname,Buffer,Status)
 
      CALL XPIDEFAULTPAR(Parname,'r',buffer1,Status)
 
      GOTO 100
c$$$      IMPLICIT NONE
c$$$      CHARACTER*(*) Parname
c$$$      COMPLEX*8 Buffer
c$$$      INTEGER*4 Status
c$$$
c$$$      INTEGER*4 TBLFPR
c$$$      character(80) str1 , str2 , buffer1
c$$$      INTEGER*4 ierr
c$$$
c$$$      INCLUDE 'tbl.inc'
c$$$      INCLUDE 'yaccfor.inc'
c$$$
c$$$      INTEGER*4 i
c$$$      INTEGER*4 j
c$$$      INTEGER*4 LENACT
c$$$      real fpnum, rtmp
c$$$      character(80) dsnafu
c$$$
c$$$      buffer1 = ' '
c$$$      IF ( Status.NE.0 ) RETURN
c$$$
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
c$$$      IF ( TBLptype(i).NE.'r' ) THEN
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
c$$$            buffer1 = SVAl(j)
c$$$*               CALL TBSVPR(Tblpfname,ierr)
c$$$            IF ( INDEX(TBLpupd(i),'l').NE.0 ) TBLpdefl(i) = buffer1
c$$$         ENDIF
c$$$ 100  CONTINUE
c$$$
c$$$      IF ( buffer1.EQ.' ' .AND. INDEX(TBLpupd(i),'q').EQ.0 )
c$$$     &     buffer1 = TBLpdefl(i)
c$$$
c$$$      call xpirange(buffer1,tblpminp(i),tblpmaxp(i),parname,ierr)
c$$$      if (ierr .ne. 0) then
c$$$         buffer1 = ' '
c$$$      end if
c$$$
c$$$      IF ( ((buffer1.EQ.' ' .OR. (.NOT.TBLpstat(i) .and. .not.
c$$$     $     tblstandalone))
c$$$     $     .AND.
c$$$     &     (ierr .ne. 0 .or. INDEX(TBLpupd(i),'h').EQ.0)) ) THEN
c$$$         do while (.true.)
c$$$            CALL XCREAD(TBLpdesc(i)(1:LENACT(TBLpdesc(i)))
c$$$     &                  //'['//TBLpdefl(i)(1:LENACT(TBLpdefl(i)))//']',
c$$$     &                  buffer1,ierr)
c$$$            IF ( ierr.NE.0 ) THEN
c$$$               Status = 200
c$$$               RETURN
c$$$            ENDIF
c$$$            IF ( buffer1.EQ.' ' ) buffer1 = TBLpdefl(i)
c$$$            CALL UPC(buffer1)
c$$$            IF ( buffer1.EQ.'INDEF' ) THEN
c$$$               Status = 3
c$$$               RETURN
c$$$            ENDIF
c$$$            dsnafu = buffer1
c$$$            rtmp = FPNUM(dsnafu,LEN(dsnafu),ierr)
c$$$            IF ( ierr.NE.0 ) THEN
c$$$               CALL XWRITE(' Please enter a real number',5)
c$$$               GOTO 150
c$$$            ENDIF
c$$$      call xpirange(buffer1,tblpminp(i),tblpmaxp(i),parname,ierr)
c$$$      if (ierr .ne. 0) goto 150
c$$$
c$$$*            CALL TBSVPR(Tblpfname,ierr)
c$$$            IF ( INDEX(TBLpupd(i),'l').NE.0 ) TBLpdefl(i) = buffer1
c$$$            GOTO 200
c$$$ 150     ENDDO
c$$$      ENDIF
c$$$ 200  TBLpstat(i) = .FALSE.
c$$$      CALL UPC(buffer1)
c$$$      IF ( buffer1.EQ.'INDEF' ) Status = 3
c$$$      READ (buffer1,*,ERR=1200) Buffer
c$$$      RETURN
c$$$ 1200  Status = 2
c$$$      RETURN
c$$$
c$$$
c$$$
c$$$* Get the default value
c$$$
c$$$      ENTRY UCLGSXD(Parname,Buffer,Status)
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
c$$$      IF ( TBLptype(i).NE.'r' ) THEN
c$$$         Status = 2
c$$$         RETURN
c$$$      ENDIF
c$$$
c$$$      buffer1 = TBLpdefl(i)
c$$$
c$$$      CALL UPC(buffer1)
c$$$      IF ( buffer1.EQ.'INDEF' ) THEN
c$$$         Status = 3
c$$$         RETURN
c$$$      ENDIF
c$$$      READ (buffer1,*,ERR=200) Buffer
c$$$
c$$$      RETURN
 
      END
 
