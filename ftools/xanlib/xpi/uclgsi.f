**==uclgsi.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
* $Id: uclgsi.f,v 3.15 2013/05/21 19:08:47 irby Exp $
* $Log: uclgsi.f,v $
* Revision 3.15  2013/05/21 19:08:47  irby
* Change character*n to character(n) to silence warnings: "Obsolescent
* feature: Old-style character length".
*
* Revision 3.14  2012/02/13 22:03:40  irby
* Reinstate previously-excised usage of TBLpstat (parameter holding status)
* array which is still used by xselect.  This allows the call to
* APE_TRAD_QUERY_STRING when e.g. the user attempts to set a parameter
* which doesn't exist (for example, "set xyz"), rather than going into an
* infinite non-reprompting loop.
*
* Revision 3.13  2011/07/13 20:34:04  irby
* Supersede XPI's prompt functions with APE: routine XPIQUERYPAR has been
* gutted and replaced with a call to ape_trad_query_string.  Was necessary to
* add a call-back from APE to a new routine 'xpi_get_text' to handle "?" as
* input (i.e. call XPIHELP) to avoid burdening APE with this.  xpi_get_text
* also allows XCREAD to correctly perform the necessary voodoo for e.g. xselect.
*
* Revision 3.11  2001/05/24 18:19:25  miket
* Using extra real*8 buffer to keep internal READ from choking on floats
*
* Revision 3.10  1999/02/02 17:36:15  toliver
* revised internal write statements to eliminate possibility of overflow
*
c Revision 3.9  1997/05/16  20:49:09  oneel
c Fixed loop problem with eof
c
* Revision 3.8  1997/02/04 21:54:06  oneel
* update to use fparhelp.  Thanks larry.
*
c Revision 3.7  1997/01/17  21:23:13  oneel
c Add in ? help, bug fix, don't bring out debugging help
c
c Revision 3.6  1997/01/17  21:09:41  oneel
c Add in ? help
c
c Revision 3.5.1.1  1996/04/16  01:39:24  dunfee
c Start of pristine ftools CVS...
c
c Revision 1.2  1995/04/17  17:17:54  oneel
c Removed leading spaces before a number so that FPNUM would work
c correctly
c
*
      SUBROUTINE UCLGSI(Parname,Buffer,Status)
 
C     return an ingeger*4 value for parname
 
      CHARACTER*(*) Parname
      INTEGER Buffer
      INTEGER Status
      character(80) buffer1 , line
      INTEGER LENACT , j
      real*8 rbuf
 
      INCLUDE 'yaccfor.inc'
 
      IF ( DEBug ) THEN
         line = 'uclgsi called for ' // Parname(1:LENACT(Parname))
         CALL XWRITE(line,5)
      ENDIF
      CALL XPIQUERYPAR(Parname,'i',buffer1,.TRUE.,.FALSE.,.FALSE.,
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
      READ (buffer1,*,ERR=210) Buffer
      IF ( DEBug ) THEN
         WRITE (line,*,IOSTAT=j) 'final return of ' , Buffer
         CALL XWRITE(line,5)
      ENDIF
      RETURN
 200  Status = 2
      RETURN
 210  read (buffer1,*,ERR=200) rbuf
      Buffer = rbuf
      IF ( DEBug ) THEN
         WRITE (line,*,IOSTAT=j) 'final return of ' , Buffer
         CALL XWRITE(line,5)
      ENDIF
      RETURN
 
      ENTRY UCLGSID(Parname,Buffer,Status)
 
      CALL XPIDEFAULTPAR(Parname,'i',buffer1,Status)
 
      GOTO 100
 
      END
**==xpidefaultpar.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
 
 
      SUBROUTINE XPIDEFAULTPAR(Parname,Type,Buffer,Status)
 
*     Get the default value
 
 
      CHARACTER*(*) Parname , Type , Buffer
      INTEGER Status , i
 
      INCLUDE 'tbl.inc'
 
      IF ( Status.NE.0 ) RETURN
 
      CALL XPICHECKPAR(Parname,Type,i,Status)
 
      IF ( Status.NE.0 ) RETURN
 
      Buffer = TBLpdefl(i)
      RETURN
      END
**==xpicheckpar.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
      SUBROUTINE XPICHECKPAR(Parname,Type,I,Status)
 
      CHARACTER*(*) Parname , Type
      INTEGER Status
      INTEGER I , TBLFPR , LENACT , j
 
      INCLUDE 'tbl.inc'
 
 
      I = TBLFPR(Parname)
 
      IF ( I.EQ.0 ) THEN
         Status = 1
         RETURN
      ENDIF
 
      IF ( I.GT.TBLpcnt ) THEN
         Status = 1
         RETURN
      ENDIF
 
      RETURN
 
      END
**==xpiquerypar.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
 
 
      SUBROUTINE XPIQUERYPAR(Parname,Type,Buffer1,Isnum,Isstr,Isbool,
     &                       Status)
 
*
*     return a integer*4 value related to parname
*
      IMPLICIT NONE
      CHARACTER*(*) Parname , Buffer1 , Type
      LOGICAL Isnum , Isstr , Isbool
 
      INTEGER APE_TRAD_QUERY_STRING
      INTEGER APE_TRAD_SET_STRING
      INTEGER Status
 
      REAL FPNUM , rtmp
      character(80) str1 , str2 , dsnafu , str3
      INTEGER ierr
 
      INCLUDE 'tbl.inc'
      INCLUDE 'yaccfor.inc'
 
      INTEGER i
      INTEGER j
      INTEGER k
      INTEGER LENACT
      INTEGER inlen
      LOGICAL on
      LOGICAL off
      logical noquestion
      character(100) question
      integer lquestion
 
      ierr=0
 
      IF ( Status.NE.0 ) RETURN
 
* check the parameter request
 
      CALL XPICHECKPAR(Parname,Type,i,Status)
 
      IF ( Status.NE.0 ) RETURN
 
      Buffer1 = ' '
 
* did the user enter it on the command line
 
      str1 = Parname
      CALL UPC(str1)
 
      DO 100 j = 1 , NPArs
         str2 = SPArs(j)
         CALL UPC(str2)
         str3 = SVAl(j)
         CALL UPC(str3)
 
         IF ( Isbool ) THEN
            k = LENACT(str2)
            on = .FALSE.
            off = .FALSE.
            IF ( str2(k:k).EQ.'+' ) THEN
               on = .TRUE.
               str2(k:k) = ' '
            ENDIF
            IF ( str2(k:k).EQ.'-' ) THEN
               off = .TRUE.
               str2(k:k) = ' '
            ENDIF
            IF ( str3.EQ.'on' ) on = .TRUE.
            IF ( str3.EQ.'off' ) off = .TRUE.
            IF ( str3(1:1).EQ.'Y' ) on = .TRUE.
            IF ( str3(1:1).EQ.'N' ) off = .TRUE.
         ENDIF
 
         IF ( str1.EQ.str2 ) THEN
            Buffer1 = SVAl(j)
            IF ( INDEX(TBLpupd(i),'l').NE.0 ) TBLpdefl(i) = Buffer1
            ierr = APE_TRAD_SET_STRING(str2,SVAl(j))
            IF ( Isstr ) THEN
               CALL YSTCLQ1(TBLpdefl(i))
               IF ( TBLpstat(i) ) THEN
                  TBLpstat(i) = .FALSE.
                  RETURN
               ENDIF
            ENDIF
         ENDIF
 100  CONTINUE

      IF (Buffer1.EQ.' ' .or. (.not.TBLpstat(i))) THEN
            ierr = APE_TRAD_QUERY_STRING(Parname,Buffer1)
            IF ( ierr.NE.0 ) THEN
               Status = ierr
               RETURN
            ENDIF
      ENDIF
      TBLpstat(i) = .FALSE.
 
      RETURN
      END
 
* origional code
 
C$$$*
C$$$*     return a integer*4 value related to parname
C$$$*
c$$$      IMPLICIT NONE
c$$$      CHARACTER*(*) Parname
c$$$      INTEGER*4 Buffer
c$$$      INTEGER*4 Status
c$$$
c$$$      REAL FPNUM , rtmp
c$$$      INTEGER*4 TBLFPR
c$$$      character(80) str1 , str2 , buffer1 , dsnafu
c$$$      INTEGER*4 ierr
c$$$
c$$$      INCLUDE 'tbl.inc'
c$$$      INCLUDE 'yaccfor.inc'
c$$$
c$$$      INTEGER*4 i
c$$$      INTEGER*4 j
c$$$      INTEGER*4 LENACT
c$$$
c$$$
c$$$      IF ( Status.NE.0 ) RETURN
c$$$
c$$$      buffer1 = ' '
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
c$$$      IF ( TBLptype(i).NE.'i' ) THEN
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
c$$$*     CALL TBSVPR(Tblpfname,ierr)
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
c$$$      IF ( (buffer1.EQ.' ' .OR. (.NOT.TBLpstat(i) .and. .not.
c$$$     $     tblstandalone))
c$$$     $     .AND.
c$$$     &     (ierr .ne. 0 .or. INDEX(TBLpupd(i),'h').EQ.0) ) THEN
c$$$         DO WHILE ( .TRUE. )
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
c$$$               CALL XWRITE(' Please enter an integer',5)
c$$$               GOTO 150
c$$$            ENDIF
c$$$      call xpirange(buffer1,tblpminp(i),tblpmaxp(i),parname,ierr)
c$$$      if (ierr .ne. 0) goto 150
c$$$
c$$$
c$$$*     CALL TBSVPR(Tblpfname,ierr)
c$$$            IF ( INDEX(TBLpupd(i),'l').NE.0 ) TBLpdefl(i) = buffer1
c$$$            GOTO 200
c$$$ 150     ENDDO
c$$$      ENDIF
c$$$ 200  TBLpstat(i) = .FALSE.
c$$$      CALL UPC(buffer1)
c$$$      IF ( buffer1.EQ.'INDEF' ) THEN
c$$$         Status = 3
c$$$         RETURN
c$$$      ENDIF
c$$$      READ (buffer1,*,ERR=300) Buffer
c$$$      RETURN
c$$$ 300  Status = 2
c$$$      RETURN
c$$$*     Get the default value
c$$$
c$$$      ENTRY UCLGSID(Parname,Buffer,Status)
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
c$$$      IF ( TBLptype(i).NE.'i' ) THEN
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
c$$$      READ (buffer1,*,ERR=300) Buffer
c$$$      RETURN
 

      subroutine xpihelp (parname)


      include 'tbl.inc'
      integer lenact
      integer i,ier
      character(100) cmd
      character*(*) parname

      if (tblstandalone) then

         cmd = tblstandalonecmd
         ier = lenact(cmd)
         do i=1,lenact(cmd)
            if (cmd(i:i) .eq. '.') then
               ier = i-1
               goto 300
            endif
         end do
300      cmd = 'fparhelp '//cmd(1:ier)//' '//parname(1:lenact(parname))
         call spawn(cmd,lenact(cmd),ier)
      end if
      return
      end
