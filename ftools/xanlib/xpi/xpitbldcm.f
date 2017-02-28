**==tbldcm.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
* $Id: xpitbldcm.f,v 3.4 2015/06/11 20:02:35 kaa Exp $
* $Log: xpitbldcm.f,v $
* Revision 3.4  2015/06/11 20:02:35  kaa
* Added the history command to the set of standard gtcom2 commands. This toggles
* writing the history file on and off. The immediate use is in xselect to avoid
* problems when running multiple instances of the program.
*
* Note that adding a new command required putting it in lists in both ldcmds1.f
* and xpitbldcm.f - one of these ought to be redundant.
*
* Tidied up a bunch of compiler warnings.
*
* Revision 3.3  2013/05/21 19:08:47  irby
* Change character*n to character(n) to silence warnings: "Obsolescent
* feature: Old-style character length".
*
* Revision 3.2  1999/02/02 17:38:07  toliver
* revised internal write statements to eliminate possibility of overflow
*
c Revision 3.1  1997/04/03  00:05:40  dunfee
c To solve a problem with a shared variable in two different common blocks
c (in xpi and estec) I've changed the name of tbldcm in xpi to xpitbldcm.
c Before the xanlib/estec version (still the same name) was replacing the
c xpi one but using a different common block for the same variable than
c the other routines in xpi.
c
* Revision 3.5.1.1  1996/04/16 01:39:20  dunfee
* Start of pristine ftools CVS...
*
c Revision 1.2  1995/05/30  19:54:27  oneel
c Ron Zellar's ihf changes
c
* Loads the commands
      SUBROUTINE XPITBLDCM(File_command,Ierr)
 
*
      IMPLICIT NONE
 
      CHARACTER*(*) File_command
      INTEGER Ierr
 
      INCLUDE 'tbl.inc'
 
      INTEGER lun
      INTEGER i
      INTEGER j
      INTEGER LENACT
      character(1000) inline , zwrite
 
      CALL GETLUN(lun)
 
      TBLccnt = 0
      CALL OPENWR(lun,File_command,'old',' ','L',0,1,Ierr)
 
 
c
c hardwired commands
c
      TBLcname(1) = 'alias'
      TBLcdesc(1) = 'rename command string'
      TBLcname(2) = 'log'
      TBLcdesc(2) = 'output to log file'
      TBLcname(3) = 'chatter'
      TBLcdesc(3) = 'output verbosity'
      TBLcname(4) = 'script'
      TBLcdesc(4) = 'write commands to a file'
      TBLcname(5) = 'recall'
      TBLcdesc(5) = 'recall command'
      TBLcname(6) = 'reexecute'
      TBLcdesc(6) = 'reexecute commands'
      TBLcname(7) = 'buffer'
      TBLcdesc(7) = 'recall by terminal ^'
      TBLcname(8) = 'prompt'
      TBLcdesc(8) = 'prompt default'
      TBLcname(9) = 'keywords'
      TBLcdesc(9) = 'list keywords'
      TBLcname(10) = 'debug'
      TBLcdesc(10) = 'toggle debugging'
      TBLcname(11) = 'dumppar'
      TBLcdesc(11) = 'dump parameters'
      TBLcname(12) = 'dumpcmd'
      TBLcdesc(12) = 'dump commands'
      TBLcname(13) = 'dumpkey'
      TBLcdesc(13) = 'dump keytable'
      TBLcname(14) = 'dumpkwd'
      TBLcdesc(14) = 'dump keyword table'
      TBLcname(15) = 'lparm'
      TBLcdesc(15) = 'list parameters'
      TBLcname(16) = 'help'
      TBLcdesc(16) = 'help facility'
      TBLcname(17) = 'history'
      TBLcdesc(17) = 'toggle recording of history'
 
 
 
      TBLccnt = 17
 
      DO 100 i = TBLccnt + 1 , TBLCMAX
 
 
         TBLcacce(i) = ' '
         TBLcwtyp(i) = ' '
 100  CONTINUE
 
      IF ( Ierr.NE.0 ) THEN
         WRITE (zwrite,99001)
     &      File_command(:MIN(len(zwrite)-42,LENACT(File_command))),Ierr
         CALL XWRITE(zwrite,5)
         RETURN
      ELSE
 
 
         READ (lun,'(a)',IOSTAT=Ierr) inline
 
         DO WHILE ( Ierr.EQ.0 )
            TBLccnt = TBLccnt + 1
            IF ( TBLccnt.GT.TBLCMAX ) THEN
               CALL XWRITE(' command table too small',5)
               TBLccnt = TBLCMAX
               GOTO 200
            ENDIF
 
            i = 1
            j = 1
            TBLcname(TBLccnt) = ' '
            TBLcdesc(TBLccnt) = ' '
            TBLcacce(TBLccnt) = ' '
            TBLcwtyp(TBLccnt) = ' '
            DO WHILE ( (inline(i:i).NE.',') .AND. (i.LE.LEN(inline))
     &                 .AND. (j.LE.LEN(TBLcname(TBLccnt))) )
               TBLcname(TBLccnt)(j:j) = inline(i:i)
               i = i + 1
               j = j + 1
            ENDDO
            j = 1
            i = i + 1
            DO WHILE ( (inline(i:i).NE.',') .AND. (i.LE.LEN(inline))
     &                 .AND. (j.LE.LEN(TBLcdesc(TBLccnt))) )
               TBLcdesc(TBLccnt)(j:j) = inline(i:i)
               i = i + 1
               j = j + 1
            ENDDO
            j = 1
            i = i + 1
            DO WHILE ( (inline(i:i).NE.',') .AND. (i.LE.LEN(inline))
     &                 .AND. (j.LE.LEN(TBLcacce(TBLccnt))) )
               TBLcacce(TBLccnt)(j:j) = inline(i:i)
               i = i + 1
               j = j + 1
            ENDDO
            j = 1
            i = i + 1
            DO WHILE ( (inline(i:i).NE.',') .AND. (i.LE.LEN(inline))
     &                 .AND. (j.LE.LEN(TBLcwtyp(TBLccnt))) )
               TBLcwtyp(TBLccnt)(j:j) = inline(i:i)
               i = i + 1
               j = j + 1
            ENDDO
 
            CALL YSTCLS1(TBLcname(TBLccnt))
            CALL YSTCLS1(TBLcdesc(TBLccnt))
            CALL YSTCLS1(TBLcacce(TBLccnt))
            CALL YSTCLS1(TBLcwtyp(TBLccnt))
            CALL YSTCLQ1(TBLcname(TBLccnt))
            CALL YSTCLQ1(TBLcdesc(TBLccnt))
            CALL YSTCLQ1(TBLcacce(TBLccnt))
            CALL YSTCLQ1(TBLcwtyp(TBLccnt))
            READ (lun,'(a)',IOSTAT=Ierr) inline
 
         ENDDO
         Ierr = 0
      ENDIF
 
 
 200  CLOSE (lun)
      CALL FRELUN(lun)
 
 
      RETURN
99001 FORMAT (' Error opening load_commands file ',a,' no ',i4)
      END
