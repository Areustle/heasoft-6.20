**==tbldky.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
* $Id: tbldky.f,v 3.7 2013/05/21 19:08:47 irby Exp $
* $Log: tbldky.f,v $
* Revision 3.7  2013/05/21 19:08:47  irby
* Change character*n to character(n) to silence warnings: "Obsolescent
* feature: Old-style character length".
*
* Revision 3.6  1999/02/02 17:35:08  toliver
* revised internal write statements to eliminate possibility of overflow
*
c Revision 3.5.1.1  1996/04/16  01:39:20  dunfee
c Start of pristine ftools CVS...
c
c Revision 1.3  1995/12/06  16:20:47  oneel
c changed a print to a xwrite
c
c Revision 1.2  1995/05/30  19:56:18  oneel
c Ron Zellar's ihf changes
c
* Loads the keytables
      SUBROUTINE TBLDKY(File_keytable,Ierr)
 
*
      IMPLICIT NONE
 
      CHARACTER*(*) File_keytable
      INTEGER*4 Ierr
 
      INCLUDE 'tbl.inc'
 
      INTEGER*4 lun
      INTEGER*4 i
      INTEGER*4 j
      character(1000) inline
      INTEGER*4 LENACT
      character(200) zwrite
      character(20) str1
 
      CALL GETLUN(lun)
 
      TBLkname(1) = '?'
      TBLkparm(1) = 'hidden'
      TBLkposo(1) = 0
      TBLkclus(1) = ' '
      TBLkexcl(1) = 0
      TBLkname(2) = '?'
      TBLkparm(2) = 'full'
      TBLkposo(2) = 0
      TBLkclus(2) = ' '
      TBLkexcl(2) = 0
      TBLkname(3) = '?'
      TBLkparm(3) = 'name'
      TBLkposo(3) = 1
      TBLkclus(3) = ' '
      TBLkexcl(3) = 0
      TBLkname(4) = '??'
      TBLkparm(4) = 'hidden'
      TBLkposo(4) = 0
      TBLkclus(4) = ' '
      TBLkexcl(4) = 0
      TBLkname(5) = '??'
      TBLkparm(5) = 'full'
      TBLkposo(5) = 0
      TBLkclus(5) = ' '
      TBLkexcl(5) = 0
      TBLkname(6) = 'script'
      TBLkparm(6) = 'xpifname'
      TBLkposo(6) = 1
      TBLkclus(6) = ' '
      TBLkexcl(6) = 0
      TBLkname(7) = 'alias'
      TBLkparm(7) = 'show'
      TBLkposo(7) = 0
      TBLkclus(7) = ' '
      TBLkexcl(7) = 0
      TBLkname(8) = 'alias'
      TBLkparm(8) = 'all'
      TBLkposo(8) = 0
      TBLkclus(8) = ' '
      TBLkexcl(8) = 0
      TBLkname(9) = 'alias'
      TBLkparm(9) = 'delete'
      TBLkposo(9) = 0
      TBLkclus(9) = ' '
      TBLkexcl(9) = 0
      TBLkname(10) = 'alias'
      TBLkparm(10) = 'system'
      TBLkposo(10) = 0
      TBLkclus(10) = ' '
      TBLkexcl(10) = 0
      TBLkname(11) = 'alias'
      TBLkparm(11) = 'previous'
      TBLkposo(11) = 0
      TBLkclus(11) = ' '
      TBLkexcl(11) = 0
      TBLkname(12) = 'alias'
      TBLkparm(12) = 'new'
      TBLkposo(12) = 0
      TBLkclus(12) = ' '
      TBLkexcl(12) = 0
      TBLkname(13) = 'alias'
      TBLkparm(13) = 'aname'
      TBLkposo(13) = 1
      TBLkclus(13) = ' '
      TBLkexcl(13) = 0
      TBLkname(14) = 'alias'
      TBLkparm(14) = 'acmd'
      TBLkposo(14) = 2
      TBLkclus(14) = ' '
      TBLkexcl(14) = 0
      TBLkname(15) = 'recall'
      TBLkparm(15) = 'recallwhat'
      TBLkposo(15) = 1
      TBLkclus(15) = ' '
      TBLkexcl(15) = 0
      TBLkname(16) = 'lparm'
      TBLkparm(16) = 'name'
      TBLkposo(16) = 1
      TBLkclus(16) = ' '
      TBLkexcl(16) = 0
      TBLkname(17) = 'log'
      TBLkparm(17) = 'xpifname'
      TBLkposo(17) = 1
      TBLkclus(17) = ' '
      TBLkexcl(17) = 0
      TBLkname(18) = 'buffer'
      TBLkparm(18) = 'aname'
      TBLkposo(18) = 1
      TBLkclus(18) = ' '
      TBLkexcl(18) = 0
      TBLkname(19) = 'reexecute'
      TBLkparm(19) = 'recallwhat'
      TBLkposo(19) = 1
      TBLkclus(19) = ' '
      TBLkexcl(19) = 0
      TBLkname(20) = 'help'
      TBLkparm(20) = 'helptopic'
      TBLkposo(20) = 1
      TBLkclus(20) = ' '
      TBLkexcl(20) = 0
      TBLkcnt =20 
c      OPEN (lun,FILE=File_keytable,IOSTAT=Ierr,STATUS='old')
      CALL OPENWR(lun,File_keytable,'old',' ',' ',0,1,Ierr)
 
 
      IF ( Ierr.NE.0 ) THEN
         WRITE (zwrite,99001)
     &        File_keytable(:MIN(len(zwrite)-42,LENACT(File_keytable))),
     &        Ierr
         CALL XWRITE(zwrite,5)
         RETURN
      ELSE
 
 
         READ (lun,'(a)',IOSTAT=Ierr) inline
 
         DO WHILE ( Ierr.EQ.0 )
            TBLkcnt = TBLkcnt + 1
            IF ( TBLkcnt.GT.TBLKMAX ) THEN
               CALL XWRITE(' keytable table too small',5)
               write (inline,*) ' TBLKCNT is ' , TBLkcnt
               call xwrite (inline,5)
               TBLkcnt = TBLKMAX
               GOTO 100
            ENDIF
 
            i = 1
            j = 1
            TBLkname(TBLkcnt) = ' '
            TBLkparm(TBLkcnt) = ' '
            TBLkposo(TBLkcnt) = 0
            TBLkclus(TBLkcnt) = ' '
            TBLkexcl(TBLkcnt) = 0
            DO WHILE ( (inline(i:i).NE.',') .AND. (i.LE.LEN(inline))
     &                 .AND. (j.LE.LEN(TBLkname(TBLkcnt))) )
               TBLkname(TBLkcnt)(j:j) = inline(i:i)
               i = i + 1
               j = j + 1
            ENDDO
            j = 1
            i = i + 1
            DO WHILE ( (inline(i:i).NE.',') .AND. (i.LE.LEN(inline))
     &                 .AND. (j.LE.LEN(TBLkparm(TBLkcnt))) )
               TBLkparm(TBLkcnt)(j:j) = inline(i:i)
               i = i + 1
               j = j + 1
            ENDDO
            j = 1
            i = i + 1
            str1 = ' '
            DO WHILE ( (inline(i:i).NE.',') .AND. (i.LE.LEN(inline))
     &                 .AND. (j.LE.LEN(str1)) )
               str1(j:j) = inline(i:i)
               i = i + 1
               j = j + 1
            ENDDO
            READ (str1,*,IOSTAT=Ierr) TBLkposo(TBLkcnt)
            j = 1
            i = i + 1
            DO WHILE ( (inline(i:i).NE.',') .AND. (i.LE.LEN(inline))
     &                 .AND. (j.LE.LEN(TBLkclus(TBLkcnt))) )
               TBLkclus(TBLkcnt)(j:j) = inline(i:i)
               i = i + 1
               j = j + 1
            ENDDO
 
            j = 1
            i = i + 1
            str1 = ' '
            DO WHILE ( (inline(i:i).NE.',') .AND. (i.LE.LEN(inline))
     &                 .AND. (j.LE.LEN(str1)) )
               str1(j:j) = inline(i:i)
               i = i + 1
               j = j + 1
            ENDDO
            READ (str1,*,IOSTAT=Ierr) TBLkexcl(TBLkcnt)
 
 
            CALL YSTCLS1(TBLkname(TBLkcnt))
            CALL YSTCLS1(TBLkparm(TBLkcnt))
            CALL YSTCLS1(TBLkclus(TBLkcnt))
            CALL YSTCLQ1(TBLkname(TBLkcnt))
            CALL YSTCLQ1(TBLkparm(TBLkcnt))
            CALL YSTCLQ1(TBLkclus(TBLkcnt))
            READ (lun,'(a)',IOSTAT=Ierr) inline
 
         ENDDO
         Ierr = 0
      ENDIF
 
 
 100  CLOSE (lun)
      CALL FRELUN(lun)
 
 
      RETURN
99001 FORMAT (' Error opening load_commands file ',a,' no ',i4)
      END
