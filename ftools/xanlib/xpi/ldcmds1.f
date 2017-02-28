**==ldcmds1.spg  processed by SPAG 4.50F  at 15:17 on 26 Aug 1994
      SUBROUTINE LDCMDS1(Infile,Ierr)
* $Id: ldcmds1.f,v 3.8 2015/06/11 20:02:35 kaa Exp $
* $Log: ldcmds1.f,v $
* Revision 3.8  2015/06/11 20:02:35  kaa
* Added the history command to the set of standard gtcom2 commands. This toggles
* writing the history file on and off. The immediate use is in xselect to avoid
* problems when running multiple instances of the program.
*
* Note that adding a new command required putting it in lists in both ldcmds1.f
* and xpitbldcm.f - one of these ought to be redundant.
*
* Tidied up a bunch of compiler warnings.
*
* Revision 3.7  2013/05/21 19:08:47  irby
* Change character*n to character(n) to silence warnings: "Obsolescent
* feature: Old-style character length".
*
* Revision 3.6  1999/02/02 17:33:55  toliver
* revised internal write statements to eliminate possibility of overflow
*
c Revision 3.5.1.1  1996/04/16  01:39:03  dunfee
c Start of pristine ftools CVS...
c
c Revision 1.2  1995/05/30  19:50:08  oneel
c Ron Zellar's change for interactive help
c
c
c  load valid browse commands from a file - Nick early 1990
c
c updated by Nick 28.05.91 to use openwr and new common structure
c changed name from load_commands
c
      INCLUDE 'commands.inc'
      INCLUDE 'estec.inc'
c
c  list browse commands
c
      CHARACTER*(*) Infile
      character(80) string
      INTEGER ispace , isp , iend , Ierr , ist , lrecl
      character(16) temp_com(ZNCOM) , temp1(ZNCOM)
      character(30) temp_descrip(ZNCOM)
c
      INTEGER io , LENACT , irec , po(ZNCOM) , n
      LOGICAL list
c
      Ierr = 0
      CALL GETLUN(io)
c      OPEN (UNIT=io,FILE=infile,STATUS='old',shared,ERR=200)
c
      lrecl = 0
      CALL OPENWR(io,Infile,'OLD',' ',' ',lrecl,1,Ierr)
c
      IF ( Ierr.NE.0 ) THEN
         WRITE(ZWRite,99001)Infile(:MIN(len(ZWRite)-42,LENACT(Infile))),
     &                      Ierr
         CALL XWRITE(ZWRite,5)
         RETURN
      ENDIF
c
      ZLIst_no = 0
      ZLIst_nolist = 0
c
c hardwired commands
c
      ZCOm_name(1) = 'alias'
      ZCOm_descrip(1) = 'rename command string'
      ZCOm_name(2) = 'log'
      ZCOm_descrip(2) = 'output to log file'
      ZCOm_name(3) = 'chatter'
      ZCOm_descrip(3) = 'output verbosity'
      ZCOm_name(4) = 'script'
      ZCOm_descrip(4) = 'write commands to a file'
      ZCOm_name(5) = 'recall'
      ZCOm_descrip(5) = 'recall command'
      ZCOm_name(6) = 'reexecute'
      ZCOm_descrip(6) = 'reexecute commands'
      ZCOm_name(7) = 'buffer'
      ZCOm_descrip(7) = 'recall by terminal ^'
      ZCOm_name(8) = '?'
      ZCOm_descrip(8) = 'list keywords'
      ZCOm_name(9) = '??'
      ZCOm_descrip(9) = 'list commands'
      ZCOm_name(10) = 'prompt'
      ZCOm_descrip(10) = 'prompt default'
      ZCOm_name(11) = 'keywords'
      ZCOm_descrip(11) = 'list keywords'
      ZCOm_name(12) = 'debug'
      ZCOm_descrip(12) = 'toggle debugging'
      ZCOm_name(13) = 'dumppar'
      ZCOm_descrip(13) = 'dump paramter table'
      ZCOm_name(14) = 'dumpcmd'
      ZCOm_descrip(14) = 'dump command table'
      ZCOm_name(15) = 'dumpkey'
      ZCOm_descrip(15) = 'dump key table'
      ZCOm_name(16) = 'dumpkwd'
      ZCOm_descrip(16) = 'dump keyword table'
      ZCOm_name(17) = 'lparm'
      ZCOm_descrip(17) = 'list parameters'
      ZCOm_name(18) = 'help'
      ZCOm_descrip(18) = 'help facility'
      ZCOm_name(19) = 'history'
      ZCOm_descrip(19) = 'toggle history saving'
      irec = 19
      n = 1
      DO WHILE ( n.LE.irec )
         ZCOm_list(n) = ZCOm_name(n)
         CALL UPC(ZCOm_name(n))
         ZCOm_descrip_list(n) = ZCOm_descrip(n)
         n = n + 1
      ENDDO
      ZLIst_no = irec
c
      DO WHILE ( irec.LT.ZNCOM )
         CALL RDULKC(io,string,iend,Ierr)
         CALL UNLOCK(io)
         IF ( iend.EQ.1 ) GOTO 100
         IF ( Ierr.NE.0 ) THEN
c
            ZWRite = ' Error: reading ' // Infile(:LENACT(Infile))
            CALL XWRITE(ZWRite,5)
            RETURN
         ELSE
            CALL RMVLBK(string)
            irec = irec + 1
            ist = 2
            list = .FALSE.
            IF ( string(1:1).NE.'*' ) THEN
               list = .TRUE.
               ist = 1
            ENDIF
c
            iend = LENACT(string)
            ispace = ist
            DO WHILE ( string(ispace:ispace).NE.' ' .AND. 
     &                 ispace.LT.iend )
               ispace = ispace + 1
            ENDDO
c
            IF ( ispace.EQ.ist ) THEN
               WRITE (ZWRite,99002)
               CALL XWRITE(ZWRite,5)
               irec = irec - 1
            ELSEIF ( ispace.EQ.iend ) THEN
               WRITE (ZWRite,99003)
               CALL XWRITE(ZWRite,5)
               irec = irec - 1
            ELSE
               isp = ispace
               IF ( isp.GT.16 ) isp = 16
               ZCOm_name(irec) = string(ist:isp)
               ZCOm_descrip(irec) = string(ispace+1:iend)
               CALL RMVLBK(ZCOm_descrip(irec))
c
               IF ( list ) THEN
                  ZLIst_no = ZLIst_no + 1
                  ZCOm_list(ZLIst_no) = ZCOm_name(irec)
                  ZCOm_descrip_list(ZLIst_no) = ZCOm_descrip(irec)
               ELSE
                  ZLIst_nolist = ZLIst_nolist + 1
                  ZCOm_nolist(ZLIst_nolist) = ZCOm_name(irec)
                  ZCOm_descrip_nolist(ZLIst_nolist) = ZCOm_descrip(irec)
               ENDIF
               CALL UPC(ZCOm_name(irec))
            ENDIF
         ENDIF
c
      ENDDO
 100  ZCOm_no = irec
c     	write(zwrite,876)zcom_no, zlist_no, zncom
c876	format(' there are',i5,' browse commands, with', i5,
c     &' visible and',i5,' possible')
c     	call xwrite(zwrite,5)
      IF ( ZNCOM-ZCOm_no.LT.5 ) WRITE (ZWRite,99004)
c
      IF ( ZCOm_no.EQ.ZNCOM ) THEN
         WRITE (ZWRite,99005)
         CALL XWRITE(ZWRite,5)
      ENDIF
c
      CLOSE (io)
      CALL FRELUN(io)
c
c put visible in alphabetical order
c  sort on the command name
c
      n = 1
      DO WHILE ( n.LE.ZLIst_no )
         temp_com(n) = ZCOm_list(n)
         temp_descrip(n) = ZCOm_descrip_list(n)
         po(n) = n
         n = n + 1
      ENDDO
c
      CALL QSORSM(ZLIst_no,ZCOm_descrip_list,temp1,po)
c
      n = 1
      DO WHILE ( n.LE.ZLIst_no )
         ZCOm_list(n) = temp_com(po(n))
         ZCOm_descrip_list(n) = temp_descrip(po(n))
         n = n + 1
      ENDDO
c
      RETURN
99001 FORMAT (' Error opening load_commands file ',a,' no ',i4)
99002 FORMAT (' Warning: blank command line ignored')
99003 FORMAT (' Warning: Command 80 characters long ignored')
99004 FORMAT (' Warning: only 5 free commands left, increase zncom')
99005 FORMAT (' Warning: maximum number of commands exceeded')
      END
