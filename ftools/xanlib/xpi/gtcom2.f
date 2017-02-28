**==gtcom2.spg  processed by SPAG 4.50F  at 15:17 on 26 Aug 1994
* $Id: gtcom2.f,v 3.15 2015/06/11 20:02:35 kaa Exp $
* $Log: gtcom2.f,v $
* Revision 3.15  2015/06/11 20:02:35  kaa
* Added the history command to the set of standard gtcom2 commands. This toggles
* writing the history file on and off. The immediate use is in xselect to avoid
* problems when running multiple instances of the program.
*
* Note that adding a new command required putting it in lists in both ldcmds1.f
* and xpitbldcm.f - one of these ought to be redundant.
*
* Tidied up a bunch of compiler warnings.
*
* Revision 3.14  2013/05/21 19:08:47  irby
* Change character*n to character(n) to silence warnings: "Obsolescent
* feature: Old-style character length".
*
* Revision 3.13  2004/12/14 20:44:45  irby
* Undo revision 3.11 which wasn't the right fix to the openwr / xselect.hty
* problem.  I think the right fix would be to precede filenames in calls to
* openwr in the udc1 routine (called by gtcom2) with char(92), but at this
* point it looks like it will be best to modify openwr to not lowercase
* anything anymore.
*
* Revision 3.12  2004/11/03 21:11:19  kaa
* The name of the help file was not being SAVEd.
*
* Revision 3.11  2004/07/28 19:22:43  irby
* Precede filename with a backslash when calling LOCASE to prevent
* lower casing of directory names.
*
* Revision 3.10  1999/12/03 14:44:45  peachey
* Migrated tbldkw.f to tbldkw1.f to prevent collision with estec's version.
*
* Revision 3.9  1999/01/01 02:51:27  guerber
* increased file-name strings to 255; replaced some internal writes with
* assignments to avoid potential overflows
*
* Revision 3.8  1998/03/25 20:48:24  peachey
* Added a '1' to xinird.f and xnxtrd.f to distiguish these routines from
* routines of the same name in xparse
*
* Revision 3.7  1997/04/03 00:05:36  dunfee
* To solve a problem with a shared variable in two different common blocks
* (in xpi and estec) I've changed the name of tbldcm in xpi to xpitbldcm.
* Before the xanlib/estec version (still the same name) was replacing the
* xpi one but using a different common block for the same variable than
* the other routines in xpi.
*
* Revision 3.6  1996/07/30 15:37:33  miket
* MJT 30July96 initializing cwd variable
*
c Revision 3.5.1.1  1996/04/16  01:39:01  dunfee
c Start of pristine ftools CVS...
c
c Revision 1.8  1995/06/30  12:37:55  oneel
c Forgot an include file which broke tbldpr
c
c Revision 1.7  1995/06/15  20:03:30  oneel
c tblextrapar needs to be true
c
c Revision 1.6  1995/06/15  12:53:16  oneel
c added tblextrapar
c
c Revision 1.5  1995/06/14  18:06:35  oneel
c Made sure that tblstandalone was set to false before loading .par file
c
c Revision 1.4  1995/06/06  14:34:18  oneel
c Add in tbldpr_reset call before tbldpr
c
c Revision 1.3  1995/05/30  19:43:03  oneel
c Ron Zellar's help changes
c
c Revision 1.2  1995/05/30  19:39:39  oneel
c Test RCS directory
c
* BEGIN ===================================================== BEO 92/1/7
* Edits from lib/estec/gtcom.for on 92/1/7
*
*      SUBROUTINE GTCOM(String,Parse,Prompt_in,Command,Command_no,Sysdir,
*     &                 Program,Version,Idone)
      SUBROUTINE GTCOM2(String,Parse,Prompt_in,Command,Command_no,
     &                  Sysdisk,Sysdir,Program,Version,Idone)
* END ======================================================= BEO 92/1/7
c
      INCLUDE 'estec.inc'
      INCLUDE 'commands.inc'
      INCLUDE 'yaccfor.inc'
      INCLUDE 'tbl.inc'
c
c   changed by nick 7/2/90 to use a command file.
c   made program independent and changed to getcom by nick 29/5/91
c   made into gtcom which passes the parsed string in and out
c                                                     nick 24/6/91
* input
* all char lengths are recommended
* sysdir (c*30) - the system directory where the commands files are kept
* program (c*10) - the name of the program running (all files are called this)
* version (c*6) - the version number of the program
* prompt_in (c*80) - user defined prompt string, if ' '
*                    then uses [nn]program>
* output
* command_no (i*4) - current command number
* command (c*50 - current command
* idone (i*4) - set to 1 if the command has already been executed in getcom
*             - set to -1 if there is a fatal error
c   all commands are in a file which is read in to common by load_commands.
c   The format is obvious.
c   if you dont want the command to appear
c   under the ? command, then preceed it with a '*'. put these at the
c   end to keep things tidy. they will still be valid commands.
c
c if idone = 1 the command is already found
c program is the program name
c sysdir is where the the system udc file goes
c
      CHARACTER*(*) String
      CHARACTER*(*) Command , Prompt_in , Version , Program , Sysdir
      character(255) file_system , file_user , file_type , file_recall
      character(255) rootname , file_command , tmpstr, file_help
      character(80)  helptopic
      INTEGER Command_no , Idone , n_max , Parse
      LOGICAL from_recall , udc_found , isthere
      character(20) form
      character(80) prompt
      character(256) user_def_command , command_in
      character(256) tstring , previous_command
      character(16) NOSTAR
      INTEGER start(10) , npipe , npip , ico
      INTEGER in , jqual , status , ierr
      INTEGER LENACT , lprompt , ica
      INTEGER flag , number , tct
      LOGICAL qskip , first_time , keyfound
      LOGICAL prompt_on , YSTCMD
c
      character(80) seshead
      character(512) log_file , script_file
      INTEGER ui_no
      LOGICAL new , page , quoted
c

      INTEGER SAVCOM , nccom , nacom
      PARAMETER (SAVCOM=20)
      character(255) savstrg(SAVCOM)

* BEGIN ===================================================== BEO 92/1/7
      CHARACTER*(*) Sysdisk
      character(255) sysdir1
      character(255) cwd
      logical dolog, dohist
* END ======================================================= BEO 92/1/7
c
      SAVE npip, npipe, tstring, start, previous_command
      SAVE file_system, file_user, file_recall, file_help
      SAVE nccom, nacom, savstrg
      SAVE from_recall, first_time, prompt_on
      SAVE dolog, dohist

      DATA first_time/.TRUE./
      DATA npip, npipe, nccom, nacom/4*0/
      DATA dolog/.TRUE./
      DATA dohist/.TRUE./

* Clean up the stored arrays

      CALL YINIT
c
c pass in parse string
c
      ZSTring = String
      ZPArse = Parse
c
c fi command_no is zero, set up initial parameters
c
      IF ( first_time ) THEN
         prompt_on = .FALSE.
         first_time = .FALSE.
c
         Command_no = 0
         previous_command = ' '
         from_recall = .FALSE.
c
c get directories, make the file names
c
* BEGIN ======================================================= BEO 92/1/7
         sysdir1 = Sysdir
         Sysdir = ' '
         CALL PTEND(Sysdisk(1:LENACT(Sysdisk)),
     &              sysdir1(1:LENACT(sysdir1)),Sysdir)
* END ========================================================= BEO 92/1/7
         ZSYsdir = Sysdir
C         CALL LOCASE(Zsysdir)
         CALL GETDIR(ZCUrrent_directory)
C         CALL LOCASE(Zcurrent_directory)
c
c get dir to put user generated system files
c
         CALL GETUDR(rootname)
c
c load the commands
c
         file_command = Sysdir(:LENACT(Sysdir))
     &                  //Program(:LENACT(Program))
     &                  //Version(:LENACT(Version))//'.cmd'

         CALL GTBUFNOTSTAND
C         CALL LOCASE(file_command)
         CALL LDCMDS1(file_command,ierr)
         IF ( ierr.NE.0 ) CALL LEAVE

* BEGIN ========================================================= RSZ 95/5/18

c
c Set the help file
c
         file_help = Sysdir(:LENACT(Sysdir))
     &                  //Program(:LENACT(Program))
     &                  //Version(:LENACT(Version))//'.hlp'

         INQUIRE(FILE=file_help,EXIST=isthere)

         IF ( .NOT. isthere ) THEN
            file_help = Sysdir(:LENACT(Sysdir))
     &                  //Program(:LENACT(Program))
     &                  //Version(:LENACT(Version))//'.shf'
         ENDIF

* END =========================================================== RSZ 95/5/18

* BEGIN ========================================================= BEO 92/1/7
         file_command = Sysdir(:LENACT(Sysdir))
     &                  //Program(:LENACT(Program))
     &                  //Version(:LENACT(Version))//'.cmd1'

C         CALL LOCASE(file_command)
         CALL XPITBLDCM(file_command,ierr)
         IF ( ierr.NE.0 ) CALL LEAVE
         file_command = Sysdir(:LENACT(Sysdir))
     &                  //Program(:LENACT(Program))
     &                  //Version(:LENACT(Version))//'.kw'

C         CALL LOCASE(file_command)
         CALL TBLDKW1(file_command,ierr)
         IF ( ierr.NE.0 ) CALL LEAVE
         file_command = Sysdir(:LENACT(Sysdir))
     &                  //Program(:LENACT(Program))
     &                  //Version(:LENACT(Version))//'.key'

C         CALL LOCASE(file_command)
         CALL TBLDKY(file_command,ierr)
         IF ( ierr.NE.0 ) CALL LEAVE
*         file_command = Sysdir(:LENACT(Sysdir))
*     &                   // Program(:LENACT(Program)) // '.par'

C         CALL LOCASE(file_command)
         tmpstr = Program(:LENACT(Program))//Version(:LENACT(Version))
         CALL TBFDPR(tmpstr(1:LENACT(tmpstr)),Sysdisk,sysdir1,
     &               file_command,ierr)

C         CALL LOCASE(file_command)
         call tbldpr_reset
         tblextrapar = .true.
         CALL TBLDPR(file_command,ierr)
         IF ( ierr.NE.0 ) CALL LEAVE
         file_command = Sysdir(:LENACT(Sysdir))
     &                  //Program(:LENACT(Program))//'.udc1'

C         CALL LOCASE(file_command)
         CALL TBLDAL(file_command,ierr)


* open log file

C         CALL LOCASE(Program)
         log_file = Program(:LENACT(Program))//'.log'
         CALL GTDELLOG(log_file)
         seshead = Program(1:LENACT(Program))
     &             //' '//Version(1:LENACT(Version))//' '
         CALL GETTIM(seshead(LENACT(seshead)+2:))
         seshead = seshead
         CALL GETDAT(seshead(LENACT(seshead)+2:))
         ZSTring = 'LOG'
         ZPArse = 4
         if (dolog) then
            CALL SETLOG(ZSTring,ZPArse,log_file,seshead)
         end if
         Idone = 1
C         log_file = '+'//log_file
C         call logger(6,rbuf,nbuf,log_file,ltmp)
         cwd=' '
         CALL XPICWD(cwd)
         log_file = '+'//cwd(1:LENACT(cwd))//log_file
         if (dolog) then
            CALL XPISETENV('FLOGFILE',log_file(:LENACT(log_file)))
         end if



* END ====================================================== BEO 92/1/7

c
c udc files
c
         file_user = rootname(:LENACT(rootname))
     &               //Program(:LENACT(Program))//'.udc'
C         CALL LOCASE(file_user)
         file_system = Sysdir(:LENACT(Sysdir))
     &                 //Program(:LENACT(Program))//'.udc'
C         CALL LOCASE(file_system)
c
c command recall
c
         file_recall = rootname(:LENACT(rootname))
     &                 //Program(:LENACT(Program))//'.hty'
C         CALL LOCASE(file_recall)
c
      ELSE
c
         previous_command = ZSTring
      ENDIF
      Command_no = Command_no + 1
      Idone = 0
c
c is it a user defined prompt, or the default?
c
      IF ( Prompt_in.EQ.' ' .OR. prompt_on ) THEN
         prompt = ' '
         prompt(1:1) = '['
         ica = INT(ALOG10(REAL(Command_no))) + 1
         CALL CRTFMT(ica,form,1)
         WRITE (prompt(2:1+ica),form) Command_no
         prompt = prompt(:LENACT(prompt))
     &            //']'//Program(:LENACT(Program))//'>'
      ELSE
         prompt = Prompt_in
      ENDIF
      lprompt = LENACT(prompt)
      lprompt = MIN(lprompt,70)
 100  DO WHILE ( .TRUE. )
c
c read in command line
c
         ierr = 0
         IF ( npipe.EQ.0 ) THEN
            ZPArse = 0
c
c command from reexecute
c
            IF ( nacom.GE.1 ) THEN
               CALL XCMGET(tstring,ZPArse,savstrg,SAVCOM,nccom,nacom,
     &                     ierr)
               IF ( ierr.NE.0 ) THEN
                  CALL XNXTRD1(prompt(1:lprompt),tstring,ZPArse,ierr,
     &                        *600)
                  IF ( ierr.NE.0 ) GOTO 200
               ELSE
                  zwrite = ' ' // prompt(:LENACT(prompt)) //
     &                ' ' // tstring(:LENACT(tstring))
                  ierr = 0
                  CALL XWRITE(ZWRite,5)
               ENDIF
c
c command from history
c
            ELSEIF ( from_recall ) THEN
               zwrite = ' ' // prompt(:LENACT(prompt)) //
     &             ' ' // zstring(:LENACT(zstring))
               tstring = ZSTring(:LENACT(ZSTring))
               CALL XWRITE(ZWRite,5)
               CALL YPRINT
C               print *,'Before xpiparsestr'
               CALL XPIPARSTR(ZSTring)
C               print *,'After xpiparsestr'
C               print *,'zstring is ',zstring
               CALL YPRINT
            ELSE
               CALL XNXTRD1(prompt(1:lprompt),tstring,ZPArse,ierr,*600)
               IF ( ierr.NE.0 ) GOTO 200
               nacom = 0
            ENDIF
C            from_recall = .FALSE.
c
c piping
c
            npipe = 1
            npip = 1
            start(1) = 0
            DO 120 ico = 2 , 10
               start(ico) = 255
 120        CONTINUE
            quoted = .FALSE.
            DO 140 ico = 1 , 255
               IF ( tstring(ico:ico).EQ.'"' ) quoted = .NOT.quoted
               IF ( tstring(ico:ico).EQ.'|' .AND. .NOT.quoted ) THEN
                  npipe = npipe + 1
                  start(npipe) = ico
               ENDIF
 140        CONTINUE
         ELSE
            npip = npip + 1
            ZPArse = 0
         ENDIF
         npipe = npipe - 1
         ZSTring = tstring(start(npip)+1:start(npip+1)-1)
C         call xpiparstr(zstring)

         udc_found = .FALSE.
         GOTO 300
 200  ENDDO
 300  DO WHILE ( .TRUE. )

c decode command line
* BEGIN ==================================================== BEO 93/1/8
*         CALL XGTARG(Zstring,Zparse,Zbeg,Zend,qskip,iflag,delim,*100,
*     &               *100,*600)
         CALL YGETCMD(tstring)
         qskip = ZSTring.EQ.' '
         tct = 1
         DO WHILE ( ZSTring(tct:tct).EQ.' ' .AND. tct.LT.LENACT(ZSTring)
     &              )
            tct = tct + 1
         ENDDO

         ZBEg = tct
         ZENd = LENACT(tstring)
C         print *,'Tstring is ',tstring
         ZPArse = ZENd

* END ====================================================== BEO 93/1/8

c if the line is blank

         IF ( qskip ) GOTO 100

c first convert command and quals to upc and remove any qualifiers

         CALL UPC(ZSTring(ZBEg:ZENd))
         in = INDEX(ZSTring(ZBEg:ZENd),'/')
         IF ( in.NE.0 .AND. in.GT.ZBEg ) ZENd = in - 1

c   check to see if it is a keyword
C         keyfound = .FALSE.
C         CALL LSTKW1(Zstring(Zbeg:Zend),keyfound)
c go around again if a keyword was entered
C         IF ( keyfound ) GOTO 100

c check if its a user defined command

         command_in = ZSTring(ZBEg:ZENd)
         IF ( .NOT.udc_found ) THEN
            CALL UDC1(user_def_command,command_in,1,file_system,
     &                file_user,new,page)

c if not found check the system defined commands

            IF ( command_in(119:120).EQ.'*%' )
     &           CALL UDC1(user_def_command,command_in,11,file_system,
     &           file_user,new,page)

c user defined command found

            IF ( command_in(119:120).NE.'*%' ) THEN
               udc_found = .TRUE.
               ZPArse = 0

c add rest of string to user defined command

               ZSTring = user_def_command(:LENACT(user_def_command))
     &                   //ZSTring(ZENd+1:)
               CALL XPIPARSTR(ZSTring)

c check if there is a special parser symbol

               IF ( ZSTring(1:1).EQ.'@' .OR. ZSTring(1:1).EQ.'%' ) THEN
                  CALL LDBUF(ZSTring,1,ierr)
                  IF ( ierr.NE.0 ) THEN
                     CALL XWRITE(' Error in ldbuf',5)
                     GOTO 100
                  ENDIF
               ENDIF
               GOTO 400
            ENDIF
         ENDIF
c check to see if normal command in command list

         status = 0
         IF ( from_recall ) THEN
            CALL YGETCMD(Command)
            ZCOmmand = Command
         ELSE
C         print *,zbeg,zend,zstring
            CALL GMATCH(ZCOm_name,ZCOm_no,ZSTring(ZBEg:ZENd),jqual,
     &                  status)
C         print *,status

            IF ( status.NE.0 .AND. ZSTring(ZBEg:ZENd).EQ.'?' ) THEN
               ZCOmmand = '?'
               Command = ZCOmmand
            ENDIF
         ENDIF
         IF ( status.EQ.0 ) THEN
            IF ( from_recall ) THEN
               from_recall = .FALSE.
            ELSE

c replace command string by full command

               ZCOmmand = NOSTAR(ZCOm_name(jqual))
               Command = ZCOmmand
            ENDIF
c
         ELSEIF ( status.EQ.1 ) THEN

c   check to see if it is a keyword
            keyfound = .FALSE.
            CALL LSTKW1(ZSTring(ZBEg:ZENd),keyfound)
c go around again if a keyword was entered
            IF ( .NOT.(keyfound) ) THEN
c command not found, go around again unless it is eof

               IF ( ZSTring(ZBEg:ZENd).EQ.'EOF' ) THEN
                  Command = 'EXIT'
                  Idone = -1
                  RETURN
               ENDIF
               WRITE (ZWRite,99001)
               CALL XWRITE(ZWRite,5)
            ENDIF
            GOTO 100
         ELSE
* Some other error
            GOTO 100
         ENDIF

c command found, check the standard commands

* save the updated command
         CALL YCAPCMD(Command)

* Now check the parameters
         IF ( .NOT.YSTCMD() ) GOTO 100

c ****************************************************************** recall
c dont save the recall command itself!


         IF ( Command.EQ.'RECALL' ) THEN

            Command_no = Command_no - 1
            CALL RECLUI1(n_max,number,flag,status)
            IF ( status.EQ.0 ) THEN
               CALL RECALL1(n_max,Command_no,number,flag,file_recall,
     &                      ierr)
               IF ( flag.EQ.3 .AND. ierr.EQ.0 ) THEN
                  previous_command = ZSTring
                  from_recall = .TRUE.
                  GOTO 100
               ENDIF
            ENDIF
            Idone = 1
            GOTO 500
         ELSE
c if history is on then save the command except if it is the history command
c itself (which might get confusing)
            IF ( dohist .AND. Command.NE.'HISTORY' ) THEN
               CALL RECALL1(20,Command_no,1,2,file_recall,ierr)
            ENDIF
         ENDIF

c *************************************************************** reexecute

         IF ( Command.EQ.'REEXECUTE' ) THEN

            CALL XCMGET(ZSTring,ZPArse,savstrg,SAVCOM,nccom,nacom,ierr)
            IF ( nacom.GT.0 ) GOTO 100
            Idone = 1
            GOTO 500
         ELSE
            CALL XCMSAV(ZSTring,ZPArse,savstrg,SAVCOM,nccom)
         ENDIF

c ****************************************************************** chatter

         IF ( Command.EQ.'CHATTER' ) THEN

            CALL SETCHT(ZSTring,ZPArse)
            Idone = 1

c ****************************************************************** history

         ELSEIF ( Command.EQ.'HISTORY' ) THEN

            dohist = .NOT.dohist
            IF ( dohist ) THEN
               CALL XWRITE('History turned on', 5)
            ELSE
               CALL XWRITE('History turned off', 5)
            ENDIF
            Idone = 1

c ******************************************************************* log

         ELSEIF ( Command.EQ.'LOG' ) THEN

C            CALL LOCASE(Program)
            log_file = Program(:LENACT(Program))//'.log'
            seshead = Program(1:LENACT(Program))
     &                //' '//Version(1:LENACT(Version))//' '
            CALL GETTIM(seshead(LENACT(seshead)+2:))
            seshead = seshead
            CALL GETDAT(seshead(LENACT(seshead)+2:))
            CALL SETLOG(ZSTring,ZPArse,log_file,seshead)
            log_file = '+'//log_file(1:len(log_file)-1)
            CALL XPISETENV('FLOGFILE',log_file(:LENACT(log_file)))
            Idone = 1

c ******************************************************************* script

         ELSEIF ( Command.EQ.'SCRIPT' ) THEN

C            CALL LOCASE(Program)
C            script_file = Program(:LENACT(Program)) // '.xco'
            script_file = Program(:LENACT(Program))
            seshead = Program//' '//Version//' '
            CALL GETTIM(seshead(LENACT(seshead)+2:))
            seshead = seshead
            CALL GETDAT(seshead(LENACT(seshead)+2:))
            script_file = Program
            CALL SETSCR1(ZSTring,ZPArse,script_file,'.xco',seshead)
            Idone = 1

c ***************************************************************** buffer

         ELSEIF ( Command.EQ.'BUFFER' ) THEN

            CALL SETRCA(ZSTring,ierr)
            Idone = 1

c ***************************************************************** prompt

         ELSEIF ( Command.EQ.'PROMPT' ) THEN

            IF ( prompt_on ) THEN
               prompt_on = .FALSE.
            ELSE
               prompt_on = .TRUE.
            ENDIF
            Idone = 1

c ******************************************************************* ?

         ELSEIF ( Command.EQ.'?' .OR. Command.EQ.'??' ) THEN

            CALL LSTCMD1()
            Idone = 1

c ****************************************************************** keywords

         ELSEIF ( Command.EQ.'KEYWORDS' ) THEN
            CALL LSTCMD1()
            Idone = 1

* BEGIN ======================================================= BEO 92/1/8


c ***************************************************************** debug
         ELSEIF ( Command.EQ.'DEBUG' ) THEN
            IF ( DEBug ) THEN
               CALL YDEBUG(.FALSE.)
               CALL XPISETENV('XPIDEBUG','0')
            ELSE
               CALL YDEBUG(.TRUE.)
               CALL XPISETENV('XPIDEBUG','1')
            ENDIF
            Idone = 1

c ***************************************************************** dumppar
         ELSEIF ( Command.EQ.'DUMPPAR' ) THEN
            CALL DUMPPAR()
            Idone = 1

c ***************************************************************** dumpcmd
         ELSEIF ( Command.EQ.'DUMPCMD' ) THEN
            CALL DUMPCMD()
            Idone = 1

c ***************************************************************** dumpkey
         ELSEIF ( Command.EQ.'DUMPKEY' ) THEN
            CALL DUMPKEY()
            Idone = 1

c ***************************************************************** dumpkwd
         ELSEIF ( Command.EQ.'DUMPKWD' ) THEN
            CALL DUMPKW()
            Idone = 1

c ***************************************************************** lparm
         ELSEIF ( Command.EQ.'LPARM' ) THEN
            CALL LPARM
            Idone = 1

c ***************************************************************** eof
         ELSEIF ( Command.EQ.'EOF' ) THEN
            Command = 'EXIT'
            Idone = -1

* END ========================================================= BEO 92/1/8

c ***************************************************************** alias

         ELSEIF ( Command.EQ.'ALIAS' .OR. Command.EQ.'UDC' ) THEN

            CALL ALIAS1(previous_command,file_system,file_user,
     &                  file_type,ui_no,ierr)
            Idone = 1

* BEGIN ======================================================= RSZ 95/5/17
c ***************************************************************** help
         ELSEIF ( Command.EQ.'HELP' ) THEN

            helptopic = ' '
            CALL UCLGST('helptopic',helptopic,ierr)
            CALL f_ihf(file_help,helptopic)
            Idone = 1

* END ========================================================= RSZ 95/5/17
c end

         ENDIF
         GOTO 500
 400  ENDDO

 500  String = ZSTring
      Parse = ZPArse

      RETURN

 600  Idone = -1

      RETURN
99001 FORMAT (' Command not found; type ? for a command listing')

      entry gtcom2_nolog
      dolog = .false.
      return
      entry gtcom2_dolog
      dolog = .true.
      return

      entry gtcom2_nohist
      dohist = .false.
      return
      entry gtcom2_dohist
      dohist = .true.
      return

      END
