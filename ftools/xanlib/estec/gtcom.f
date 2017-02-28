**==GTCOM.spg  processed by SPAG 3.09I  at 15:05 on 20 Nov 1992
**..GTCOM.FOR
      SUBROUTINE GTCOM(String,Parse,Prompt_in,Command,Command_no,Sysdir,
     &                 Program,Version,Idone)
c
      INCLUDE 'estec.inc'
      INCLUDE 'commands.inc'
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
      character(80) file_system , file_user , file_type , file_recall
      character(80) rootname , file_command , file_commandy
      INTEGER*4 Command_no , Idone , n_max , Parse
      LOGICAL from_recall , udc_found
      character(20) form
      character(80) prompt
      character(256) user_def_command , command_in
      character(256) tstring , previous_command , NOSTAR
      INTEGER*2 start(10) , npipe , npip , ico
      INTEGER*4 delim , in , jqual , status , iflag , ierr
      INTEGER*4 LENACT , lprompt , ica
      INTEGER*4 flag , number
      LOGICAL*4 qskip , first_time , keyfound
      LOGICAL*4 prompt_on
c
      character(80) seshead
      character(80) log_file , script_file
      INTEGER*4 ui_no
      LOGICAL new , page , quoted
c
 
      INTEGER*4 SAVCOM , nccom , nacom
      PARAMETER (SAVCOM=20)
      character(255) savstrg(SAVCOM)
c
      SAVE npip , npipe , tstring , start , previous_command
      SAVE file_system , file_user , file_recall
      SAVE nccom , nacom , savstrg
      SAVE from_recall , first_time , prompt_on
 
      DATA first_time/.TRUE./
      DATA npip , npipe , nccom , nacom/4*0/
 
      new = .FALSE.
c
c pass in parse string
c
      Zstring = String
      Zparse = Parse
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
         Zsysdir = Sysdir
         CALL LOCASE(Zsysdir)
         CALL GETDIR(Zcurrent_directory)
         CALL LOCASE(Zcurrent_directory)
c
c get dir to put user generated system files
c
         CALL GETUDR(rootname)
c
c load the commands
c
         file_command = Sysdir(:LENACT(Sysdir))
     &                  // Program(:LENACT(Program)) // '.cmd'
         CALL LOCASE(file_command)
         CALL LDCMDS(file_command,ierr)
         IF ( ierr.NE.0 ) CALL LEAVE
 
 
         ierr = 0
         file_commandy = Sysdir(:LENACT(Sysdir))
     &                   // Program(:LENACT(Program)) // '.kw'
         CALL LOCASE(file_commandy)
         CALL TBLDKW(file_commandy,ierr)
 
         ierr = 0
         file_commandy = Sysdir(:LENACT(Sysdir))
     &                   // Program(:LENACT(Program)) // '.cmd1'
*         CALL TBLDCM(file_commandy,ierr)
         ierr = 0
 
 
c
c udc files
c
         file_user = rootname(:LENACT(rootname))
     &               // Program(:LENACT(Program)) // '.udc'
         CALL LOCASE(file_user)
         file_system = Sysdir(:LENACT(Sysdir))
     &                 // Program(:LENACT(Program)) // '.udc'
         CALL LOCASE(file_system)
c
c command recall
c
         file_recall = rootname(:LENACT(rootname))
     &                 // Program(:LENACT(Program)) // '.hty'
         CALL LOCASE(file_recall)
c
      ELSE
c
         previous_command = Zstring
      ENDIF
      Command_no = Command_no + 1
      Idone = 0
c
c is it a user defined prompt, or the default?
c
      IF ( Prompt_in.EQ.' ' .OR. prompt_on ) THEN
         prompt = ' '
         prompt(1:1) = '['
         ica = ALOG10(REAL(Command_no)) + 1
         CALL CRTFMT(ica,form,1)
         WRITE (prompt(2:1+ica),form) Command_no
         prompt = prompt(:LENACT(prompt)) // ']' // 
     &            Program(:LENACT(Program)) // '>'
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
            Zparse = 0
c
c command from reexecute
c
            IF ( nacom.GE.1 ) THEN
               CALL XCMGET(tstring,Zparse,savstrg,SAVCOM,nccom,nacom,
     &                     ierr)
               IF ( ierr.NE.0 ) THEN
                  CALL XNXTRD(prompt(1:lprompt),tstring,Zparse,ierr,
     &                        *600)
                  IF ( ierr.NE.0 ) GOTO 200
               ELSE
                  Zwrite = ' ' // prompt(:LENACT(prompt)) // ' ' //
     &                     tstring(:LENACT(tstring))
                  CALL XWRITE(Zwrite,5)
               ENDIF
c
c command from history
c
            ELSEIF ( from_recall ) THEN
               Zwrite = ' ' // prompt(:LENACT(prompt)) // ' ' //
     &                  Zstring(:LENACT(Zstring))
               tstring = Zstring(:LENACT(Zstring))
               CALL XWRITE(Zwrite,5)
            ELSE
               CALL XNXTRD(prompt(1:lprompt),tstring,Zparse,ierr,*600)
               IF ( ierr.NE.0 ) GOTO 200
               nacom = 0
            ENDIF
            from_recall = .FALSE.
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
               IF ( tstring(ico:ico).EQ.'"' ) THEN
                  quoted = .NOT.quoted
               ENDIF
               IF ( tstring(ico:ico).EQ.';' .AND. .NOT.quoted ) THEN
                  npipe = npipe + 1
                  start(npipe) = ico
               ENDIF
 140        CONTINUE
         ELSE
            npip = npip + 1
            Zparse = 0
         ENDIF
         npipe = npipe - 1
         Zstring = tstring(start(npip)+1:start(npip+1)-1)
 
         udc_found = .FALSE.
         GOTO 300
 200  ENDDO
 300  DO WHILE ( .TRUE. )
 
c decode command line
 
         CALL XGTARG(Zstring,Zparse,Zbeg,Zend,qskip,iflag,delim,*100,
     &               *100,*600)
 
c if the line is blank
 
         IF ( qskip ) GOTO 100
 
c first convert command and quals to upc and remove any qualifiers
 
         CALL UPC(Zstring(Zbeg:Zend))
         in = INDEX(Zstring(Zbeg:Zend),'/')
         IF ( in.NE.0 .AND. in.GT.Zbeg ) Zend = in - 1
 
c   check to see if it is a keyword
C         keyfound = .FALSE.
C         CALL LSTKW(Zstring(Zbeg:Zend),keyfound)
c go around again if a keyword was entered
C         IF ( keyfound ) GOTO 100
 
c check if its a user defined command
 
            command_in = Zstring(Zbeg:Zend)
            IF ( .NOT.udc_found ) THEN
               CALL UDC(user_def_command,command_in,1,file_system,
     &                  file_user,new,page)
 
c if not found check the system defined commands
 
               IF ( command_in(119:120).EQ.'*%' ) THEN
                  CALL UDC(user_def_command,command_in,11,file_system,
     &                     file_user,new,page)
               ENDIF
 
c user defined command found
 
               IF ( command_in(119:120).NE.'*%' ) THEN
                  udc_found = .TRUE.
                  Zparse = 0
 
c add rest of string to user defined command
 
                  Zstring = user_def_command(:LENACT(user_def_command))
     &                      // Zstring(Zend+1:)
 
c check if there is a special parser symbol
 
                  IF ( Zstring(1:1).EQ.'@' .OR. Zstring(1:1).EQ.'%' )
     &                 THEN
                     CALL LDBUF(Zstring,1,ierr)
                     IF ( ierr.NE.0 ) THEN
                        CALL XWRITE(' Error in ldbuf',5)
                        GOTO 100
                     ENDIF
                  ENDIF
                 go to 400 
              ENDIF
           ENDIF
c check to see if normal command in command list
 
         status = 0
         CALL GMATCH(Zcom_name,Zcom_no,Zstring(Zbeg:Zend),jqual,status)
         IF ( status.EQ.0 ) THEN
 
c replace command string by full command
 
            Zcommand = NOSTAR(Zcom_name(jqual))
            Command = Zcommand
c
         ELSEIF ( status.EQ.1 ) THEN
 
c   check to see if it is a keyword
            keyfound = .FALSE.
            CALL LSTKW(Zstring(Zbeg:Zend),keyfound)
c go around again if a keyword was entered
            IF ( .NOT.(keyfound) ) THEN
c command not found, go around again
 
               WRITE (Zwrite,99001)
               CALL XWRITE(Zwrite,5)
            ENDIF
            GOTO 100
         ELSE
            GOTO 100
         ENDIF
 
c command found, check the standard commands
 
c ****************************************************************** recall
c dont save the recall command itself!
 
         IF ( Command.EQ.'RECALL' ) THEN
 
            Command_no = Command_no - 1
            CALL RECLUI(n_max,number,flag,status)
            IF ( status.EQ.0 ) THEN
               CALL RECALL(n_max,Command_no,number,flag,file_recall,
     &                     ierr)
               IF ( flag.EQ.3 .AND. ierr.EQ.0 ) THEN
                  previous_command = Zstring
                  from_recall = .TRUE.
                  GOTO 100
               ENDIF
            ENDIF
            Idone = 1
            GOTO 500
         ELSE
            CALL RECALL(20,Command_no,1,2,file_recall,ierr)
         ENDIF
 
c *************************************************************** reexecute
 
         IF ( Command.EQ.'REEXECUTE' ) THEN
 
            CALL XCMGET(Zstring,Zparse,savstrg,SAVCOM,nccom,nacom,ierr)
            IF ( nacom.GT.0 ) GOTO 100
            Idone = 1
            GOTO 500
         ELSE
            CALL XCMSAV(Zstring,Zparse,savstrg,SAVCOM,nccom)
         ENDIF
 
c ****************************************************************** chatter
 
         IF ( Command.EQ.'CHATTER' ) THEN
 
            CALL SETCHT(Zstring,Zparse)
            Idone = 1
 
c ******************************************************************* log
 
         ELSEIF ( Command.EQ.'LOG' ) THEN
 
            CALL LOCASE(Program)
            log_file = Program(:LENACT(Program)) // '.log'
            seshead = Program // ' ' // Version // ' '
            CALL GETTIM(seshead(LENACT(seshead)+2:))
            seshead = seshead // ' '
            CALL GETDAT(seshead(LENACT(seshead)+2:))
            CALL SETLOG(Zstring,Zparse,log_file,seshead)
            Idone = 1
 
c ******************************************************************* script
 
         ELSEIF ( Command.EQ.'SCRIPT' ) THEN
 
            CALL LOCASE(Program)
            script_file = Program(:LENACT(Program)) // '.xco'
            seshead = Program // ' ' // Version // ' '
            CALL GETTIM(seshead(LENACT(seshead)+2:))
            seshead = seshead // ' '
            CALL GETDAT(seshead(LENACT(seshead)+2:))
            script_file = Program
            CALL SETSCR(Zstring,Zparse,script_file,'.log',seshead)
            Idone = 1
 
c ***************************************************************** buffer
 
         ELSEIF ( Command.EQ.'BUFFER' ) THEN
 
            CALL SETRCA(Zstring,ierr)
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
 
         ELSEIF ( Command.EQ.'?' ) THEN
 
            CALL LSTCMD()
            Idone = 1
 
c ****************************************************************** keywords
 
         ELSEIF ( Command.EQ.'KEYWORDS' ) THEN
            CALL LSTCMD()
            Idone = 1
 
 
 
c ***************************************************************** alias
 
         ELSEIF ( Command.EQ.'ALIAS' .OR. Command.EQ.'UDC' ) THEN
 
            CALL ALIAS(previous_command,file_system,file_user,file_type,
     &                 ui_no,ierr)
            Idone = 1
 
c end
 
         ENDIF
         GOTO 500
 400  ENDDO
 
 500  String = Zstring
      Parse = Zparse
 
      RETURN
 
 600  Idone = -1
 
      RETURN
99001 FORMAT (' Command not found; type ? for a command listing')
      END

