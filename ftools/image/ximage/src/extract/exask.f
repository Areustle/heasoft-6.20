c
      SUBROUTINE EXASK(Cpro,Cque,Lur,Lut,Lcd,Lul,Luw,Cinput,Cv,Iv,Rv,Dv,
     &                 Ity,No,*,*)
      implicit none
c
c ls 25/2/88 general purpose routine for reading/writing a parameter
c            in an interactive program
c ls 6/4/88  upgraded to decode more than 1 param. in input string
c ls 6/7/88  no=-1 option added (rev.2)
c ls 27/9/88 to cure problem in integers followed by 1 space (rev.3)
c ls 8/2/89  temporary variables ivt,rvt,dvt introduced to avoid reset of in
c            default case of error in input string (rev.4)
c ls 16/12/89 remove comments following "!" from string and cure problem with
c             1 chars first replies starting with blanks (Rev.5)
c ls 16/12/89 to allow reading again from command file after an error in
c             command file and a correct reply from terminal (rev.6)
c
c      I   cpro = promtpt issued
c      I   cque = question asked
c      I   lur,lut,lul = logical units of read, terminal,
c                        logfile (write to lul only if >0)
c      I   lcd = cd_xronos chattiness (use 10 for other uses)
c      I   luw = logical unit to write cmd file (if luw.ne.0 and EOF
c                is read from input string luw is closed)
c      R   cinput = string containing all replies
c      R   cv,iv,rv,dv = char,int*4,real*4,real*8 parameter to be read
c      I   ity =           1 ,  2  ,   3  ,   4   parameter type
c      I   no  = No. of parameter in string
c                (if =1 question is asked, if=-1 question is not asked)
c
c      R   * =  Conditional return 1 is to ask question again when there is
c               an input error and no>1 (to go to statement where xrask
c               is called with no=1).
c      R   * =  Conditional return 2 is to stop asking for a given param.
c               when entering a character '-' (in other cases it can be
c               considered a return for defualt)
c
c   (xrhelps(/help)),xgthlp, xrbell = subroutines used
c
c   Input String: 132 chars. at max., if no chars are read the relevant
c                 variable is unchanged (to have defaults). If more than
c                 one param. is contained in the string the subroutine should
c                 be called once for each parameter by setting no=1,2,3, etc
c                 for the first, second, third param. etc. The question is
c                 asked only if no=1. Parameters should be asked in the
c                 same order of the input string. No params can be skipped
c                 (no=-1 is to decode first varaible without asking question)
c
c   EOF in input string: if from terminal closes all units and stops
c                        if from command files transfer control to term.
c                        (EOF is cntlD in unix and cntlZ in VMS)
c
c   Question + Prompt String: contains 39 chars at max; if this is not
c                             enough use the xraskl routine which writes
c                             a longer question.
c
c
c   This version is for vms. Changes for unix are indicated (c!)
c
      CHARACTER Cpro*(*) , Cque*(*) , Cv*(*) , Cinput*(*) , cform*12 , 
     &          ctemp*132 , cdum*132
      INTEGER*4 Lur , Lut , Lcd , Lul , Luw , Iv , Ity , ierr , nch , 
     &          No , ia , io , ivt , LENACT
      REAL*4 Rv , rvt
      REAL*8 Dv , dvt
      SAVE 
c      save ia,io,nch
 
c
c     if(lut.ne.0)write(lut,*)' ity, no, nch, ia, io ',ity,no,nch,ia,io
c
c Rev.2 start
c
      IF ( No.EQ.-1 ) THEN
         io = 0
c Rev.5
         CALL EXREMCOM(Cinput,132,'!')
         nch = LENACT(Cinput)
         CALL EXMOVLEFT(Cinput,nch)
      ENDIF
c Rev.2 stop
c
c
      IF ( No.NE.1 ) GOTO 200
 100  io = 0
c  write question *
      IF ( Lcd.GE.2 .OR. Lur.LT.0 )
     &     WRITE (*,'('' '',a,a,t41,''=> '',$)') Cpro , Cque
      IF ( Lul.NE.0 ) WRITE (Lul,'('' '',a,a,t41,''=> '',$)') Cpro , 
     &                       Cque
c  read answer and get no. of chars. (nch)
c Rev.6
c      if(lur.eq.lut)
c Rev.6
c Rev. 6
      IF ( Lur.EQ.Lut .OR. Lur.LT.0 ) THEN
         READ (*,'(a)',IOSTAT=ierr,END=500) Cinput
         nch = LENACT(Cinput)
         CALL EXMOVLEFT(Cinput,nch)
      ENDIF
c
c Rev. 6
      IF ( Lur.NE.Lut .AND. Lur.GT.0 ) THEN
         READ (Lur,'('' '',a)',IOSTAT=ierr,END=500) Cinput
         nch = LENACT(Cinput)
         CALL EXMOVLEFT(Cinput,nch)
      ENDIF
c
c  echo to terminal if read is not from terminal
c
c Rev.6
c why is there the condition nch=0 ? unix work fine for nch=0
c      IF (lur.NE.lut .AND. lur.GT.0 .AND. lcd.GE.2 .AND. nch.GT.0)
C      IF (lur.NE.lut .AND. lur.GT.0 .AND. lcd.GE.2 )
C     &     CALL xrwrplust (lut,cinput)
c
c Rev.6 start  reset lcd to original value
      IF ( Lur.LT.0 ) THEN
         Lur = IABS(Lur)
         Lcd = Lcd - 100
      ENDIF
c Rev.6 stop
*      IF (lul.NE.0) CALL xrwrplusl (lul,cinput)
c      IF (lul.NE.0 .AND. nch.GT.0) CALL exwrplusl (lul,cinput)  !!!!!
c       write(*,'(i10,10x,a)')nch,cinput
c Rev.5
      CALL EXREMCOM(Cinput,132,'!')
c  condition for default
 200  IF ( nch.EQ.0 ) RETURN
cc  accept also / for default for compatibility with FTN 77.
c      if(cinput.eq.'/') return
c  if all blanks return (and take default)
      IF ( Cinput(1:).EQ.' ' ) RETURN
c  condition for conditional return 2 (to stop asking for a given parameter)
      IF ( Cinput.EQ.'-' ) RETURN 2
c  condition for entering help
      IF ( Cinput(1:4).EQ.'help' .OR. Cinput(1:4).EQ.'HELP' ) THEN
         cdum = Cinput(6:)
c        call xrhelps (lut)
         GOTO 100
      ENDIF
c  determine indexes ia (start) and io (stop) to read current param.
c   condition for default of remaining params.
      IF ( io.EQ.nch ) RETURN
      IF ( Cinput(io+1:).EQ.' ' ) RETURN
c   otherwise determine new ia and io
      ia = io + 1
      DO WHILE ( .TRUE. )
         io = io + 1
c      if(io.eq.nch) go to 5
c Rev.3
c      if(io.eq.nch.and.cinput(io:io).ne.' ') go to 5
c Rev.5 start
         IF ( io.EQ.nch .AND. Cinput(io:io).NE.' ' ) THEN
            IF ( io.GT.1 .AND. Cinput(io-1:io-1).EQ.' ' ) ia = io
            GOTO 400
         ENDIF
c Rev.5 stop
c Rev.3
         IF ( io.EQ.nch .AND. Cinput(io:io).EQ.' ' ) GOTO 300
         IF ( io.NE.1 .AND. io.NE.ia ) THEN
            IF ( Cinput(io:io).NE.' ' .AND. Cinput(io-1:io-1).EQ.' ' )
     &           ia = io
            IF ( .NOT.(Cinput(io:io).NE.' ' .OR. (Cinput(io:io).EQ.' '
     &           .AND. Cinput(io-1:io-1).EQ.' ')) ) GOTO 300
         ENDIF
      ENDDO
 300  io = io - 1
c      write(*,*)' ia, io ',ia,io           !!!!!!
c  decode input string according to variable type
 400  IF ( Ity.EQ.2 ) THEN
         WRITE (cform,99001) ia , io - ia + 1
c Rev.4
         READ (Cinput,cform,IOSTAT=ierr,ERR=600) ivt
c Rev.4
         Iv = ivt
         RETURN
      ELSEIF ( Ity.EQ.3 ) THEN
         WRITE (cform,99002) ia , io - ia + 1
c Rev.4
         READ (Cinput,cform,IOSTAT=ierr,ERR=600) rvt
c Rev.4
         Rv = rvt
         RETURN
      ELSEIF ( Ity.EQ.4 ) THEN
         WRITE (cform,99003) ia , io - ia + 1
c Rev.4
         READ (Cinput,cform,IOSTAT=ierr,ERR=600) dvt
c Rev.4
         Dv = dvt
         RETURN
      ELSE
         ctemp = ' '
         ctemp(ia:io) = Cinput(ia:io)
         WRITE (cform,99004) ia
         READ (ctemp,cform,IOSTAT=ierr,ERR=600) Cv
         RETURN
      ENDIF
c  EOF in input string
c Rev.6
c901   continue
c Rev.6
 500  Lur = IABS(Lur)
c Rev.6 increase lcd to have full messages
      Lcd = Lcd + 100
c      if(lut.ne.0)
      WRITE (*,'(/,'' xrask> EOF in input string'')')
      IF ( Lul.NE.0 ) WRITE (Lul,'(/,'' xrask> EOF in input string'')')
c  if EOF from terminal close all write units
      IF ( Lur.EQ.Lut ) THEN
         IF ( Luw.NE.0 ) THEN
            CLOSE (Luw)
c          if (lut.ne.0)
            WRITE (*,'('' >>>> Incomplete command file created'')')
            IF ( Lul.NE.0 ) WRITE (Lul,
     &                     '('' >>>> Incomplete command file created'')'
     &                     )
         ENDIF
         IF ( Lul.NE.0 ) CLOSE (Lul)
c        call xrbell (lut)
         STOP
c Rev.6
      ELSE
c Rev.6
         Lur = Lut
      ENDIF
      GOTO 700
c  other errors
c900   if (lut.ne.0)
 600  WRITE (*,'('' xrask> Error'',i5,'' in input string'')') ierr
      IF ( Lul.NE.0 ) WRITE (Lul,
     &                     '('' xrask> Error'',i5,'' in input string'')'
     &                     ) ierr
c  if error from cmd file transfer control to terminal
 700  IF ( Lur.NE.Lut ) THEN
c Rev.6
c        close(lur)
c Rev.6
c        lur=lut
         IF ( Lur.GT.0 ) THEN
c Rev.6
            Lur = -Lur
c Rev.6 increase lcd to have full messages
            Lcd = Lcd + 100
         ENDIF
c        if(lut.ne.0)
         WRITE (*,'('' xrask> Control transferred to terminal'')')
         IF ( Lul.NE.0 ) WRITE (Lul,
     &                   '('' xrask> Control transferred to terminal'')'
     &                   )
      ENDIF
c      call xrbell (lut)
      IF ( No.NE.1 ) RETURN 1
      GOTO 100
99001 FORMAT ('(t',I2.2,',i',I2.2,')   ')
99002 FORMAT ('(t',I2.2,',f',I2.2,'.0) ')
99003 FORMAT ('(t',I2.2,',f',I2.2,'.0) ')
99004 FORMAT ('(t',I2.2,',a)     ')
      END
