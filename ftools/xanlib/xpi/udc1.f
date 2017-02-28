**==udc1.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
      SUBROUTINE UDC1(Command,Alias_name,Nflag,File_system,File_user,
     &                New,Page)
c
c command - complete input or output command string (256 long)
c alias_name - alias name for command
c  subroutine to load, retrieve and delete user-defined commands
c  nflag = 0 -> to load u-d command
c  nflag = 1 -> to retrieve u-d command
c  nflag = 2 -> to list all user-defined commands
c  nflag = 3 -> delete u-d command
c  nflag = 10 -> to load system-defined command
c  nflag = 11 -> to retrieve s-d command
c  nflag = 12 -> to list all system-defined commands
c  nflag = 13 -> delete system-defined command
c  file_system, user - file names where commands stored
c  new - erase exsiting file
c  page put in page breaks for listing
c routine by paolo
c commands read into memory added - by Nick 30.5.91
c plus many other things added - Nick 3.6.91
c made cfile 255 chars; try to ensure internal writes don't overflow (many
c could be simple assignments!) - Jeff Guerber RSTX/GSFC 1998-12-31
c
      INCLUDE 'estec.inc'
      INTEGER*4 N_MAX
      PARAMETER (N_MAX=99)
c
c n_max is the maximum number of commands
c
      CHARACTER*(*) Command , File_system , File_user
      character(376) blank , string , udcudc , command_string
      character(376) save(N_MAX*2) , save_string
      character(80) prompt
      CHARACTER*(*) Alias_name
      character(255) cfile
      character(60) ans
      character(2) com_type(N_MAX*2)
      LOGICAL found , there , open , never_found , read_sys , read_user
      LOGICAL New , Page
      LOGICAL*4 finish , qeof , qans , sys
      INTEGER ios , lun , flag , i , n , in , ic , ncom , isl
      INTEGER isb , LENACT , Nflag , in1 , ii
      INTEGER*4 ierr , wild_1 , wild_2 , len_1 , in2
      INTEGER*4 nbeg , nend , nrecl , nsave , nfound
      DATA nsave/0/ , read_sys/.FALSE./ , read_user/.FALSE./
      ios = 0
      never_found = .TRUE.
      len_1 = LENACT(Alias_name)
c
C      CALL UPC(Command)
      CALL UPC(Alias_name)
c
      nbeg = 119
      nend = 120
      nrecl = 376
c
      IF ( len_1.NE.0 .AND. Alias_name(len_1:len_1).EQ.'*' )
     &     Alias_name(len_1:len_1) = ' '
      wild_1 = INDEX(Alias_name,'*')
      wild_2 = INDEX(Alias_name(wild_1+1:),'*')
      IF ( wild_2.NE.0 ) THEN
         WRITE (ZWRite,*) ' Second wildcard allowed only at the end '
         CALL XWRITE(ZWRite,5)
         RETURN
      ENDIF
      Alias_name(nbeg:nend) = '  '
      IF ( Nflag.GE.10 ) THEN
         flag = Nflag - 10
         cfile = File_system
         sys = .TRUE.
      ELSE
         sys = .FALSE.
         flag = Nflag
         cfile = File_user
      ENDIF
c
c first check the file exists
c
      INQUIRE (FILE=cfile,EXIST=there,OPENED=open,NUMBER=lun)
      IF ( .NOT.there .AND. flag.EQ.1 ) THEN
         Alias_name(nbeg:nend) = '*%'
         RETURN
      ELSEIF ( .NOT.there .AND. flag.EQ.2 ) THEN
         IF ( sys ) THEN
            WRITE (ZWRite,'('' No system aliases created'')')
            CALL XWRITE(ZWRite,5)
         ELSE
            WRITE (ZWRite,'('' No user aliases created'')')
            CALL XWRITE(ZWRite,5)
         ENDIF
         RETURN
      ENDIF
c
c if the file is there and its not been read in then
c
      IF ( there ) THEN
c
c open file and read in aliases, if this is the first time
c
         IF ( (read_sys .AND. sys) ) THEN
            open = .TRUE.
         ELSEIF ( (read_user .AND. .NOT.sys) ) THEN
            open = .TRUE.
         ENDIF
c
         IF ( .NOT.open ) THEN
            CALL GETLUN(lun)
c            OPEN (lun,FILE=cfile,STATUS='old',ACCESS='direct',readonly,
c     &            RECL=nrecl,FORM='formatted',MAXREC=51,iostat=ierr)
c
c
            CALL OPENWR(lun,cfile,'old','df',' ',nrecl,1,ierr)
c
            IF ( ierr.NE.0 ) THEN
               WRITE (ZWRite,99001) ierr , cfile
               CALL XWRITE(ZWRite,5)
               CALL FRELUN(lun)
               Alias_name(nbeg:nend) = '*%'
               RETURN
            ENDIF
c
c read the commands into memory to speed things up
c
            READ (lun,'(a)',REC=1) string(:nrecl)
            CALL UNLOCK(lun)
            READ (string,'(i2)',IOSTAT=ios) ncom
            n = 1
            DO WHILE ( n.LE.(ncom+1) .AND. ncom.NE.1 .AND.
     &                 n.LE.(N_MAX+1) .AND. ios.EQ.0 )
               n = n + 1
               nsave = nsave + 1
               READ (lun,'(a)',REC=n,IOSTAT=ios) save(nsave)
               CALL UNLOCK(lun)
               CALL UPC(save(nsave))
               IF ( string.EQ.' ' .OR. string((n+9):(n+9)).EQ.'0' .OR.
     &              save(nsave).EQ.' ' ) THEN
                  nsave = nsave - 1
               ELSEIF ( Nflag.GE.10 ) THEN
                  com_type(nsave) = 'SY'
               ELSE
                  com_type(nsave) = 'US'
               ENDIF
            ENDDO
c
c remember that the file has been read, then close it
c
            IF ( sys ) THEN
               read_sys = .TRUE.
            ELSE
               read_user = .TRUE.
            ENDIF
            CLOSE (lun)
            CALL FRELUN(lun)
c
         ENDIF
c
c open the file for read access if delete or load (flag 3 and 0)
c
         IF ( (flag.EQ.3 .OR. flag.EQ.0) ) THEN
            IF ( (read_sys .AND. sys) .OR. (read_user .AND. .NOT.sys) )
     &           THEN
c
c first check the file exists
c
               INQUIRE (FILE=cfile,EXIST=there)
               IF ( .NOT.there .AND. flag.EQ.1 ) THEN
                  Alias_name(nbeg:nend) = '*%'
                  CALL XWRITE(' Error: the udc file is missing',5)
                  RETURN
               ELSEIF ( .NOT.there .AND. flag.EQ.2 ) THEN
                  CALL XWRITE(' Error: the udc file is missing',5)
                  RETURN
               ENDIF
c
c if the file is there and its not open then
c
               IF ( there ) THEN
                  CALL GETLUN(lun)
c
c            OPEN (lun,FILE=cfile,STATUS='old',ACCESS='direct',shared,
c     &            RECL=nrecl,FORM='formatted',MAXREC=51,iostat=ierr)
c
                  CALL OPENWR(lun,cfile,'old','df',' ',nrecl,0,ierr)
c
                  IF ( ierr.NE.0 ) THEN
                     WRITE (ZWRite,99001) ierr , cfile
                     CALL XWRITE(ZWRite,5)
                     CALL FRELUN(lun)
                     Alias_name(nbeg:nend) = '*%'
                     RETURN
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
c
c if new, then erase current file and save buffer
c this must be combined with a new alias creation
c
         IF ( New ) THEN
            IF ( sys ) THEN
               CALL XWRITE('Erasing current system defined commands',5)
            ELSE
               CALL XWRITE('Erasing current user defined commands',5)
            ENDIF
c
            CALL XQUEST('Are you sure?','Y',qans,qeof)
            IF ( qans .AND. .NOT.qeof ) THEN
c
               blank = ' '
               string = ' 1'//blank
               WRITE (lun,'(a)',REC=1) string(:nrecl)
               DO 10 i = 2 , N_MAX
                  WRITE (lun,'(a)',REC=i) blank(:nrecl)
 10            CONTINUE
               CALL UNLOCK(lun)
c
c remove it from save
c
               i = 1
               DO WHILE ( i.LE.nsave )
                  IF ( (com_type(i).EQ.'SY' .AND. sys) .OR.
     &                 (com_type(i).EQ.'US' .AND. .NOT.sys) ) THEN
                     IF ( i.LT.nsave ) THEN
                        DO 12 ii = i , nsave - 1
                           save(ii) = save(ii+1)
                           com_type(ii) = com_type(ii+1)
 12                     CONTINUE
                     ENDIF
                     nsave = nsave - 1
                  ELSE
                     i = i + 1
                  ENDIF
               ENDDO
            ELSE
               RETURN
            ENDIF
         ENDIF
c
c   delete command
c
         IF ( flag.EQ.3 ) THEN
            READ (lun,'(a)',REC=1) string(:nrecl)
            CALL UNLOCK(lun)
            READ (string,'(i2)') ncom
            CALL UPC(Alias_name)
            in = INDEX(Alias_name,' ') - 1
            found = .FALSE.
            n = 1
            DO WHILE ( n.LE.(ncom+1) .AND. n.LE.(N_MAX+1) .AND.
     &                 ios.EQ.0 )
               n = n + 1
               READ (lun,'(a)',REC=n,IOSTAT=ios) command_string
               CALL UNLOCK(lun)
               CALL UPC(command_string)
               IF ( command_string.NE.' ' .AND. string((n+9):(n+9))
     &              .NE.'0' ) THEN
                  CALL CMPSTR(Alias_name,command_string,found)
                  IF ( found ) THEN
                     never_found = .FALSE.
                     READ (lun,'(a)',REC=n) string(:nrecl)
                     CALL UNLOCK(lun)
                     save_string(:nrecl) = string(:nrecl)
                     qans = .FALSE.
                     qeof = .FALSE.
                     in = INDEX(string(1:20),' ') + 1
                     in1 = INDEX(string(nend+1:nrecl),' ') + 1 + nend
c make the prompt and make sure it aint too long
                     prompt = '* Deleting -> '//string(1:in)
     &                        //' == '//string(nend+1:in1)
                     in = LENACT(prompt)
                     IF ( in.GT.50 ) in = 50
                     prompt = prompt(1:in)//'; OK? '
c
                     CALL XQUEST(prompt,'Y',qans,qeof)
c99001                FORMAT (' ',A,' == ',A,'; OK? ')
                     IF ( qans .AND. .NOT.qeof ) THEN
                        string = ' '
                        WRITE (lun,'(a)',REC=n) string(:nrecl)
                        CALL UNLOCK(lun)
                        READ (lun,'(a)',REC=1) string(:nrecl)
                        CALL UNLOCK(lun)
                        string((n+9):(n+9)) = '0'
                        WRITE (lun,'(a)',REC=1) string(:nrecl)
                        CALL UNLOCK(lun)
c remove it from save
                        i = 1
                        DO WHILE ( i.LE.nsave )
                           IF ( save_string(:nrecl).EQ.save(i)(:nrecl) )
     &                          THEN
                              IF ( i.LT.nsave ) THEN
                                 DO 14 ii = i , nsave - 1
                                    save(ii) = save(ii+1)
                                    com_type(ii) = com_type(ii+1)
 14                              CONTINUE
                              ENDIF
                              i = nsave + 1
                              nsave = nsave - 1
                           ENDIF
                           i = i + 1
                        ENDDO
                     ELSE
                        ZWRite = 'Command not deleted'
                        CALL XWRITE(ZWRite,5)
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
            IF ( never_found ) THEN
               WRITE (ZWRite,'(''  no matches '')')
               CALL XWRITE(ZWRite,5)
            ENDIF
            GOTO 200
         ENDIF
c
c list commands
c
         IF ( flag.EQ.2 ) THEN
            ic = 0
            in = INDEX(Alias_name(1:nbeg-1),' ') - 1
            DO 20 i = 1 , nsave
               udcudc = save(i)
               IF ( udcudc.NE.' ' ) THEN
                  IF ( in.GT.0 ) THEN
                     CALL CMPSTR(Alias_name,udcudc,found)
                  ELSE
                     found = .TRUE.
                  ENDIF
c
                  IF ( found ) THEN
c
                     IF ( (Nflag.GE.10 .AND. com_type(i).EQ.'SY') .OR.
     &                    (Nflag.LT.10 .AND. com_type(i).EQ.'US') ) THEN
                        IF ( found .AND. never_found ) THEN
                           IF ( sys ) THEN
                              WRITE (ZWRite,
     &                        '('' ------ system aliases -----------'')'
     &                        )
                              CALL XWRITE(ZWRite,5)
                           ELSE
                              WRITE (ZWRite,
     &                               '('' --- user aliases -----'')')
                              CALL XWRITE(ZWRite,5)
                           ENDIF
                        ENDIF
                        never_found = .FALSE.
                        IF ( ic.NE.0 .AND. MOD((ic),20).EQ.0 .AND.
     &                       Page ) THEN
                           CALL PAGBRK(finish)
                           IF ( finish ) GOTO 40
                        ENDIF
                        ic = ic + 1
                        zwrite = '  ' // udcudc(1:20) // ' == '
     &                      // udcudc(nend+1:LENACT(udcudc))
                        CALL XWRITE(ZWRite,5)
                     ELSE
                        found = .FALSE.
                     ENDIF
                  ENDIF
               ENDIF
 20         CONTINUE
 40         IF ( never_found ) THEN
               IF ( sys ) THEN
                  WRITE (ZWRite,
     &                   '('' ------ system aliases -----------'')')
                  CALL XWRITE(ZWRite,5)
               ELSE
                  WRITE (ZWRite,'('' --- user aliases -----'')')
                  CALL XWRITE(ZWRite,5)
               ENDIF
               WRITE (ZWRite,'(''  no matches '')')
               CALL XWRITE(ZWRite,5)
            ENDIF
            RETURN
         ENDIF
c
c retrieve command
c
         IF ( flag.EQ.1 ) THEN
            isl = 0
            isl = INDEX(Alias_name,'/')
            isb = INDEX(Alias_name,' ')
            IF ( isl.NE.0 ) THEN
               in = isl - 1
            ELSE
               in = isb - 1
            ENDIF
            found = .FALSE.
            n = 1
            nfound = 0
            DO WHILE ( n.LE.nsave .AND. n.LE.(N_MAX*2) )
               udcudc = save(n)
               IF ( Alias_name(1:in).EQ.udcudc(1:in) ) THEN
                  nfound = nfound + 1
                  found = .TRUE.
                  IF ( isl.NE.0 ) THEN
                     command_string = udcudc(nend+1:LENACT(udcudc))
     &                                //Alias_name(isl:)
                  ELSE
                     command_string = udcudc(nend+1:LENACT(udcudc))
     &                                //Alias_name(isb:)
                  ENDIF
               ELSE
                  found = .FALSE.
               ENDIF
               n = n + 1
            ENDDO
            IF ( nfound.GT.1 ) THEN
               WRITE (ZWRite,*) 'Ambiguous alias command'
               CALL XWRITE(ZWRite,5)
            ELSEIF ( nfound.EQ.1 ) THEN
               Command = command_string
            ENDIF
            IF ( nfound.NE.1 ) Alias_name(nbeg:nend) = '*%'
c
c load command
c
         ELSEIF ( flag.EQ.0 ) THEN
c check it does not already exist
c
c first remove any qualifiers
c
            isl = 0
            isl = INDEX(Alias_name,'/')
            isb = INDEX(Alias_name,' ') - 1
            IF ( isl.NE.0 ) THEN
               in = isl - 1
            ELSE
               in = isb
            ENDIF
c
c make sure the command does not already exist IN THE SAME FILE
c by reading thru it
c
            READ (lun,'(a)',REC=1) string(:nrecl)
            CALL UNLOCK(lun)
            READ (string,'(i2)') ncom
            found = .FALSE.
            n = 1
            DO WHILE ( .NOT.found .AND. n.LE.ncom+1 .AND.
     &                 ncom.NE.1 .AND. n.LE.(N_MAX+1) .AND. ios.EQ.0 )
               n = n + 1
               READ (lun,'(a)',REC=n,IOSTAT=ios) command_string
               CALL UNLOCK(lun)
               CALL UPC(command_string)
               IF ( command_string.NE.' ' .AND. string((n+9):(n+9))
     &              .NE.'0' ) THEN
c
                  IF ( command_string(1:in).EQ.Alias_name(1:in) ) THEN
                     found = .TRUE.
                     in2 = INDEX(command_string,' ')
                     zwrite = 'Overwriting ' //  command_string(1:in2)
     &                   // ' == '
     &                   //command_string(nend+1:LENACT(command_string))
     &                   // ' OK? [d/f=y]: '
                     CALL XCREAD(ZWRite,ans,ierr)
                     CALL UPC(ans)
                     IF ( ierr.NE.0 .OR. ans(1:1).EQ.'N' ) THEN
                        WRITE (ZWRite,*) 'Command not overwritten'
                        RETURN
                     ELSE
                        n = n - 1
                     ENDIF
                  ENDIF
               ENDIF
               n = n + 1
            ENDDO
c
c if the command was not already found get the next slot
c
            IF ( .NOT.found ) THEN
               n = 0
               DO 50 i = 1 , ncom
                  IF ( string((10+i):(10+i)).EQ.'0' ) n = i + 1
 50            CONTINUE
               IF ( n.EQ.0 ) n = ncom + 1
               IF ( n.GT.N_MAX ) THEN
                  WRITE (ZWRite,*)
     &                     ' max number of aliased parameters exceeded '
                  CALL XWRITE(ZWRite,5)
                  GOTO 200
               ENDIF
            ENDIF
c
c write new command
c
            string = Alias_name(1:nend)//Command
            WRITE (lun,'(a)',REC=n) string
            CALL UNLOCK(lun)
c
c update/replace command in save, first find it
c
            IF ( .NOT.found ) THEN
               nsave = nsave + 1
               IF ( nsave.GT.N_MAX*2 ) THEN
                  WRITE (ZWRite,99004) N_MAX
                  CALL XWRITE(ZWRite,5)
                  GOTO 200
               ENDIF
               save(nsave) = string
               IF ( Nflag.GE.10 ) THEN
                  com_type(nsave) = 'SY'
               ELSE
                  com_type(nsave) = 'US'
               ENDIF
            ELSE
               n = 1
               DO WHILE ( n.LE.nsave .AND. n.LE.(N_MAX*2) )
                  udcudc = save(n)
                  IF ( Alias_name(1:in).EQ.udcudc(1:in) ) THEN
                     IF ( (sys .AND. com_type(n).EQ.'SY') .OR.
     &                    (.NOT.sys .AND. com_type(n).EQ.'US') ) THEN
                        save(n) = string(:nrecl)
                        n = nsave
                     ENDIF
                  ENDIF
                  n = n + 1
               ENDDO
            ENDIF
c
c update first record if its not an over write
c
            IF ( n.GT.ncom ) THEN
               READ (lun,'(a)',REC=1) string(:nrecl)
               WRITE (string(1:2),'(i2)') n
               WRITE (lun,'(a)',REC=1) string(:nrecl)
               CALL UNLOCK(lun)
            ELSEIF ( .NOT.found ) THEN
               READ (lun,'(a)',REC=1) string(:nrecl)
               string((9+n):(9+n)) = ' '
               WRITE (lun,'(a)',REC=1) string(:nrecl)
               CALL UNLOCK(lun)
            ENDIF
         ELSE
            WRITE (ZWRite,*) ' wrong flag in udc'
            CALL XWRITE(ZWRite,5)
         ENDIF
      ELSEIF ( flag.EQ.0 ) THEN
         ZWRite = ' Creating file ' // cfile
         CALL XWRITE(ZWRite,5)
         CALL GETLUN(lun)
c
c         OPEN (lun,FILE=cfile,STATUS='new',ACCESS='direct',shared,
c     &         RECL=nrecl,FORM='formatted',MAXREC=51)
c
         CALL OPENWR(lun,cfile,'new','df',' ',nrecl,0,ierr)
         IF ( ierr.NE.0 ) THEN
            WRITE (ZWRite,99001) ierr , cfile
            CALL XWRITE(ZWRite,5)
            GOTO 200
         ENDIF
         blank = ' '
         string = ' 1'//blank
         WRITE (lun,'(a)',REC=1) string(:nrecl)
         DO 100 i = 2 , N_MAX
            WRITE (lun,'(a)',REC=i) blank(:nrecl)
 100     CONTINUE
         CALL UNLOCK(lun)
c          attempting to read from non-existent file
         IF ( flag.EQ.1 ) THEN
c
c load command
c
         ELSEIF ( flag.EQ.0 ) THEN
            READ (lun,'(a)',REC=1) string(:nrecl)
            CALL UNLOCK(lun)
            READ (string,'(i2)') ncom
            n = 0
            DO 120 i = 1 , ncom
               IF ( string((10+i):(10+i)).EQ.'0' ) n = i + 1
 120        CONTINUE
            IF ( n.EQ.0 ) n = ncom + 1
            string = Alias_name(1:nend)//Command
            WRITE (lun,'(a)',REC=n) string(:nrecl)
            CALL UNLOCK(lun)
c
c put it into memory
c
            nsave = nsave + 1
            save(nsave) = Alias_name(1:nend)//Command
            IF ( Nflag.GE.10 ) THEN
               com_type(nsave) = 'SY'
            ELSE
               com_type(nsave) = 'US'
            ENDIF
            IF ( n.GT.ncom ) THEN
               WRITE (string,'(i2)') n
               WRITE (lun,'(a)',REC=1) string(:nrecl)
               CALL UNLOCK(lun)
            ELSE
               string((9+n):(9+n)) = ' '
               WRITE (lun,'(a)',REC=1) string(:nrecl)
               CALL UNLOCK(lun)
            ENDIF
            IF ( sys ) THEN
               read_sys = .TRUE.
            ELSE
               read_user = .TRUE.
            ENDIF
         ELSE
            WRITE (ZWRite,*) ' wrong flag in udc'
            CALL XWRITE(ZWRite,5)
         ENDIF
      ENDIF
c
c close the file if delete or load (flag 3 and 0)
c
 200  IF ( (flag.EQ.3 .OR. flag.EQ.0) ) THEN
c
c first check the file exists and is open
c
         INQUIRE (FILE=cfile,EXIST=there,OPENED=open,NUMBER=lun)
c
c if the file is there and is opne, close it
c
         IF ( there .AND. open ) THEN
            CLOSE (lun)
            CALL FRELUN(lun)
         ENDIF
      ENDIF
      RETURN
c
c note: formats used with zwrite, which is char*256 (estec.inc)
c
99001 FORMAT (' Open error no ',i5,' for file ',a226)
99004 FORMAT (' Maximum number of commands exceeded ',i5)
      END
