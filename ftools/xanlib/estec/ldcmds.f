**==LDCMDS.spg  processed by SPAG 3.09I  at 09:44 on 20 Aug 1992
      SUBROUTINE LDCMDS(Infile,Ierr)
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
      INTEGER*4 ispace , isp , iend , Ierr , ist , lrecl
      character(16) temp_com(ZNCOM) , temp1(ZNCOM)
      character(30) temp_descrip(ZNCOM)
c
      INTEGER*4 io , LENACT , irec , po(ZNCOM) , n
      LOGICAL*4 list
c
      Ierr = 0
      CALL GETLUN(io)
c      OPEN (UNIT=io,FILE=infile,STATUS='old',shared,ERR=200)
c
      lrecl = 0
      CALL OPENWR(io,Infile,'OLD',' ',' ',lrecl,1,Ierr)
c
      IF ( Ierr.NE.0 ) THEN
        WRITE (Zwrite,99001)Infile(:MIN(LENACT(Infile),len(Zwrite)-42)),
     &                      Ierr
        CALL XWRITE(Zwrite,5)
        RETURN
      ENDIF
c
      Zlist_no = 0
      Zlist_nolist = 0
c
c hardwired commands
c
      Zcom_name(1) = 'alias'
      Zcom_descrip(1) = 'rename command string'
      Zcom_name(2) = 'log'
      Zcom_descrip(2) = 'output to log file'
      Zcom_name(3) = 'chatter'
      Zcom_descrip(3) = 'output verbosity'
      Zcom_name(4) = 'script'
      Zcom_descrip(4) = 'write commands to a file'
      Zcom_name(5) = 'recall'
      Zcom_descrip(5) = 'recall command'
      Zcom_name(6) = 'reexecute'
      Zcom_descrip(6) = 'reexecute commands'
      Zcom_name(7) = 'buffer'
      Zcom_descrip(7) = 'recall by terminal ^'
      Zcom_name(8) = '?'
      Zcom_descrip(8) = 'list commands'
      Zcom_name(9) = 'prompt'
      Zcom_descrip(9) = 'prompt default'
      irec = 9
      n = 1
      DO WHILE ( n.LE.irec )
         Zcom_list(n) = Zcom_name(n)
         CALL UPC(Zcom_name(n))
         Zcom_descrip_list(n) = Zcom_descrip(n)
         n = n + 1
      ENDDO
      Zlist_no = irec
c
      DO WHILE ( irec.LT.ZNCOM )
         CALL RDULKC(io,string,iend,Ierr)
         CALL UNLOCK(io)
         IF ( iend.EQ.1 ) GOTO 100
         IF ( Ierr.NE.0 ) THEN
c
            Zwrite = ' Error: reading ' // Infile(:LENACT(Infile))
            CALL XWRITE(Zwrite,5)
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
               WRITE (Zwrite,99002)
               CALL XWRITE(Zwrite,5)
               irec = irec - 1
            ELSEIF ( ispace.EQ.iend ) THEN
               WRITE (Zwrite,99003)
               CALL XWRITE(Zwrite,5)
               irec = irec - 1
            ELSE
               isp = ispace
               IF ( isp.GT.16 ) isp = 16
               Zcom_name(irec) = string(ist:isp)
               DO WHILE ( string(ispace:ispace).EQ.' ' .AND. 
     &                    ispace.LT.iend )
                  ispace = ispace + 1
               ENDDO
               ist = ispace
               if ( iend-ist+1.GT.30 ) iend = ist + 29
               Zcom_descrip(irec) = string(ist:iend)
c
               IF ( list ) THEN
                  Zlist_no = Zlist_no + 1
                  Zcom_list(Zlist_no) = Zcom_name(irec)
                  Zcom_descrip_list(Zlist_no) = Zcom_descrip(irec)
               ELSE
                  Zlist_nolist = Zlist_nolist + 1
                  Zcom_nolist(Zlist_nolist) = Zcom_name(irec)
                  Zcom_descrip_nolist(Zlist_nolist) = Zcom_descrip(irec)
               ENDIF
               CALL UPC(Zcom_name(irec))
            ENDIF
         ENDIF
c
      ENDDO
 100  Zcom_no = irec
c     	write(zwrite,876)zcom_no, zlist_no, zncom
c876	format(' there are',i5,' browse commands, with', i5,
c     &' visible and',i5,' possible')
c     	call xwrite(zwrite,5)
      IF ( ZNCOM-Zcom_no.LT.5 ) WRITE (Zwrite,99004)
c
      IF ( Zcom_no.EQ.ZNCOM ) THEN
         WRITE (Zwrite,99005)
         CALL XWRITE(Zwrite,5)
      ENDIF
c
      CLOSE (io)
      CALL FRELUN(io)
c
c put visible in alphabetical order
c  sort on the command name
c
      n = 1
      DO WHILE ( n.LE.Zlist_no )
         temp_com(n) = Zcom_list(n)
         temp_descrip(n) = Zcom_descrip_list(n)
         po(n) = n
         n = n + 1
      ENDDO
c
      CALL QSORSM(Zlist_no,Zcom_descrip_list,temp1,po)
c
      n = 1
      DO WHILE ( n.LE.Zlist_no )
         Zcom_list(n) = temp_com(po(n))
         Zcom_descrip_list(n) = temp_descrip(po(n))
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
