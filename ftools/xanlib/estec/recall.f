**==RECALL.spg  processed by SPAG 3.09I  at 12:44 on  1 Jun 1993
      SUBROUTINE RECALL(N_max,Start_no,Command_no,Flag,Filename,Ierr)
c   flag=1  -> list last 20 (or up to 50) previously executed commands
c   flag=2  -> store last command in history file
c   flag=3  -> retrieve command
c
      INCLUDE 'estec.inc'
c
      CHARACTER*(*) Filename
      INTEGER*4 lu_history , Ierr , Flag , i , Command_no , Start_no , 
     &          total
      INTEGER*4 LENACT , start , isum , N_max , id , nrecl , max_list
      character(20) form
      character(256) string
      LOGICAL open , there , ok
      nrecl = 256
      IF ( Flag.LT.1 .OR. Flag.GT.3 ) THEN
         WRITE (Zwrite,
     &          '('' subroutine recall called with wrong flag '')')
         CALL XWRITE(Zwrite,5)
         RETURN
      ENDIF
      INQUIRE (FILE=Filename,EXIST=there,OPENED=open,NUMBER=lu_history)
      IF ( Flag.EQ.1 .AND. .NOT.there ) RETURN
      IF ( Flag.EQ.2 .AND. .NOT.there ) THEN
         CALL GETLUN(lu_history)
c
         CALL OPENWR(lu_history,Filename,'new','df',' ',nrecl,0,Ierr)
c
         IF ( Ierr.NE.0 ) THEN
            WRITE (Zwrite,99001) Ierr,
     &             Filename(:MIN(LENACT(Filename),len(Zwrite)-30))
            CALL XWRITE(Zwrite,5)
            RETURN
         ENDIF
c
c         OPEN (lu_history,FILE=filename,shared,
c     &         STATUS='new',RECL=100,ACCESS='direct',MAXREC=51,
c     &         FORM='formatted')
c
         string = ' '
         DO 50 i = 2 , 50
            WRITE (lu_history,'(a)',REC=i) string
            CALL UNLOCK(lu_history)
 50      CONTINUE
         string = ' 0   1'
         WRITE (lu_history,'(a)',REC=1) string
         CALL UNLOCK(lu_history)
      ENDIF
      IF ( there .AND. .NOT.open ) THEN
         CALL GETLUN(lu_history)
c
         CALL OPENWR(lu_history,Filename,'old','df',' ',nrecl,0,Ierr)
c
c
c         OPEN (lu_history,FILE=filename,shared,
c     &         STATUS='old',RECL=100,ACCESS='direct',MAXREC=51,
c     &         FORM='formatted')
      ENDIF
      IF ( Flag.EQ.1 ) THEN
         IF ( N_max.GT.50 ) THEN
            N_max = 50
            WRITE (Zwrite,
     &             '('' max no of listed commands reset to 50 '')')
            CALL XWRITE(Zwrite,5)
         ENDIF
         READ (lu_history,'(a)',REC=1,ERR=100) string
         CALL UNLOCK(lu_history)
         READ (string,'(I2,2x,i2)',ERR=100) total , start
         CALL UNLOCK(lu_history)
         ok = .TRUE.
c
         IF ( total-1.LT.N_max ) THEN
            max_list = total - 1
         ELSE
            max_list = N_max
         ENDIF
c
         isum = max_list
         i = start - max_list - 1
         DO WHILE ( ok )
            i = i + 1
            IF ( i.LT.2 ) i = i + 50
            IF ( i.GT.51 ) i = i - 50
            Command_no = Start_no - isum
            string = ' '
            READ (lu_history,'(a)',REC=i,ERR=100) string
            CALL UNLOCK(lu_history)
            IF ( Command_no.GT.0 ) THEN
               id = LOG(REAL(Command_no)) + 1
            ELSE
               id = 2
               IF ( Command_no.LE.-10 ) id = 3
            ENDIF
            CALL CRTFMT(id,form,3)
            WRITE (*,form) Command_no , string(1:LENACT(string))
c            IF ( total.LT.51 .AND. i.LE.2 ) ok = .FALSE.
            isum = isum - 1
            IF ( isum.LT.0 ) ok = .FALSE.
         ENDDO
      ELSEIF ( Flag.EQ.2 ) THEN
         READ (lu_history,'(a)',REC=1,ERR=100) string
         CALL UNLOCK(lu_history)
         READ (string,'(I2,2x,i2)',ERR=100) total , start
         CALL UNLOCK(lu_history)
         total = total + 1
         start = start + 1
         IF ( total.GT.51 ) total = 51
         IF ( start.GT.51 ) start = 2
         string = Zstring(:LENACT(Zstring))
         WRITE (lu_history,'(a)',REC=start) string
         CALL UNLOCK(lu_history)
         WRITE (string,'(I2,2x,i2)') total , start
         WRITE (lu_history,'(a)',REC=1) string
         CALL UNLOCK(lu_history)
      ELSEIF ( Flag.EQ.3 ) THEN
         READ (lu_history,'(a)',REC=1,ERR=100) string
         CALL UNLOCK(lu_history)
         READ (string,'(I2,2x,i2)',ERR=100) total , start
         CALL UNLOCK(lu_history)
         i = start - Start_no + Command_no
         IF ( i.GT.51 ) i = i - 50
         IF ( i.LT.2 ) i = i + 50
         READ (lu_history,'(a)',REC=i,IOSTAT=Ierr) string
         CALL UNLOCK(lu_history)
         IF ( Ierr.NE.0 .OR. string.EQ.' ' ) THEN
            Ierr = 1
            WRITE (Zwrite,99002)
            CALL XWRITE(Zwrite,5)
            RETURN
         ENDIF
         id = 1
         IF ( Command_no.GE.10 .OR. Command_no.LT.0 ) id = 2
         IF ( Command_no.LE.-10 ) id = 3
         Zstring = string
      ENDIF
      RETURN
* we had an error with an empty file
 100  string = ' '
      DO 200 i = 2 , 50
         WRITE (lu_history,'(a)',REC=i) string
         CALL UNLOCK(lu_history)
 200  CONTINUE
      string = ' 0   1'
      WRITE (lu_history,'(a)',REC=1) string
      CALL UNLOCK(lu_history)
      Ierr = 1
      RETURN
 
99001 FORMAT (' Error no: ',i5,' opening file ',a)
99002 FORMAT (' Recall error, command not found')
      END
