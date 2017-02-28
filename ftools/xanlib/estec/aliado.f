**==ALIADO.spg  processed by SPAG 3.09I  at 09:41 on 20 Aug 1992
      SUBROUTINE ALIADO(Alias_command,Command,Show,All,Delete,System,
     &                  File_system,File_user,New,Page)
c
      INCLUDE 'estec.inc'
c
      INTEGER LENACT , ieof
      CHARACTER*(*) Alias_command , Command
      CHARACTER*(*) File_system , File_user
      LOGICAL*4 Show , define , Delete , System , user , All
      LOGICAL*4 New , Page
c
      IF ( New ) define = .TRUE.
c
      IF ( Delete .OR. Show .OR. All ) THEN
         define = .FALSE.
      ELSE
         define = .TRUE.
      ENDIF
c
      IF ( All ) THEN
         System = .TRUE.
         user = .TRUE.
      ENDIF
c
      IF ( .NOT.System .AND. .NOT.user ) user = .TRUE.
c
      IF ( Show .AND. Alias_command.EQ.' ' ) Alias_command = '*'
c tell them/ask  what they have done...
      IF ( define ) THEN
         IF ( Alias_command.EQ.' ' ) THEN
            CALL XCREAD('Enter alias name (<CR> to exit): ',
     &                  Alias_command,ieof)
            IF ( ieof.NE.0 ) RETURN
            CALL UPC(Alias_command)
            IF ( Alias_command.EQ.' ' ) RETURN
         ENDIF
         IF ( Command.EQ.' ' ) THEN
            CALL XCREAD('Enter command to be aliased (<CR> to exit):',
     &                  Command,ieof)
            IF ( ieof.NE.0 ) RETURN
            CALL UPC(Command)
            IF ( Command.EQ.' ' ) RETURN
         ENDIF
         Zwrite = Alias_command(:LENACT(Alias_command)) //
     &            Command(:LENACT(Command))
         CALL XWRITE(Zwrite,5)
      ELSEIF ( Delete ) THEN
         IF ( Alias_command.EQ.' ' ) THEN
            CALL XCREAD('Enter alias name to delete (<CR> to exit): ',
     &                  Alias_command,ieof)
            IF ( ieof.NE.0 ) RETURN
            CALL UPC(Alias_command)
            IF ( Alias_command.EQ.' ' ) RETURN
         ENDIF
      ENDIF
      IF ( Show .AND. System ) THEN
         CALL UDC(Command,Alias_command,12,File_system,File_user,New,
     &            Page)
      ENDIF
      IF ( Show .AND. user ) THEN
         CALL UDC(Command,Alias_command,2,File_system,File_user,New,
     &            Page)
      ENDIF
      IF ( define .AND. System ) THEN
         IF ( Zcurrent_directory.EQ.Zsysdir ) THEN
            CALL UDC(Command,Alias_command,10,File_system,File_user,New,
     &               Page)
         ELSE
            WRITE (Zwrite,99002) Zsysdir
            CALL XWRITE(Zwrite,5)
            RETURN
         ENDIF
      ELSEIF ( define .AND. user ) THEN
         CALL UDC(Command,Alias_command,0,File_system,File_user,New,
     &            Page)
      ELSEIF ( Delete .AND. System ) THEN
         CALL UDC(Command,Alias_command,13,File_system,File_user,New,
     &            Page)
      ELSEIF ( Delete .AND. user ) THEN
         CALL UDC(Command,Alias_command,3,File_system,File_user,New,
     &            Page)
      ENDIF
      RETURN
99002 FORMAT (' Error: current directory must be ',a)
      END
