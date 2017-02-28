**==aliado1.spg  processed by SPAG 4.50F  at 15:17 on 26 Aug 1994
      SUBROUTINE ALIADO1(Alias_command,Command,Show,All,Delete,System,
     &                   File_system,File_user,New,Page)
c
      INCLUDE 'estec.inc'
c
      INTEGER LENACT , ieof
      CHARACTER*(*) Alias_command , Command
      CHARACTER*(*) File_system , File_user
      LOGICAL Show , define , Delete , System , user , All
      LOGICAL New , Page
 
c
      IF ( New ) define = .TRUE.
c
      IF ( Delete .OR. Show .OR. All ) THEN
         define = .FALSE.
      ELSE
         define = .TRUE.
      ENDIF
c
      IF ( .NOT.System ) user = .TRUE.
c
      IF ( All ) THEN
         System = .TRUE.
         user = .TRUE.
      ENDIF
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
         ZWRite = ' ' // Alias_command(:LENACT(Alias_command)) //
     &            ' == '// Command(:LENACT(Command))
         CALL XWRITE(ZWRite,5)
      ELSEIF ( Delete ) THEN
         IF ( Alias_command.EQ.' ' ) THEN
            CALL XCREAD('Enter alias name to delete (<CR> to exit): ',
     &                  Alias_command,ieof)
            IF ( ieof.NE.0 ) RETURN
            CALL UPC(Alias_command)
            IF ( Alias_command.EQ.' ' ) RETURN
         ENDIF
      ENDIF
      IF ( Show .AND. System ) CALL UDC1(Command,Alias_command,12,
     &     File_system,File_user,New,Page)
      IF ( Show .AND. user ) CALL UDC1(Command,Alias_command,2,
     &                                 File_system,File_user,New,Page)
      IF ( define .AND. System ) THEN
         IF ( ZCUrrent_directory.EQ.ZSYsdir ) THEN
            CALL UDC1(Command,Alias_command,10,File_system,File_user,
     &                New,Page)
         ELSE
            WRITE (ZWRite,99002) ZSYsdir
            CALL XWRITE(ZWRite,5)
            RETURN
         ENDIF
      ELSEIF ( define .AND. user ) THEN
         CALL UDC1(Command,Alias_command,0,File_system,File_user,New,
     &             Page)
      ELSEIF ( Delete .AND. System ) THEN
         CALL UDC1(Command,Alias_command,13,File_system,File_user,New,
     &             Page)
      ELSEIF ( Delete .AND. user ) THEN
         CALL UDC1(Command,Alias_command,3,File_system,File_user,New,
     &             Page)
      ENDIF
      RETURN
99002 FORMAT (' Error: current directory must be ',a)
      END
