**==ALIAS.spg  processed by SPAG 3.09I  at 09:41 on 20 Aug 1992
      SUBROUTINE ALIAS(Command,File_system,File_user,File_type,Ui_no,
     &                 Status)
c
c alias command names to other names
c from Paolo's ximage version
c Nick 4.9.90
c
      INCLUDE 'estec.inc'
c
      INTEGER*4 Status
      INTEGER*4 Ui_no
      CHARACTER*(*) Command , File_system , File_user , File_type
      character(256) saved_command
      character(256) alias_command
      LOGICAL*4 show , delete , system , previous , all
      LOGICAL*4 new , page
c
      saved_command = Command
c
      CALL ALIAUI(alias_command,Command,show,all,delete,system,previous,
     &            new,page,Status)
      IF ( Status.NE.0 ) RETURN
      IF ( previous ) Command = saved_command
      CALL ALIADO(alias_command,Command,show,all,delete,system,
     &            File_system,File_user,new,page)
c
      RETURN
      END
