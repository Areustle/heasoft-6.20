**==alias1.spg  processed by SPAG 4.50F  at 15:17 on 26 Aug 1994
      SUBROUTINE ALIAS1(Command,File_system,File_user,File_type,Ui_no,
     &                  Status)
c
c alias command names to other names
c from Paolo's ximage version
c Nick 4.9.90
c
c
      INTEGER*4 Status
      INTEGER*4 Ui_no
      CHARACTER*(*) Command , File_system , File_user , File_type
      character(256) saved_command
      character(256) alias_command
      LOGICAL show , delete , system , previous , all
      LOGICAL new , page
 
      INTEGER*4 ierr
c
      saved_command = Command
      alias_command = ' '
      ierr = 0
c
      CALL UCLGSB('show',show,ierr)
      ierr = 0
      CALL UCLGSB('all',all,ierr)
      ierr = 0
      CALL UCLGSB('delete',delete,ierr)
      ierr = 0
      CALL UCLGSB('system',system,ierr)
      ierr = 0
      CALL UCLGSB('previous',previous,ierr)
      ierr = 0
      CALL UCLGSB('all',all,ierr)
      ierr = 0
      CALL UCLGSB('new',new,ierr)
      ierr = 0
      CALL UCLGSB('page',page,ierr)
 
      IF ( show .OR. all ) THEN
      ELSEIF ( delete ) THEN
         ierr = 0
         CALL UCLGST('aname',alias_command,ierr)
         IF ( alias_command.EQ.' ' ) RETURN
      ELSE
         ierr = 0
         CALL UCLGST('aname',alias_command,ierr)
         IF ( alias_command.EQ.' ' ) RETURN
         Command = ' '
         ierr = 0
         CALL UCLGST('acmd',Command,ierr)
      ENDIF
      IF ( previous ) Command = saved_command
      CALL ALIADO1(alias_command,Command,show,all,delete,system,
     &             File_system,File_user,new,page)
      RETURN
      END
