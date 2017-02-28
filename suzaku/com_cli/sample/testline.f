      integer  length
      character * 80  buffer
C
 10   Continue
      Call CLrdln('PROMPT> ',buffer,length)
      write(*,*) 'length=',length
      write(*,*) 'buffer=',buffer(:length)
C
      Goto 10
      End
