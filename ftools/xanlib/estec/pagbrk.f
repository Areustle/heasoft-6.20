**==PAGBRK.spg  processed by SPAG 3.09I  at 09:45 on 20 Aug 1992
      SUBROUTINE PAGBRK(Qend)
c
c	routine to do parsable pagebreaks in browse
c							31/1/90 nick
      LOGICAL*4 Qend , ierr
      character(80) prompt
      character(10) reply
c
      Qend = .FALSE.
c
      reply = ' '
      WRITE (prompt,99001)
      CALL XCREAD(prompt,reply,ierr)
      IF ( ierr .OR. reply.NE.' ' ) Qend = .TRUE.
c
      RETURN
99001 FORMAT (
     &       ' <CR> continues, to exit type any character <CR> or <EOF>'
     &       )
      END
