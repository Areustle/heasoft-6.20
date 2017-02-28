
      SUBROUTINE xinfix(prompt,string,iparse,ierr,*,*)

      CHARACTER*(*) prompt,string
      INTEGER iparse,ierr

c            rashafer 1986 mar 5
c      XPARSE subroutine to prompt the user at the terminal for a fixed
c      parameter to be inserted before the current parse position
c      prompt      c*      i: prompt string
c      string      c*      i/r: parse string
c      iparse      i4      i/r: parse position
c      ierr      i4      r: error flag returned during the read (N.B. that
c                  if the insertion fails no error is currently generated
c                  although it is signaled by the secondary return)
c      Alternate returns:
c            *1 - EOF
c            *2 - The insertion failed for space (or someother I/O error)

      INCLUDE 'xparinc.inc'
      INCLUDE 'writebuf.inc'

      INTEGER ierr2, oldlen, newlen

      INTEGER lenact
      EXTERNAL lenact

c Force the next few lines to be read from the terminal

      CALL xnewcm('*',.true.,ierr)

c Save all characters after the current parse position in the xprmsw workspace

      xprmsw = ' '
      IF ( iparse .LT. len(string) ) THEN
         xprmsw = ' '//string(iparse+1:)
         oldlen = lenact(xprmsw)
      ELSE
         oldlen=0
      ENDIF

c Read from the command line into string

      CALL xcread(prompt,string,ierr)

c If the read failed then put back the saved string and return

      IF ( ierr .NE. 0 ) THEN
         IF ( oldlen .GT. 1 ) THEN
            string = xprmsw(2:oldlen)
         ELSE
            string = ' '
         ENDIF
         iparse = 0
         IF ( ierr .LT. 0 ) THEN
c            ** n.b. that there was no need to remove the terminal from
c            ** the command file when the EOF was generated (ierr<0) as it
c            ** was already done by xcread
            RETURN 1
         ELSE
            CALL xrmvcm(ierr2)
            RETURN 2
         ENDIF
      ENDIF

c Return control to any script in use

      CALL xrmvcm(ierr2)

c Construct the new current parse string by appending the saved string

      newlen = lenact(string)
      IF ( newlen .LE. 0 ) THEN
         newlen = 1
         string(1:1) = comma
      ENDIF
      iparse = 0
      string(newlen+1:) = xprmsw(:MAX(oldlen,len(string)-newlen))
      IF ( newlen+oldlen .GT. len(string) ) THEN
         wrtstr = '*WARNING*: During an insertion some data was lost:'//
     &          '"'//xprmsw(len(string)-newlen+1:oldlen)//'"'
         CALL xwrite(wrtstr, 2)
         WRITE(wrtstr,'(3(a,i8))') 'newlen = ', newlen, ' oldlen = ', 
     &     oldlen, ' len(string) = ', len(string)
         CALL xwrite(wrtstr, 2)
         RETURN 2
      ENDIF

      RETURN
      END
