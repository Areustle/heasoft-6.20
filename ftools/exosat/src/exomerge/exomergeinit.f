      subroutine exomergeinit(evtfile, outfile, 
     &     tchat, lchat, clobber,ierr)
c
c
c INITialize files and parameters for program exomerge
c read parameters in the parameters file.
c
c  O  evtfile   (d)  list of event files
c  O  outfile   (d)  output file
c  O  tchat     (i)  Chattiness terminal
c  O  lchat     (i)  Chattiness log 
c  O  ierr      (i)  Error return
c  O  clobber   (l)  Overwrite output file if it exists?
      IMPLICIT NONE


      character(80) errm
      CHARACTER*(*) outfile, evtfile
      INTEGER*4 ierr, tchat, lchat
      LOGICAL clobber
c
      errm = ' '

c
c input list event file  
      CALL uclgst('evtfile',evtfile,ierr)
      errm= 'Unable to get EVTFILE'
      IF(ierr.NE.0) goto 999
      call xwrite(evtfile,30)
      IF(evtfile(1:1).NE.'@')THEN
         evtfile='@'//evtfile
      call xwrite(evtfile,30)
      ENDIF
c
c output file name
      CALL uclgst('outfile',outfile,ierr)
      errm= 'Unable to get OUTFILE'
      IF(ierr.NE.0) goto 999

c
c terminal chat
      CALL uclgsi('tchat',tchat,ierr)
      errm ='Unable to get TCHAT'
      IF(ierr.NE.0) goto 999

c
c log chat
      CALL uclgsi('lchat',lchat,ierr)
      errm= 'Unable to get LCHAT'
      IF(ierr.NE.0) goto 999 

c
c clobber?
      CALL uclgsb('clobber',clobber,ierr)
      errm ='Unable to get CLOBBER'
      IF(ierr.NE.0) goto 999


      IF(ierr.eq.0) goto 1000
c
999   CONTINUE
      call xerror(errm,5)
1000  CONTINUE
      RETURN
      END
