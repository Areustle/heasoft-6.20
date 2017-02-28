c
      subroutine exomerge 
c 
c Merges LE event files using housekeeping files when necessary.
c
c
      character(160) evtfile, outfile
      INTEGER*4 ichat, lchat, tchat
      character(80) log_file, istring, program
      character(80) errm, string
      INTEGER*4 lenact, ierr, parse 
      LOGICAL clobber

c Dummy variables for log file.

      DATA istring,parse /' ',0/

c
c Initialize variable
c Set up FITSIO and XPI, and get parameters.
      ierr=0
      errm=' '
      program='exomerge'
      CALL exomergeinit (evtfile, outfile,
     &     tchat,lchat,clobber,ierr)
c
      IF (ierr.EQ.0) THEN
c open the log file if necessary tchat>=lchat
c reset the internal chatness to lchat
c
        CALL xchaty(tchat,lchat)
        ichat=lchat
        string=program(1:lenact(program))//' start correction'
        CALL XWRITE(string, ichat) 
        log_file='+'//program(1:lenact(program))//'.log'
        IF (lchat.GE.tchat) THEN
           CALL SETLOG (istring, parse, log_file, ' ')
        ENDIF
c
c Make the correction to the TIME column 
c If HDUCLAS is EVENT also to the GTI
c
      CALL exomergemake (evtfile, outfile, 
     &             ichat,clobber,ierr)
        IF (ierr.NE.0) THEN
          WRITE(errm, '('' Status from exomergemake : '',I4)')ierr
          CALL XERROR(errm, 5)
        ENDIF
      ENDIF
      IF (ierr.NE.0) THEN
         WRITE(errm, '('' Status after closing : '',I4)')ierr
         CALL XERROR(errm, 5)
      ENDIF
      END
