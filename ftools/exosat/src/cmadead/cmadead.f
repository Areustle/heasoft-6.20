c
      subroutine cmadead 
c 
c Corrects rates in the cma lightcurve for the dead time 
c
c program  name for this program
c
c      character(160) datadir
      character(160) infile, evtfile, outfile
      REAL*8 minexpo, inttime
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
      program='cmadead'
      CALL cmadeadinit (infile, evtfile, outfile, minexpo,
     &     inttime, tchat,lchat,clobber,ierr)
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
      CALL cmadeadmake (infile, evtfile, outfile, minexpo, 
     &             ichat,clobber,ierr)
        IF (ierr.NE.0) THEN
          WRITE(errm, '('' Status from cmadeadmake : '',I4)')ierr
          CALL XERROR(errm, 5)
        ENDIF
      ENDIF
      IF (ierr.NE.0) THEN
         WRITE(errm, '('' Status after closing : '',I4)')ierr
         CALL XERROR(errm, 5)
      ENDIF
      END
