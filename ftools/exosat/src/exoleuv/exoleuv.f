c
      subroutine exoleuv
c
c EXOSAT LE UV count rate for stars of different spectral types
c
      INTEGER*4 tchat,lchat,ichat, ierr
      INTEGER*4 lenact, parse
      REAL*4 filter, vmag, evmag 
      character(80) log_file,sptype, errm, program, string, istring
c
      character(40) taskname
      common /task/ taskname




c Dummy variables for log file.

      DATA istring,parse /' ',0/

c

      taskname = 'exoleuv 1.0'
      ierr=0
      errm=' '
      program='exoleuv'
c
c Get the parameter
        CALL leuv_par(filter ,sptype, vmag, evmag, tchat,lchat,ierr)
        IF(ierr.EQ.0)THEN
c open the log file if necessary tchat>=lchat
c reset the internal chatness to lchat 

          CALL xchaty(tchat,lchat)
c          ichat=lchat
          ichat=tchat
c          string=program(1:lenact(program))//' start'
c          CALL XWRITE(string, ichat) 
          log_file='+'//program(1:lenact(program))//'.log'
          IF (lchat.GE.tchat) THEN
            CALL SETLOG (istring, parse, log_file, ' ')
          ENDIF
c
c calculate count_rate        
C
          CALL leuv_make(filter ,sptype, vmag, evmag, ichat, ierr)
          IF (ierr.NE.0) THEN
            WRITE(errm, '('' Status from leuv_make : '',I4)')ierr
            CALL XERROR(errm, 5)
          ENDIF
        ELSE
            WRITE(errm, '('' Status from leuv_par : '',I4)')ierr
            CALL XERROR(errm, 5)
        ENDIF
        IF (ierr.NE.0) THEN
           WRITE(errm, '('' Status after closing : '',I4)')ierr
           CALL XERROR(errm, 5)
        ENDIF
        END
