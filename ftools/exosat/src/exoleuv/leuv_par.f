***leuv_par.for
       SUBROUTINE leuv_par(filter,sptype,vmag,bvmag,tchat,lchat,ierr)
c
c get the parameters open log file 
c O filter 
c O sptype spectral type
c O vmag visual mag
c O bvmag b-v 
c tchat lchat terminal and log chatness
c ierr error if ne 0.
       
       CHARACTER*(*) sptype
       character(80) errm, par_file
       REAL*4 vmag, bvmag, filter
       INTEGER*4 tchat, lchat, ierr, lenact
c
       errm=' '

c
c Get parameters
c filter code
c
       call uclgsr('FILTER',filter,ierr)
       errm= 'Unable to read FILTER'
       IF(ierr.NE.0) GOTO 999
c
c spectral type
c
       call uclgst('SPTYPE',sptype,ierr)
       errm= 'Unable to read SPECTRAL TYPE'
       IF(ierr.NE.0) GOTO 999
c
c magnitude
c
       call uclgsr('VMAG',vmag,ierr)
       errm= 'Unable to read V Magnitude'
       IF(ierr.NE.0) GOTO 999
c
c B_V
c
       call uclgsr('BVMAG',bvmag,ierr)
       errm='Unable to read E(B_V)'
       IF(ierr.NE.0) GOTO 999
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
c
      IF(ierr.eq.0) goto 1000

999   CONTINUE
      call xaerror(errm,5)
1000  CONTINUE
      RETURN
      END 
