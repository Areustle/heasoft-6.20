      subroutine cmadeadinit(infile, evtfile, outfile, minexpo,
     &     inttime, tchat, lchat, clobber,ierr)
c
c
c INITialize files and parameters for program cmadead
c read parameters in the parameters file.
c
c  O  infile    (d)  input file 
c  O  evtfile   (d)  event file
c  O  outfile   (d)  output file
c  O  tchat     (i)  Chattiness terminal
c  O  lchat     (i)  Chattiness log 
c  O  ierr      (i)  Error return
c  O  clobber   (l)  Overwrite output file if it exists?
      IMPLICIT NONE


      character(80) errm
      CHARACTER*(*) infile, outfile, evtfile
      INTEGER*4 ierr, tchat, lchat
      REAL*8 minexpo, inttime
      LOGICAL clobber
c
      errm = ' '

c input data directory
c      CALL uclgst('datadir',datadir,ierr)
c      errm= 'Unable to get DATADIR'
c      IF(ierr.NE.0) goto 999

c input file name 
      CALL uclgst('infile',infile,ierr)
      errm= 'Unable to get INFILE'
      IF(ierr.NE.0) goto 999
c
c input list event file  
      CALL uclgst('evtfile',evtfile,ierr)
      errm= 'Unable to get EVTFILE'
      IF(ierr.NE.0) goto 999

c
c output file name
      CALL uclgst('outfile',outfile,ierr)
      errm= 'Unable to get OUTFILE'
      IF(ierr.NE.0) goto 999

c
c Minimun exposure 
      CALL uclgsd('minexpo',minexpo,ierr)
      errm= 'Unable to get MINEXPO'
      IF(ierr.NE.0) goto 999

c
c Integration time
c      CALL uclgsd('inttime ',inttime,ierr)
c      errm= 'Unable to get INTTIME'
c      IF(ierr.NE.0) goto 999

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
