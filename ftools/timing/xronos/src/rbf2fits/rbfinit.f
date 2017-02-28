      subroutine rbfinit(rbf_name,fits_name,tchat,lchat,ierr)

c get parameter values for program rbf2fits
c  O  rbf_name  (c)  Name of input file
c  O  fits_name (c)  name of output file
c  O  tchat     (i)  Terminal chattiness
c  O  lchat     (i)  Log chattiness
c  O  ierr      (i)  error status
c subroutine calls: tbldstand, xaerror, uclgsi, uclpsi, uclgst, uclpst
c

      IMPLICIT NONE

      include '../../include/io.inc'
      INCLUDE 'rbf2fits.inc'
c
      CHARACTER*(*) fits_name,rbf_name
      character(80) par_file
      INTEGER*4 ierr,tchat,lchat,LENACT
      EXTERNAL LENACT
      parameter (subname = 'rbfinit:')
c
c Initialize XPI.

c      CALL tbldstand(program,disk,directory,ierr)

c Prompt user for input file name.

      CALL uclgst('infile',rbf_name,ierr)
c      CALL uclpst('infile',rbf_name,ierr)
      errm = 'Error reading input file name'
      IF(ierr.ne.0) GOTO 999

c Prompt user for output file.
      call xwrite (' If blank uses infile with ".lc" extension', 10)
      CALL uclgst('outfile',fits_name,ierr)
c      CALL uclpst('outfile',fits_name,ierr)
      errm = 'Error reading output file name'
      IF(ierr.ne.0) GOTO 999

c Read hidden chattiness parameters from .par file.

      CALL uclgsi('tchat',tchat,ierr)
c      CALL uclpsi('tchat',tchat,ierr)
      CALL uclgsi('lchat',lchat,ierr)
c      CALL uclpsi('lchat',lchat,ierr)
      errm = 'Error reading/writing chattiness parameters'
      IF(ierr.ne.0) GOTO 999

c Save parameter file.

c      par_file = program(:lenact(program)) // '.par'
c      CALL tbsvpr(par_file,ierr)
c      errm = 'Error saving parameter file'
c      IF(ierr.ne.0) GOTO 999

      GOTO 1000

999   CONTINUE
      errm = subname//' '//errm
      CALL xaerror(errm,1)
1000  CONTINUE

      RETURN
      END
