      subroutine abcinit(in_fil,bc_fil,ou_fil,rastr,decstr
     &                   ,tchat,lchat,scc_fil,ierr)

c get parmater values for program ABC.

c  O  in_fil    (c)  Name of input file
c  O  bc_fil    (c)  Name of barycenter correction file
c  O  ou_fil    (c)  name of output file
c  O  rastr     (c)  R.A. string
c  O  decstr    (c)  Declination string
c  O  tchat     (i)  Terminal chattiness
c  O  lchat     (i)  Log chattiness
c  O  scc_fil   (c)  Name of SCC correction file
c  O  ierr      (i)  error status

c Author:  eal GSFC/HSTX HEASARC  February 1994
c Revised  eal   (from abcinit.f) May 1994 to strip out all but XPI calls.

      IMPLICIT NONE

c      INCLUDE 'abcdef.inc'

      CHARACTER*(*) ou_fil,in_fil,bc_fil,rastr,decstr,scc_fil
      character(80) errm
      INTEGER*4 ierr,tchat,lchat

c Initialize XPI.

c      CALL tbldstand(program,disk,directory,ierr)

c Prompt user for input file name.

      CALL uclgst('infile',in_fil,ierr)
c      CALL uclpst('infile',in_fil,ierr)
      errm = 'Error reading/writing input file name'
      IF(ierr.ne.0) GOTO 999

c Prompt user for Barycenter file.

      CALL uclgst('bcfile',bc_fil,ierr)
c      CALL uclpst('bcfile',bc_fil,ierr)
      errm = 'Error reading/writing barycenter file name'
      IF(ierr.ne.0) GOTO 999

c Prompt user for output file.
 
      CALL uclgst('outfile',ou_fil,ierr)
c      CALL uclpst('outfile',ou_fil,ierr)
      errm = 'Error reading/writing output file name'
      IF(ierr.ne.0) GOTO 999

c Prompt user for RA and dec.
 
      CALL uclgst('ra',rastr,ierr)
c      CALL uclpst('ra',rastr,ierr)
      CALL uclgst('dec',decstr,ierr)
c      CALL uclpst('dec',decstr,ierr)
      errm = 'Error reading/writing RA/DEC strings'
      IF(ierr.ne.0) GOTO 999

c Read hidden chattiness parameters from .par file.

      CALL uclgsi('tchat',tchat,ierr)
c      CALL uclpsi('tchat',tchat,ierr)
      CALL uclgsi('lchat',lchat,ierr)
c      CALL uclpsi('lchat',lchat,ierr)
      errm = 'Error reading/writing chattiness parameters'
      IF(ierr.ne.0) GOTO 999

c Read hidden SCC file name

      CALL uclgst('sccfile',scc_fil,ierr)
c      CALL uclpst('sccfile',scc_fil,ierr)
      errm = 'Error reading/writing SCC correction file name'
      IF(ierr.ne.0) GOTO 999

c Save parameter file.

c      par_file = program(:lenact(program)) // '.par'
c      CALL tbsvpr(par_file,ierr)
c      errm = 'Error saving parameter file'
c      IF(ierr.ne.0) GOTO 999

      GOTO 1000

999   CONTINUE
      CALL xaerror(errm,1)
1000  CONTINUE

      RETURN
      END
