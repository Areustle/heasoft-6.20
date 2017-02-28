      subroutine bctinit(in_fil,ou_fil,rastr,decstr
     &                 ,tchat,lchat,eph_fil,ierr)

c Read parameter file for program bct.

c  O  in_fil    (c)  Name of input file
c  O  ou_fil    (c)  name of output file
c  O  rastr     (c)  R.A. string
c  O  decstr    (c)  Declination string
c  O  tchat     (i)  Terminal chattiness
c  O  lchat     (i)  Log chattiness
c  O  eph_fil   (c)  name of JPL 2000 ephemeris file
c  O  ierr      (i)  error status

c subroutine calls: tbldstand, getinfil, getlun, xaerror, ftopen
c                   getoufil, raanddec, ftinit, delfil, ftphpr, ftpdef,
c                   ftpkyd

c Author:  eal GSFC/HSTX HEASARC  February 1994

      IMPLICIT NONE
 
c      INCLUDE 'bctdef.inc'
 
      CHARACTER*(*) ou_fil,in_fil,rastr,decstr,eph_fil
      character(80) errm
      INTEGER ierr,tchat,lchat

c Initialize XPI.

c      CALL tbldstand(program,disk,directory,ierr)

c Prompt user for input file name.

      CALL uclgst('infile',in_fil,ierr)
c      CALL uclpst('infile',in_fil,ierr)
      errm = 'Error reading/writing input file name'
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

c Read hidden JPL 2000 ephemeris file name

      CALL uclgst('ephfile',eph_fil,ierr)
c      CALL uclpst('ephfile',eph_fil,ierr)
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
