      subroutine lcmathinit(in_fil,bg_fil,ou_fil,tchat,lchat,mi,ai,mb,ab
     &                  ,as,docor,iopt,err_mode, ierr)

c INITialize files and parameters for program LCMATH.

c  O  in_fil    (c)  Name of input file (single file only)
c  O  bg_fil    (c)  Name of background file (single file only)
c  O  ou_fil    (c)  name of output file (single file only)
c  O  tchat     (i)  Terminal chattiness
c  O  lchat     (i)  Log chattiness
c  O  mi        (i)  Multiplicative factor for input
c  O  ai        (i)  Additive offset for input
c  O  mb        (i)  Multiplicative factor for background
c  O  ab        (i)  Additive offset for background
c  O  as        (l)  if .true. means Add, rather than Subtract 'bg' file
c  O  docor     (l)  if .true. means apply deadtime/vignet correction
c  O  iopt      (i)  xronos options array (used only for energy bounds here)
c  O  err_mode  (i)  how the error should be calculated 
c  O  ierr      (i)  error status

c Author:  eal GSFC/HSTX HEASARC  February 1994

      IMPLICIT NONE

c      INCLUDE 'lcmathdef.inc'

      include '../../include/io.inc'
      logical as,docor
      character*(*) ou_fil,in_fil,bg_fil
      INTEGER*4 ierr,iopt(*),tchat,lchat,lenact, err_mode
      REAL mi,mb,ai,ab
      EXTERNAL lenact
      parameter (subname = 'lcmathinit:')

c Relevant Parameters:
c
c    infile   Input filename
c    bgfile   Background filename
c    outfile  Output filename
c    mi       Multiplicative constant for input
c    ai       Additive constant for input
c    mb       Multiplicative constant for background
c    ab       Additive constant for background
c    iopt(4)  Maximum energy or channel 
c    iopt(1)  Minimum energy or channel 
c    error_mode  method for the error

      errm = ' '

c Initialize XPI.

c      CALL tbldstand(program,disk,directory,ierr)

c Prompt user for input file name.

      CALL uclgst('infile',in_fil,ierr)
c      CALL uclpst('infile',in_fil,ierr)
      errm = 'Error reading/writing input file name'
      IF(ierr.ne.0) GOTO 999

c Prompt user for Background file.

      CALL uclgst('bgfile',bg_fil,ierr)
c      CALL uclpst('bgfile',bg_fil,ierr)
      errm = 'Error reading/writing Background file name'
      IF(ierr.ne.0) GOTO 999

c Prompt user for output file.

      CALL uclgst('outfile',ou_fil,ierr)
c      CALL uclpst('outfile',ou_fil,ierr)
      errm = 'Error reading/writing output file name'
      IF(ierr.ne.0) GOTO 999

c Prompt user for scaling and offset.

      CALL uclgsr('multi',mi,ierr)
c      CALL uclpsr('multi',mi,ierr)
      CALL uclgsr('multb',mb,ierr)
c      CALL uclpsr('multb',mb,ierr)

c Addition/subtraction switch.

      CALL uclgsb('addsubr',as,ierr)
c      CALL uclpsb('addsubr',as,ierr)
      errm = 'Error reading/writing add/subtract switch'
      IF(ierr.ne.0) GOTO 999

      CALL uclgsr('addi' ,ai,ierr)
c      CALL uclpsr('addi' ,ai,ierr)
      CALL uclgsr('addb' ,ab,ierr)
c      CALL uclpsr('addb' ,ab,ierr)
      errm = 'Error reading/writing scaling/offset parameters'
      IF(ierr.ne.0) GOTO 999

c Correction switch.

      CALL uclgsb('docor',docor,ierr)
c      CALL uclpsb('docor',docor,ierr)
      errm = 'Error reading/writing correction switch'
      IF(ierr.ne.0) GOTO 999

c Get energy bounds, if any.

c >>> Want ftools type format:  -, min -, - max, min - max.<<<

c >>> Call these only if there is more than one energy channel.<<<

      CALL uclgsi('emin',iopt(1),ierr)
c      CALL uclpsi('emin',iopt(1),ierr)
      CALL uclgsi('emax',iopt(4),ierr)
c      CALL uclpsi('emax',iopt(4),ierr)
      errm = 'Error reading/writing energy channel parameters'
      IF(ierr.ne.0) GOTO 999
c
c method to use for the error.
      err_mode=1
      CALL uclgsi('err_mode',err_mode,ierr)
      errm = 'Error reading/writing err_mode parameters'
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
