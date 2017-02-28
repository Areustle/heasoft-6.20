c
      subroutine lcmath

c apply BacKGround subtraction to binned data.

c This program accepts two FITS files as input:
c  File1, or an 'input file' and
c  File2, or a 'background file'.
c However, the program is more general in that it can either add
c or subtract the light curves in the 2 files, and the two files
c need not actually contain "source" and "background" data.  In either case the
c integration time in File1 should be less than or equal to that in
c File2.  Also, the way the data are stored in the column containing
c the count rate should be the same in both files.  For example, if
c File1 has a RATE column with 128 energy channels stored as a
c vector, so should File2.

c Lcmath acts on one binary extension and one rate column only.

c Lcmath creates a new output file which is a copy of File1 with the
c original light curve replaced by the net light curve.

c Lcmath expects the input and background file to contain binned data,
c not an event list.

c The user may scale and/or add an offset to either or both input light curves.

c The net light curve can contain the sum of any number of contiguous energy
c channels listed in the data.  The input light curves may be 1-D
c vectors of separate energy channels, but should contain only
c one time per row.

c By default, lcmath will apply to the net light curve any vignetting or 
c deadtime correction information contained in the input file.
c The user may cancel this correction.

c Relevant subroutines: (see for more detailed information)

c    lcmathinit   Reads input parameters using XPI
c    lcmathmake   Houses the main body of the program, gathers file header
c              information
c    lcmathopen   Opens and performs checks on the input files
c    lcmathcopy   Copies File1 to the output file, making any necessary
c              changes to the format of the relevent extension.  
c              Any other extensions or images are copied verbatim.
c    lcmathread   Reads count rate data from File1, 
c              adds or subtracts File2, applies scaling and corrections
c    lcmathrdbkg  Reads count rate data from File2
c    lcmathwrite  Writes the net count rate and error to the output file
c Author:  eal  February 1994, NASA/Goddard Space Flight Center
c
c Aug 1996 - insert new paramter for error calculation august  1996
c Dec 1996 - relax the overlap start and stop checking for the input
c            source and background lightcurves
c          - delete the BACKAPP keywords before writing  
c          - update the CHECKSUM and DATASUM before closing  
c Feb 1997 - change the test for the error column in lcmathread
c            It was always recalculating the error from rate (count)
c            which was still correct if err=sqrt(rate*exp)
c
      IMPLICIT NONE
c      INCLUDE 'lcmathdef.inc'
c
      LOGICAL as,docor
      character(80) istring
      character(256) in_fil,bg_fil,ou_fil
      INTEGER*4 tchat,lchat,ierr,parse,iopt(15),err_mode
      REAL mi,mb,ai,ab

C          task common
      character(40) taskname
      common /task/ taskname


      DATA istring,parse,iopt /' ',0,15*0/

c Get parameters.

      call xrversion('lcmath', taskname)
      ierr = 0

      CALL lcmathinit(in_fil,bg_fil,ou_fil,tchat,lchat,mi,ai,mb,ab,as
     &            ,docor,iopt,err_mode,ierr)

      IF(ierr.eq.0) THEN

c Set chattiness.

         CALL xchaty(tchat,lchat)

c Open log file. (The '+' is necessary to open append.)
C Use environment variable to enable logging

c         IF(lchat.ge.tchat) THEN
c            log_fil = '+' // program(:lenact(program)) // '.log'
c            CALL setlog(istring,parse,log_fil,' ')
c         ENDIF

c Execute program.

         CALL lcmathmake(in_fil,bg_fil,ou_fil,mi,ai,mb,ab,as,docor,iopt
     &               ,err_mode,ierr)

      ENDIF

      END
