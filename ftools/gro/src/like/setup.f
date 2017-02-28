C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C       SUBROUTINE SETUP(iired)
C
C
C  $Id: setup.f,v 1.4 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C*        effect: Assign I/O device numbers, read control data from disk
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED:  BY JRM
C=======================================================================
C  $Log: setup.f,v $
C  Revision 1.4  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.3  2006/04/17 20:54:28  irby
C  Updates for compliance with g95 and gfortran:
C
C  - Replace call to lnblnk with equivalent call to len_trim.
C    lnblnk (a routine which used to live in libg2c) is not currently
C    available with g95 or gfortran.
C
C  - Change calls to "perror" (also libg2c) to fcerr or c_fcerr.
C
C  - Change calls to IDATE (libg2c) to new libgro routine GIDATE.
C
C  - Fix non-integer loop variables.
C
C  Revision 1.2  2002/04/18 19:34:10  irby
C  Bug fixes for OSF build:
C
C  - change "dreal" to acceptable replacement "dble"
C  - changed null string comparisons '' to ' ' - this may not be the best
C    solution.
C  - removed malloc.h from like.h (unnecessary, and not available under Darwin)
C  - Makefile generated using: mkmk version 1.81
C
C  Revision 1.1  2002/04/16 20:27:43  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c MISC_DIR is now read from like.par.  Deleted call to getenv to get this
c variable - Sandhia Bansal 12/01
c
c Revision 5.4  1997/10/02  16:49:01  jae
c Updated code to allow for no frame-no graphic
c UNIX browser 'lynx' to address the proper HTML
c file at http://lheawww.gsfc.nasa.gov/~jae/like/noframe.html
c when lynx is the selected BROWSER for the EGRET Like Users Guide.
c When no BROWSER is selected like defaults to terse single line
c descriptions of the commands.
c
c Revision 5.3  1996/07/26  16:10:53  jae
c Added 'q' to CTL filename input quit
c statement
c
c Revision 5.2  1996/07/19  18:43:02  jae
c Minor changes to assure proper reading of CTL
c filename.
c
c Revision 5.1  1996/02/29  20:53:25  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:56:24  jae
c Subroutine Module for like V5.00
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE SETUP(iired)

C  Common blocks used:
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/errrep.copy'

      save
c
      character(80) id
      INTEGER DATE(3)
      logical iired

      common /id/id

      id = '$Id: setup.f,v 1.4 2013/05/21 19:08:26 irby Exp $'
      LOC='SETUP'


      SIGNAL=' '
      PI180=3.1415926/180.

      if (iired) goto 103

C     Get today's date
      CALL gidate(DATE)
      YEAR=DATE(3)
      MONTH=DATE(2)
      DAY=DATE(1)

C     Assign I/O device numbers
      DO I=1,100
         LU(I)=I
      ENDDO
      LU(1)=6    !unix consule output
      LU(12)=5   !unix consule input

103   CALL CTLRED(iired)

      return
      END
c
