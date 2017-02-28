CHCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CH1 convrt.f
CH1 $Id: convrt.f,v 1.3 2013/05/21 19:08:24 irby Exp $
CH1 VERSION:  1.00       REVISION DATE: 10/29/84
CH1
CH1 ROUTINE NAME:
CH1    CONVRT
CH1
CH1 ENGLISH NAME:
CH1    CONVERT
CH1
CH1 SYSTEM AND SPACE CRAFT:
CH1    EGRET PDBGEN
CH1
CH1 LANGUAGE AND MACHINE:
CH1    VS FORTRAN, IBM 3081 MVS
CH
CH2 FUNCTION:
CH2    CONVERTS A TRUNCATED JULIAN DATE AND MILLISECOND OF DAY TO
CH2    A COMBINED REAL NUMBER
CH
CH3 FORTRAN CALLING SEQUENCE:
CH3    TIME = CONVRT(TJD,MSD)
CH3
CH3    VARIABLE         TYPE            I/O          DESCRIPTION
CH3    --------         ----            ---    ---------------------
CH3    TJD             INTEGER           I     TRUNCATED JULIAN DATE
CH3    MSD             INTEGER           I     MILLISECOND OF DAY
CH3
CH4 COMMON:
CH4  N.A.
CH4
CH4 SIGNIFICANT VARIABLES:
CH4    VARIABLE    TYPE      INT. VAL.               DESCRIPTION
CH4    N.A.
CH
CH5 SUBROUTINES CALLED:
CH5    NONE
CH
CH6 RESTRICTIONS:
CH6    NONE
CH6
CH6 ERROR HANDLING:
CH6    N.A.
CH
CH7 METHOD:
CH7   CONVRT = DFLOAT(TJD) + DFLOAT(MSD)/86400000DO
CH7  END CONVRT
CH7
CH7 REFERENCES:
CH7    N.A.
CH
CH8 PROGRAMMER AND DATE:
CH8    ALBERT ETIENNE           S.A.R.               08/14/84
CH8    E.S.Panduranga		S.T.X.		     09/15/91
CH9 MODIFICATIONS:
CH9 --------------------------------------------------------------------------
CH9 $Log: convrt.f,v $
CH9 Revision 1.3  2013/05/21 19:08:24  irby
CH9 Change character*n to character(n) to silence warnings: "Obsolescent
CH9 feature: Old-style character length".
CH9
CH9 Revision 1.2  1998/10/06 12:52:21  silvis
CH9 A stop was removed from the main program so that the learn feature
CH9 for the par file would work properly and some minor changes were made
CH9 to the par file.
CH9
c Revision 2.1  1991/10/08  21:48:29  esp
c First controlled version on the SUN.
c
CH9 --------------------------------------------------------------------------
CHCCCCCCCCCCCCCCCCCCCCCCCCCCCCC PROGRAM CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DOUBLE PRECISION FUNCTION CONVRT(TJD,MSD)

      INTEGER*4 TJD,MSD

      character(80)	rcsid
      common	/id/	rcsid
      rcsid = '$Id: convrt.f,v 1.3 2013/05/21 19:08:24 irby Exp $'

      CONVRT = DFLOAT(TJD) + DFLOAT(MSD)/86400000D0
      RETURN

CHCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      END
