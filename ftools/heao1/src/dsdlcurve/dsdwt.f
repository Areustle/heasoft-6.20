**==dsdwt.spg  processed by SPAG 4.50J  at 16:32 on 17 Oct 1998
C DSDWT.FOR
C

C This routine computes the efficiency in the day direction
C It assumes the nominal z-axis position for the present scan

      SUBROUTINE DSDWT(Aday,Stim,Etim,Effprp,Aveff)

C
c ADAY    I            r*4    day when source in FOV
c STIM    I            r*4    start time of present scan
c ETIM    I            r*4    end time for the present scan
c EFFPRP  I            r*4    the inverse of the effective FWHM 
c                             (in days) for the source
c AVEFF   O            r*4    average effficiency for that day


      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL Aday , ahr , Aveff , avtim , bhr , Effprp , Etim , Stim
      INTEGER idy
C*** End of declarations inserted by SPAG

      avtim = (Stim+Etim)/2.
      idy = avtim
      ahr = (avtim-idy)*24.
      bhr = 8.
      IF ( ahr.GT.14. ) bhr = 20.
c
c FOLLOWING CHANGED FROM LT.4 TO LT.2   10-AUG-88
c WOULD WEIGHT SOME SHORT FILES INCORRECTLY
      IF ( ahr.LT.2. ) bhr = -4.
      avtim = idy*1. + bhr/24.
      Aveff = 1. - ABS(Aday-avtim)*Effprp
      IF ( Aveff.LT.0. ) Aveff = 0.
      RETURN
      END
