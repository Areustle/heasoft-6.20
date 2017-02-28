C--- PLTSUB.FOR  Contains high level routines called by PLT.
C IOFSET
C PLTGAP
C---
      INTEGER FUNCTION IOFSET(Ivec, Iery, Ngroup, Mxrow)
      INTEGER   Ivec, Iery(*), Ngroup, Mxrow
C---
C Compute offset into the Y data array
C---
C Ivec    I
C Iery    I
C Ngroup  I
C Mxrow   I
C---
C AFT
C---
      INTEGER   i, ix, loc
C---
      ix=MIN(MAX(1,Ivec),Ngroup)
      loc=0
      DO i=1,Ngroup
         IF ( i.EQ.ix ) THEN
            IOFSET=loc
            RETURN
         END IF
         loc=loc+mxrow
         IF ( Iery(i).GT.0 ) loc=loc+Mxrow*Iery(i)
      END DO
      WRITE(*,*) 'ERROR--IOFSET, Unable to find vector',Ivec
      IOFSET = 0
      RETURN
      END
C*********
      SUBROUTINE PLTGAP(Rmini, Rmaxi, Space, Log, Rmino, Rmaxo)
      REAL      Rmini, Rmaxi, Space, Rmino, Rmaxo
      INTEGER   Log
C---
C PLT subroutine to add in the user selected whitespace to the
C data min/max for the plot boundaries
C---
C Rmini     I    Min
C Rmaxi     I    Max
C Space     I    Size of the whiteSpace
C Log       I    Flag, If not zero, then the spacing is Log
C Rmino       O  Min
C Rmaxo       O  Max
C---
C 8-Oct-1988 - rashafer
C---
      REAL tmpmin, tmpmax, delta
C
      IF (Space.EQ.0) THEN
         Rmino = Rmini
         Rmaxo = Rmaxi
         RETURN
      END IF
      tmpmin = Rmini
      tmpmax = Rmaxi
      IF ( (Log.NE.0).AND.(tmpmin.GT.0.).AND.(tmpmax.GT.0.) ) THEN
C** Its log spacing to be done
         delta = tmpmax / tmpmin
         IF ( delta .LE. 1.) THEN
C** they're degenerate, so just allow a single decade
            delta = 3.4
         ELSE
            delta = MAX( delta ** ABS(Space), SQRT(10.5/delta) )
         END IF
         Rmino = tmpmin / delta
         Rmaxo = tmpmax * delta
      ELSE
C** linear spacing
         delta = tmpmax - tmpmin
         IF ( delta .LE. 0.) THEN
            IF (tmpmin .EQ. 0.) THEN
               delta = 0.5
            ELSE
               delta = Space*ABS(tmpmin)
            END IF
         ELSE
            delta = delta*Space
         END IF
         Rmino = tmpmin - delta
         Rmaxo = tmpmax + delta
      END IF
      RETURN
      END
