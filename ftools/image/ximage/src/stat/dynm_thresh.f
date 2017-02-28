      SUBROUTINE DYNM_THRESH(Itel,Exposure,Dist,Scrit,Plim)
      IMPLICIT NONE
c
c this subroutine dynamically increases the threshold for source
c acceptance depending on exposure time and off-axis angle
c
c  I  itel     (i)  Telescope index
c  I  exposure (d)  Exposure time
c  I  dist     (r)  Distance of detection from offset
c  O  scrit    (r)  Minimum signal to noise
c  O  plim     (r)  Probability that source is a back fluctuation
c
      integer*4 Itel
      real*8 Exposure
      real*4 Dist, Scrit, Plim

      include '../include/startup.inc'
c
c  Local variables
c
      REAL*4 corrpro , corrfact , timmc , texp , tcrit

c increase threshold dynamically for
      if ( itel.gt.0 ) then
         if ( ZTElescop(itel).eq.'EXOSAT' .and.
     &        ZINstrume(itel)(1:3).eq.'CMA' ) then
            tcrit = 2.0E4
            texp = exposure
            IF ( texp.GE.1.E5 ) texp = 1.E5
            timmc = MAX(texp,tcrit)
            corrpro = timmc/tcrit
            corrfact = SQRT(corrpro)
C minimum acceptable signal-to-noise ratio
            Scrit = 1.3333E-5*Dist*Dist - 2.6667E-3*Dist + 2.5
            Plim = -5.E-3*Dist - 3.0
c added trap for divide by zero, Nick 6/9/93
            IF ( corrpro.GT.0 ) THEN
               Plim = 10.**Plim/corrpro
            ELSE
               Plim = 2.E-4
            ENDIF
            Scrit = corrfact*Scrit
            IF ( Dist.LE.200. ) THEN
               Scrit = 2.5
               Plim = 2.E-4
            ENDIF
            RETURN
         endif
      endif

      corrpro = 1.
      corrfact = 1.
      Scrit = 2.
      Plim = 1.E-4

      RETURN
      END
