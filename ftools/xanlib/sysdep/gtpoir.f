
      SUBROUTINE GTPOIR(poinum, num)

      INTEGER num
      REAL poinum(num)

c Function to return NM random Poisson numbers whose means are
c given by XM(NM). Algorithm from Press etal and random number
c generator from CERN.

      REAL PI
      PARAMETER (PI=3.141592654)

      REAL oldm, g, em, t, sq, alxm, y, random, xm
      INTEGER i, seed(2)
      LOGICAL done, first
      character(8) ctime

      REAL gammln
      EXTERNAL gammln

      EQUIVALENCE (ctime, seed(1))

      SAVE first

      DATA first /.true./
      
      alxm = 0.
      g    = 0.
      em = 0.
      t = 0.
      random = 0.
      sq = 0.
      y = 0.
      xm = 0.

c If first call to the routine then initialize random number generator

      IF ( first ) THEN

         CALL gettim(ctime)
         seed(1) = seed(1) + seed(2)
         CALL RLUXGO(3, seed(1), 0, 0)
         first = .false.

      ENDIF

c Loop round array

      oldm = -1
      DO i = 1, num

         xm = poinum(i)

         IF ( xm .LT. 12. ) THEN

            IF ( xm .NE. oldm ) THEN
               oldm = xm
               g = EXP(-xm)
            ENDIF
            em = 0
            CALL ranlux(random, 1)
            t = random
            DO WHILE ( t .GT. g )
               em = em + 1
               CALL ranlux(random, 1)
               t = t * random
            ENDDO

         ELSE

            IF ( xm .NE. oldm ) THEN
               oldm = xm
               sq = SQRT(2.*xm)
               alxm = ALOG(xm)
               g = xm * alxm - GAMMLN(xm+1.)
            ENDIF
            done = .false.
            DO WHILE ( .NOT.done )
               em = -1
               DO WHILE ( em .LT. 0. )
                  CALL ranlux(random, 1)
                  y = TAN( PI*random )
                  em = sq * y + xm
               ENDDO
               IF ( em .LT. 2.15e9 ) em = INT(em)
               t = 0.9 * (1.+y**2) * EXP(em*alxm-GAMMLN(em+1.)-g)
               CALL ranlux(random, 1)
               IF ( random .LE. t ) done = .true.
            ENDDO

         ENDIF

         poinum(i) = em
         oldm = xm

      ENDDO

      RETURN
      END
