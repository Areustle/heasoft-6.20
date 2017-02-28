c
      SUBROUTINE xrapplopt(iopt, mopt, dopt, dtime, dtint, expos, y, sy)
      implicit none
c
c ls           19/7/88 to apply file options to individual points
c ls/la Rev.1  1/3/91  to exclude all but SS and ST options for arr. time 
c                      files
c
c     I   iopt = array of flag options
c     I   mopt = array of math options
c     I   dopt = array of constants for math options
c    I/R  dtime = time of bin center (days)
c    I    dtint = duration of bin (days) (<0 means arriv. time file)
c    I/R  expos = exposure fraction in bin (0->1)
c    I/R  y = cts/s in bin
c    I/R  sy = error on cts/s in bin
c
c  Options consist of 2 chars. and in some cases a numerical value up to
c  8 digits. The maximum no. of chars in one option is 10.
c
c  Note that the array of options copt*10(10) is decoded in
c  a) flag options iopt(10) (0= no= df, 1=yes)
c  b) matemathical options mopt(10) and constants dopt(10) to be executed
c     In the same order as the input string
c
c  List of options supported by this subroutine (only math option so far !!):
c   (see also xrdecopt.for)
c
c  MUx   => mopt(nma) = 10 ; multiply data and errors by x (MUltiply)
c  MDx   => mopt(nma) = 11 ; multiply data by x (Multiply Data)
c  MEx   => mopt(nma) = 12 ; multiply errors by x (Multiply Errors)
c  MAx   => mopt(nma) = 13 ; as MU but divide exposure by x (Multiply All)
c                            (to be able to reconstruct phot. statistics)
c  DIx   => mopt(nma) = 20 ; divide data and errors by x (DIvide)
c  DDx   => mopt(nma) = 21 ; divide data by x (Divide Data)
c  DEx   => mopt(nma) = 22 ; divide  errors by x (Divide  Errors)
c  DAx   => mopt(nma) = 23 ; as DI but multiply exposure by x (Divide All)
c                            (to be able to reconstruct phot. statistics)
c  AAx   => mopt(nma) = 30 ; add data and errors with x (Add All)
c  ADx   => mopt(nma) = 31 ; add data with x (Add Data)
c  AEx   => mopt(nma) = 32 ; add errors with x (Add Errors)
c  QAx   => mopt(nma) = 33 ; add to data the square of data multiplied by x
c                            and sum to errors the product of data and error
c                            multiplied by x. This is used to multiply data
c                            and errors by a linear interpolation constant
c                            y'=y(a+by)=ya+byaya/aa=ya+cyaya with c=b/aa
c                            sy'=sy(a+by)=sya+byasya/aa=sya+cyasya
c                            (sy = error). This can be done by using options:
c                            MUa QAc (where a and c are those used above).
c  QDx   => mopt(nma) = 34 ; as above but for data only
c  QEx   => mopt(nma) = 35 ; as above but for errors only
c
c  SAx   => mopt(nma) = 40 ; subtract data and errors with x (Subtract All)
c  SDx   => mopt(nma) = 41 ; subtract  data with x (Subtract Data)
c  SEx   => mopt(nma) = 42 ; subtract  errors with x (Subtract Errors)
c
c  STx   => mopt(nma) = 50 ; shift times by x days (Shift Times)
c  SSx   => mopt(nma) = 51 ; shift times by x seconds (Shift Seconds)
c
c  Option DV (mopt(nma)=60 is handled by xrrbrdpo, xrrbrdpo2, xrrbrdpo3)
c
c
      INTEGER*4 iopt(*), mopt(*), k
      REAL*4 expos, y, sy
      REAL*8 dopt(*), dtime, dtint
c
c  Loop on all non-null math options
c
      DO k = 1, 15
         IF (mopt(k).EQ.0) RETURN
c
c  Algebraic options
c
c
c !Rev.1 (apply all options only if binned infiles) 
         IF (dtint.gt.0) THEN            
c       multiply options 10,11,12
            IF (mopt(k).EQ.10 .OR. mopt(k).EQ.11 .OR. mopt(k).EQ.13)
     &           y = y*dopt(k)
            IF (mopt(k).EQ.10 .OR. mopt(k).EQ.12 .OR. mopt(k).EQ.13)
     &           sy = sy*dopt(k)
            IF (mopt(k).EQ.13) expos = expos/dopt(k)
c
c       divide options 20,21,22
            IF (mopt(k).EQ.20 .OR. mopt(k).EQ.21 .OR. mopt(k).EQ.23)
     &           y = y/dopt(k)
            IF (mopt(k).EQ.20 .OR. mopt(k).EQ.22 .OR. mopt(k).EQ.23)
     &           sy = sy/dopt(k)
            IF (mopt(k).EQ.23) expos = expos*dopt(k)
c
c       add options 30,31,32
            IF (mopt(k).EQ.30 .OR. mopt(k).EQ.31) y = y + dopt(k)
            IF (mopt(k).EQ.30 .OR. mopt(k).EQ.32) sy = sy + dopt(k)
c
c       add quadratic options 33,34,35
            IF (mopt(k).EQ.33 .OR. mopt(k).EQ.34) y = y + dopt(k)*y*y
            IF (mopt(k).EQ.33 .OR. mopt(k).EQ.35) 
     $           sy = sy + dopt(k)*y*sy
c
c       subtract options 40,41,42
            IF (mopt(k).EQ.40 .OR. mopt(k).EQ.41) y = y - dopt(k)
            IF (mopt(k).EQ.40 .OR. mopt(k).EQ.42) sy = sy - dopt(k)
c       !Rev.1
         ENDIF
c
c  Time options
c
c       shift time (in days) option
         IF (mopt(k).EQ.50) dtime = dtime + dopt(k)
c
c       shift time (in secs) option
         IF (mopt(k).EQ.51) dtime = dtime + dopt(k)/86400.D0
c
      ENDDO
c
      RETURN
      END
c
c
c
