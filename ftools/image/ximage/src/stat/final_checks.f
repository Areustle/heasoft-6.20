      SUBROUTINE FINAL_CHECKS(Itel,Xpix,Ypix,Stonr,Scrit,Plim,Dist)
      IMPLICIT NONE
c
c  I  itel    (i)  Telescope index
c  I  x/ypix  (r)  Detection in original pixel coordinates
c  I  stonr   (r)  Signal to noise ratio
c  I  scrit   (r)  Minimum signal to noise
c  I  plim    (r)  Probability that source is a back fluctuation
c  I  dist    (r)  Distance of detection from offset
c
      integer*4 Itel
      real*4 Xpix, Ypix, Stonr, Plim, Scrit, Dist

      include '../include/io.inc'
      include '../include/startup.inc'
      include 'detect.inc'
c
c  Local variables
c
      logical cma
      REAL*4 dhot 

      write(ZWRite,*) NUMdet, ' - S/N ', Stonr, ' Prob ', PROb(NUMdet)
      call xwrite(ZWRite, 20)

      cma = .FALSE.
      if ( itel.gt.0 ) then
         if ( ZTElescop(itel).eq.'EXOSAT' .and.
     &        ZINstrume(itel)(1:3).eq.'CMA' ) then
            cma = .TRUE.
         endif
      endif

      IF ( .not.cma ) THEN
         IF ( NUMdet.EQ.1 ) THEN
            WRITE (ZWRite,*) ' snr threshold = ' , Scrit
            CALL XWRITE(ZWRite,10)
            WRITE (ZWRite,*) ' bgnd fluctuation probability limit = ' , 
     &                       Plim
            CALL XWRITE(ZWRite,10)
         ENDIF
         IF ( Stonr.LT.Scrit .OR. PROb(NUMdet).GT.Plim ) THEN
c        -> Source rejected  
            HOT(NUMdet) = 0
         ELSE
c        -> Source accepted  
            HOT(NUMdet) = 1
         ENDIF
      ELSEIF ( Stonr.LT.Scrit .OR. PROb(NUMdet).GT.Plim .OR. 
     &         Dist.GT.800. ) THEN
         HOT(NUMdet) = 0
      ELSE
         dhot = 0.
         if ( ZINstrume(itel).eq.'CMA1' ) then
            dhot = SQRT((Xpix-155.)**2.+(Ypix-71.)**2.)
         elseif ( ZINstrume(itel).eq.'CMA2' ) then
            dhot = SQRT((Xpix+11.)**2.+(Ypix+20.)**2.)
         endif
         IF ( dhot.LT.20 ) THEN
            HOT(NUMdet) = 2
         ELSE
            HOT(NUMdet) = 1
         ENDIF
      ENDIF
      RETURN
      END
