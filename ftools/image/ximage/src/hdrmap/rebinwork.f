      subroutine rebinwork (Mode, Rebinfac, Map, Szx, Szy,
     &                      Expmap, Datamin, Datamax)
      implicit none
c
c  Rebin image (core routine)
c
c  I  Mode     (i)  Rebin mode
c               0 - smooth the image for the specified rebin
c                   (the image will end with the same size, same pixels 
c                   size too) the count rate in each pixels is obtained 
c                   averaging the counts found in the pixels involved 
c                   in the rebinning (es rebin=2 4pixels ..)
c               1 - this is a "normal" rebin. the pixels and the image 
c                   size change by the rebinning factor
c               2 - the pixels size change but the image size stay the 
c                   original (the image shrink in the frame). use for mosaic
c  I  Rebinfac (i)  Rebin factor
c I/O Map      (i)  Image map
c  I  Szx/Szy  (i)  Size of map
c  I  Expmap   (l)  Whether is an exposure map (preserve time if true)
c  O  Datamin/max (d)  New min/max
c
      INTEGER*4 Mode, Rebinfac, Szx, Szy
      REAL*4 Map(Szx,Szy)
      LOGICAL Expmap
      REAL*8 Datamin, Datamax

      include '../include/maxvals.inc'
c
c  Local variables
c
      INTEGER*4 newszx, newszy, imhx, imhy
      INTEGER*4 nsum
      INTEGER*4 i , j , ii, jj , k , kk , l , ll
      INTEGER*4 in , jn , ixmin , iymin , iymax , ixmax , iyold
      REAL*4 scal , ssum, sum
      REAL*4 rmin, rmax, rnull
      LOGICAL isrnull

c  Initialize to avoid warning
      ssum = 0.
c  --

C
C  rebinning
C
      newszx = Szx/Rebinfac
      newszy = Szy/Rebinfac
      imhx = Szx/2
      imhy = Szy/2
      DO 100 i = 1 , newszx
         DO 50 j = 1 , newszy
            kk = (i-1)*Rebinfac + 1
            ll = (j-1)*Rebinfac + 1
            sum = rnull()
            nsum = 0
            DO 20 k = kk , kk + Rebinfac - 1
               IF ( k.GT.0 .AND. k.LE.Szx ) THEN
                  DO 5 l = ll , ll + Rebinfac - 1
                     IF ( l.GT.0 .AND. l.LE.Szy ) THEN
                        if ( .not.isrnull(Map(k,l)) ) then
                           if ( isrnull(sum) ) then
                              sum = Map(k,l)
                           else
                              sum = sum + Map(k,l)
                           endif
                           nsum = nsum + 1
                        endif
                     ENDIF
 5                CONTINUE
               ENDIF
 20         CONTINUE
            IF ( Mode.EQ.1 .OR. Mode.EQ.2 ) THEN
               Map(i,j) = sum
            ELSE
               scal = Rebinfac*Rebinfac
               DO 30 ii = kk , kk + Rebinfac - 1
                  IF ( ii.GT.0 .AND. ii.LE.Szx ) THEN
                     DO 22 jj = ll , ll + Rebinfac - 1
                        IF ( jj.GT.0 .AND. jj.LE.Szy ) THEN
                           IF ( jj.EQ.ll .AND. ii.EQ.kk ) THEN
                              if ( isrnull(sum) ) then
                                 ssum = sum
                              else
                                 ssum = sum/scal
                              endif
                           ENDIF
                           Map(ii,jj) = ssum
                        ENDIF
 22                  CONTINUE
                  ENDIF
 30            CONTINUE
            ENDIF
            IF ( Expmap ) then
               IF ( nsum.GT.0 ) THEN
                  Map(i,j) = sum/float(nsum)
               ELSE
                  Map(i,j) = 0.0
               ENDIF
            ENDIF
 50      CONTINUE
 100  CONTINUE
      IF ( Mode.EQ.1 .OR. Mode.EQ.2 ) THEN
         in = Szx/Rebinfac
         jn = Szy/Rebinfac
         DO 150 i = 1 , Szx
            DO 120 j = 1 , Szy
               IF ( i.GT.in .OR. j.GT.jn ) Map(i,j) = rnull()
 120        CONTINUE
 150     CONTINUE
      ENDIF
      IF ( Mode.EQ.2 ) THEN
         ixmin = imhx - imhx/Rebinfac
         iymin = imhy - imhy/Rebinfac
         ixmax = ixmin + Szx/Rebinfac
         iymax = iymin + Szy/Rebinfac
         DO 200 j = iymax , iymin + 1 , -1
            iyold = Szy/Rebinfac + j - iymax
            DO 160 i = ixmin + 1 , ixmax
               Map(i,j) = Map(i-ixmin+1,iyold)
               Map(i-ixmin,iyold) = rnull()
 160        CONTINUE
 200     CONTINUE
      ENDIF
c
c  Find min/max
c
      rmin = rnull()
      rmax = rnull()

      do i = 1, Szx
         do j = 1, Szy
            if ( .not.isrnull(Map(i,j)) ) then
               if ( isrnull(rmin) .or. Map(i,j).lt.rmin )
     &            rmin = Map(i,j)
               if ( isrnull(rmax) .or. Map(i,j).gt.rmax )
     &            rmax = Map(i,j)
            endif
         enddo
      enddo
c
c   Set min/max to zero for all-null image
c
      if ( isrnull(rmin) .and. isrnull(rmax) ) then
         rmin = 0
         rmax = 0
      endif

      Datamin = rmin
      Datamax = rmax

      RETURN
      END
