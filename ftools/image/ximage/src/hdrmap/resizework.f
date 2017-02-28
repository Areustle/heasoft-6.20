      subroutine resizework(Pixratio,Map,Work,Row,Szx,Szy,Maptype,
     &                      Datamin,Datamax)
      implicit none
c
c  Resize image (core routine)
c
c  I  Pixratio (r)  Pixel ratio (new/old)
c I/O Map      (r)  Image map
c I/O Work     (r)  Work map
c I/O Row      (r)  Row buffer
c I/O Szx/Szy  (i)  Size of maps
c  I  Maptype  (s)  Map data type (I=integer R=real)
c  O  Datamin  (d)  Minimum of resized map
c  O  Datamax  (d)  Maximum of resized map
c
      REAL*4 Pixratio
      INTEGER*4 Szx, Szy
      REAL*4 Map(Szx,Szy), Work(Szx,Szy), Row(Szx)
      CHARACTER*(*) Maptype
      REAL*8 Datamin, Datamax
c
c  Local variables
c
      INTEGER*4 i , j , ix , iy , iix
      INTEGER*4 ixnew , iynew 
      REAL*4 pixpro(4), npix(4), rnull, rmin, rmax
      REAL*4 xnew , ynew , fracx , fracy
      INTEGER*4 seed
      LOGICAL isrnull
c
c  initialize seed using current date and time
c
      CALL GETSEED(seed)
c
c  initialize working array
c
      do i = 1, Szx
         do j = 1, Szy
            Work(i,j) = rnull()
         enddo
      enddo
c
      DO 300 iy = 1, Szy
         do iix = 1, Szx
            Row(iix) = Map(iix,iy)
         enddo
         if ( iy.eq.1 ) then
            do iix = 1, Szx
               Row(iix) = rnull()
            enddo
         endif
         DO 200 ix = 1 , Szx
            xnew = FLOAT(ix)/pixratio - pixratio + 1.
            ynew = FLOAT(iy)/pixratio - pixratio + 1.
            ixnew = xnew
            iynew = ynew
            IF ( ixnew.GT.0 .AND. ixnew.LT.Szx ) THEN
               IF ( iynew.GT.0 .AND. iynew.LT.Szy ) THEN
                  IF ( .not.isrnull(Row(ix)) ) THEN
                     fracx = 1. - (xnew-FLOAT(ixnew))*pixratio
                     fracy = 1. - (ynew-FLOAT(iynew))*pixratio
                     pixpro(1) = fracx*fracy
                     pixpro(2) = fracy*(1.-fracx)
                     pixpro(3) = (1.-fracx)*(1.-fracy)
                     pixpro(4) = 1. - pixpro(1) - pixpro(2) - pixpro(3)
                     CALL DISRO(pixpro,Row(ix),4,seed,maptype,npix)
                     IF ( ixnew.LT.Szx ) THEN
                        IF ( iynew.GE.Szy ) GOTO 200
                        if ( isrnull(Work(ixnew,iynew)) ) then
                           Work(ixnew,iynew) = npix(1)
                        else
                           Work(ixnew,iynew) = Work(ixnew,iynew)
     &                                         + npix(1)
                        endif
                        if ( isrnull(Work(ixnew+1,iynew)) ) then
                           Work(ixnew+1,iynew) = npix(2)
                        else
                           Work(ixnew+1,iynew) = Work(ixnew+1,iynew)
     &                                           + npix(2)
                        endif
                        if ( isrnull(Work(ixnew+1,iynew+1)) ) then
                           Work(ixnew+1,iynew+1) = npix(3)
                        else
                           Work(ixnew+1,iynew+1) = Work(ixnew+1,iynew+1)
     &                                           + npix(3)
                        endif
                     ENDIF
                     IF ( iynew.LT.Szy ) then
                        if ( isrnull(Work(ixnew,iynew+1)) ) then
                           Work(ixnew,iynew+1) = npix(4)
                        else
                           Work(ixnew,iynew+1) = Work(ixnew,iynew+1)
     &                                           + npix(4)
                        endif
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
 200     CONTINUE
 300  CONTINUE
c
c  Find min/max
c
      rmin = rnull()
      rmax = rnull()

      do i = 1, Szx
         do j = 1, Szy
            if ( .not.isrnull(Work(i,j)) ) then
               if ( isrnull(rmin) .or. Work(i,j).lt.rmin )
     &            rmin = Work(i,j)
               if ( isrnull(rmax) .or. Work(i,j).gt.rmax )
     &            rmax = Work(i,j)
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

      return
      end
