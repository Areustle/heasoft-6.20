      subroutine rotatework(Roll1, Outrol, Map, Work, Row, Szx, Szy,
     &                      Maptype,Datamin,Datamax)
      implicit none
c
c  Core rotate image routine
c
c  I  Roll1   (d)  Original roll angle
c  I  Outrol  (d)  New roll angle
c I/O Map     (r)  Image map
c I/O Work    (r)  Temporary work map
c I/O Row     (r)  Temporary row buffer
c  I  Szx/y   (i)  Size of maps
c  I  Maptype (s)  Map data type (I=integer R=real)
c  O  Datamin (d)  Minimum value
c  O  Datamax (d)  Maximum value
c
      real*8 Roll1, Outrol
      integer*4 Szx, Szy
      real*4 Map(Szx,Szy), Work(Szx,Szy), Row(Szx)
      character*(*) Maptype
      real*8 Datamin, Datamax

      include '../include/pi.inc'
c
c  Local variables
c
c     real*4 ximh , yimh
      INTEGER*4 ximh , yimh
      INTEGER*4 ixnew, iynew
      INTEGER*4 i, j, iy, ix, iix, iiy
      REAL*4 pixpro(4), gamma1, npix(4), rmin, rmax, rnull
      REAL*4 xnew , ynew , fracx , fracy
      REAL*8 radian
      INTEGER*4 seed
      logical isrnull

      radian = 180.d0/PI
c
c  initialize seed using current date and time
c
      CALL GETSEED(seed)
     
      Roll1 = -Roll1 + 270.
      Outrol = -Outrol + 270.
      gamma1 = (Roll1-Outrol)/radian

      do i = 1, Szx
         do j = 1, Szy
            Work(i,j) = rnull()
         enddo
      enddo

      ximh = Szx/2
      yimh = Szy/2

      DO 300 iy = 1 , Szy
         DO iix = 1 , Szx
            Row(iix) = Map(iix,iy)
         ENDDO
         IF ( iy.EQ.1 ) THEN
            DO iix = 1 , Szx
               Row(iix) = rnull()
            ENDDO
         ENDIF
         DO 200 ix = 1 , Szx
            iix = ix - ximh
            iiy = iy - yimh
            xnew = FLOAT(iix)*COS(gamma1) - FLOAT(iiy)*SIN(gamma1)
     &             + FLOAT(ximh)
            ynew = FLOAT(iix)*SIN(gamma1) + FLOAT(iiy)*COS(gamma1)
     &             + FLOAT(yimh)
            ixnew = int(xnew)
            iynew = int(ynew)
            IF ( ixnew.GT.0 .AND. ixnew.LT.Szx ) THEN
               IF ( iynew.GT.0 .AND. iynew.LT.Szy ) THEN
                  IF ( .not.isrnull(Row(ix)) ) THEN
                     fracx = xnew - float(ixnew)
                     fracy = ynew - float(iynew)
                     pixpro(1) = (1.-fracx)*(1.-fracy)
                     pixpro(2) = fracx*(1.-fracy)
                     pixpro(3) = fracx*fracy
                     pixpro(4) = 1. - pixpro(1) - pixpro(2) - pixpro(3)
                     CALL DISRO(pixpro,Row(ix),4,seed,Maptype,npix)
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

      rmin = rnull()
      rmax = rnull()
      do i = 1, Szx
         do j = 1, Szy
            Map(i,j) = Work(i,j)
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
