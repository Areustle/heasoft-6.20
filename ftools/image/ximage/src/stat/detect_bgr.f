      subroutine detect_bgr(Map, Szx, Szy, Mapid, Status)
      implicit none
c
c calculates background in map
c
c  Note on NULLs: The barylim test relies on the following to
c                 be true: NINT(rnull()).EQ.0 
c
c  I  map        (r)  Image map
c  I  szx/y      (i)  Size of image map
c  I  mapid      (s)  Map id string
c  O  status     (i)  Error flag  (0 = OK)
c
      INTEGER*4 Szx, Szy, Status
      REAL*4 Map(Szx,Szy)
      CHARACTER*(*) Mapid

      INCLUDE '../include/io.inc'
      INCLUDE '../include/maxvals.inc'
      INCLUDE 'backgd.inc'
c
c  Local variables
c
      INTEGER*4 ibo , ibackk , isl , imin2 , im , imax1 , 
     &          imin1 , iflag
      INTEGER*4 i , j , imax2 , ii , jk , ibak , ijk
      INTEGER*4 min21 , min22 , max21 , max22
      INTEGER*4 jm , isj , x_center , y_center
      real*4 probx, proby
      REAL*4 su , w , bama , bima
      REAL*4 batot , umbox , bamin , bamax , bbb , x_bar , y_bar , total
      REAL*4 dist , c1 , c2 , sum1 , sum2 , sum3 , XPOLOG , is2 , dif0
      REAL*4 dif5 , pback0 , pback5 , dif , chi , percentage , var
      REAL*4 sig , esig , step , range , auppb , ajkm , baint3 , 
     &       tt
      REAL*4 amenn , threl , qel , a1 , thrsig , threh , asum, histavg
      INTEGER*4 badist(11), b1max
      INTEGER tr, iasum, color
      LOGICAL isrnull

      character*(MAX_IDSTR) bgmapid
      integer*4 di
      real*4 ximg, yimg, xpix, ypix
      real*8 xcen, ycen, dxoff, dyoff, zmx, zmy, bgzmx, bgzmy
      integer*4 nhist
      parameter(nhist = 128)
      integer*4 ibi, ibu(nhist)
      real*4 ppp0, ppp5

      character(10) bstr
      integer blen, tchat, lchat

      call xgtcht(tchat, lchat)
c
      bgmapid = 'BGMAP'

      call get_refram (mapid, di, di, zmx, zmy, xcen, ycen, Status)
      if ( Status.ne.0 ) return
      call get_refram (bgmapid, di, di, bgzmx, bgzmy, dxoff, dyoff, 
     &                 Status)
      if ( Status.ne.0 ) return
      
      Status = 0
      IZEro = 0
c  Initialize to avoid warning
      bima = 0.
      bbb = 0.
      step = 0.
c  --

      IF ( IBBac.EQ.0 ) THEN
         call XWRITE(' Background boxsize = 0', 5)
         call XWRITE(' ibbac = 0', 15)
         Status = 1
         RETURN
      ENDIF

      NBX = Szx/IBBac
      NBY = Szy/IBBac
      NBOxes = NBX*NBY

      write(ZWRite,*) ' Total no. of boxes: ', NBOxes, ', ',
     &                 NBX, ' boxes x ', NBY, ' boxes'
      call RMVXBK(ZWRITE(2:))
      call XWRITE(ZWRite, 20)

      IF ( NBOxes.GT.MAXBOX ) THEN
         WRITE (ZWRite,99001) NBOxes , MAXBOX
         CALL XWRITE(ZWRite,10)
         CALL XWRITE('       Try a bigger box size!',10)
         Status = 1
         RETURN
      ENDIF
      IF ( NBOxes.LE.4 ) THEN
         CALL XWRITE(' 4 or less bgnd boxes, try a smaller box size',10)
         Status = 1
         RETURN
      ENDIF
      DO 100 ibo = 1 , NBOxes
         BB(ibo) = 0.
         BB_flags(ibo) = 0
 100  CONTINUE
      ibackk = 0
      batot = 0.
      umbox = 0.
C starting loop over boxes for background
      bamin = 4.E4
      bamax = 0.
      if ( DRAw_boxes ) call pgbbuf
      DO 300 ibo = 1 , NBOxes
         iflag = 0
         DO 150 isl = 1 , nhist
            ibu(isl) = 0
 150     CONTINUE
         im = (ibo-1)/NBX
         imin1 = (ibo-im*NBX-1)*IBBac + 1
         imin2 = im*IBBac + 1
         imax1 = imin1 + IBBac - 1
         imax2 = imin2 + IBBac - 1
         ximg = float(imin1+imax1)/2.
         yimg = float(imin2+imax2)/2.
         call calimgpix(Szx,Szy,zmx,zmy,xcen,ycen,c1,c2,
     &                  ximg,yimg,1)
         bbb = float(IBBac)*zmx/2.
         IF ( DRAw_boxes ) then
            if ( tchat.ge.20 ) then
c              White : All boxes (For debugging purposes)
               call jrnbox(c1,c2,bbb*2.0,bbb*2.0,0.,1,-1,-1)
               call xistr(ibo,bstr,blen)
               call jrnlab(c1,c2,bstr,' ',-1,0.6,-1,-1,-1,-1.0,
     &                     -1,'center',0.0)
            endif
         endif
c
c  Box half test: compare dist of left side to right and top to bottom
c    More effective than barycenter rejection criteria in diffuse,
c    low count situations
c
         if ( BXHprob.gt.0 ) then
            call bxhtest(Map, Szx, Szy, imin1, imax1, imin2, imax2,
     &                   probx, proby, status)
            if ( status.ne.0 ) then
               if ( DRAw_boxes ) call pgebuf
               return
            endif
            write(ZWRite,*) ' ',ibo,' - Box half test: ', probx, proby
            call xwrite(ZWRite, 25)
            if ( probx.lt.BXHprob .or. proby.lt.BXHprob ) then
               write(ZWRite,*) ' ',ibo, ' - Box half match prob > ',
     &                         BXHprob
               call xwrite(ZWRite, 20)
               BB(ibo) = -1.
               BB_flags(ibo) = 4
               umbox = umbox + 1
               IF ( DRAw_boxes ) then
                  color = 2
c                 Red : Box halves don't agree
                  call jrnbox(c1,c2,bbb*2.,bbb*2.,0.,color,-1,-1)
               ENDIF
            endif
         endif
         IF ( BB(ibo).GT.-0.1 ) THEN
c
C calculating background in ith box
c *****************zeros**********************
            x_bar = 0.
            y_bar = 0.
            total = 0.
            asum = 0.
            iasum = 0
c ***************end zeros********************
            DO 200 i = imin1 , imax1
               DO 180 j = imin2 , imax2
                  if ( .not.isrnull(Map(i,j)) ) then
                     ii = nint(Map(i,j)) + 1
                     if ( ii.le.nhist .and. ii.gt.0 ) then
                        ibu(ii) = ibu(ii) + 1
                     endif
 
c check for pixels > nhist cts/pix,
                     asum = asum + nint(Map(i,j))
                     iasum = iasum + 1
                  endif
c
C *****************calculate baricenter*******************
                  x_bar = x_bar + nint(Map(i,j))*(i-imin1)
                  y_bar = y_bar + nint(Map(i,j))*(j-imin2)
                  total = total + nint(Map(i,j))
C *******************end baricenter***********************
 180           CONTINUE
 200        CONTINUE
c *****************check baricenter position**************
            IF ( total.GT.0. ) THEN
               x_bar = x_bar/total
               y_bar = y_bar/total
               x_center = (imax1-imin1)/2
               y_center = (imax2-imin2)/2
               dist = SQRT((x_bar-x_center)**2+(y_bar-y_center)**2)
     &             /(imax1-imin1)
               write(ZWRite,*) ' ',ibo,' - Barycentric distance: ',dist
               call xwrite(ZWRite, 25)
c              if ( abs(x_bar-x_center)/(imax1-imin1).gt.BARylim .or.
c    &              abs(y_bar-y_center)/(imax1-imin1).gt.BARylim ) then
               IF ( dist.GT.BARylim ) THEN
                  write(ZWRite,*) ' ',ibo, ' - Barycenter > ',
     &                            BARylim, ' from center'
                  call xwrite(ZWRite, 20)
                  BB(ibo) = -1.
                  BB_flags(ibo) = 1
                  umbox = umbox + 1
                  IF ( DRAw_boxes ) then
                     color = 5
c                    Light blue : Barycenter > BARylim from center
                     call jrnbox(c1,c2,bbb*2.,bbb*2.,0.,color,-1,-1)
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
 
         IF ( BB(ibo).GT.-0.1 ) THEN
c *******************end check****************************
C new position
            tr = nhist
            DO WHILE ( .TRUE. )
               sum1 = 0.
               sum2 = 0.
               sum3 = 0.
 
               DO i = 1 , nhist
                  sum3 = sum3 + ibu(i)
                  if ( i.le.tr ) then
                     ibi = (i-1)*ibu(i)
                     sum1 = sum1 + ibi
                     sum2 = sum2 + ibu(i)
                  endif
               ENDDO
               IF ( sum2.GT.0 ) THEN
                  BACk = sum1/sum2
               ELSE
                  BACk = 0.0
               ENDIF
               write(ZWRite,*) ' ',ibo,' - Hist count/pix sum: ',
     &                         sum1, sum2
               call xwrite(ZWRite, 25)
               write(ZWRite,*) ' ',ibo,' - Box count/pix sum: ',
     &                         asum, iasum
               call xwrite(ZWRite, 25)
               histavg = asum
               if ( iasum.ne.0 ) histavg = asum/iasum
               IF ( BACk.NE.histavg ) THEN
c
C reject boxes where total counts greater than those in distribution
c ((bright sources)) 
                  write(ZWRite,*) ' ',ibo,
     &                            ' - Total box .ne. total histogram'
                  call xwrite(ZWRite, 20)
                  IF ( DRAw_boxes ) THEN
                     color = 8
c                    Orange : total counts .ne. (128) distribution
                     call jrnbox(c1,c2,bbb*2.,bbb*2.,0.,color,-1,-1)
                  ENDIF
                  BB(ibo) = -1.
                  BB_flags(ibo) = 1
                  umbox = umbox + 1
               ENDIF
  220          CONTINUE
               IF ( BB(ibo).GT.-0.1 ) THEN
 
C test on abundance of zeroes
c (only for poissonian background)
                 IF ( iflag.eq.1 ) then
                  IF ( POIsson ) THEN
                     IF ( sum3.GT.0 ) THEN
                        pback0 = ibu(1)/sum3
                        pback5 = ibu(6)/sum3
                     ELSE
                        pback0 = 0
                        pback5 = 0
                     ENDIF
                     ppp0 = XPOLOG(0,BACk,2)
                     IF ( BACk.GT.0.0 ) THEN
                        ppp5 = XPOLOG(5,BACk,2)
                     ELSE
                        ppp5 = 0.
                     ENDIF
                     is2 = IBBac*IBBac
                     write(ZWRite,*) ' ',ibo,
     &                         ' - Actual background dist (0 & 5)',
     &                             pback0, pback5
                     call XWRITE(ZWRite, 25)
                     write(ZWRite,*) ' ',ibo,
     &                      ' - Estimated background dist (0 & 5)',
     &                          ppp0, ppp5
                     call XWRITE(ZWRite, 25)
                     dif0 = pback0 - ppp0
                     dif5 = pback5 - ppp5
                     dif = dif0
                     IF ( dif5.GT.dif0 ) dif = dif5
                     chi = 4*dif*dif*is2
                     IF ( dif.LE.0. ) chi = 0.
                     write(ZWRite,*) ' ',ibo,
     &                   ' - Chi, avg background ', chi, BACk
                     call XWRITE(ZWRite, 25)
c  Uncomment to emulate bug which ignores first box from old version
c                    IF ( chi.GE.9.0 .OR. BACk.EQ.0.
c    &                     .or. ibo.eq.1 ) THEN
                     IF ( chi.GE.9.0 .OR. BACk.EQ.0. ) THEN
C reject boxes with excess of zeroes  (blank areas)
c 
                        write(ZWRite,*) ' ',ibo,
     &                      ' - Non-poissonian stats (Chi>9 or Back=0)'
                        call xwrite(ZWRite, 20)
                        IF ( DRAw_boxes ) THEN
                           color = 7
c                          Yellow : Non-poissonian stats
                           call jrnbox(c1,c2,bbb*2.,bbb*2.,0.,
     &                                 color,-1,-1)
                        ENDIF
                        BB(ibo) = -1.
                        BB_flags(ibo) = 2
                        umbox = umbox + 1
                     ELSE
                        ibackk = ibackk + 1
                        batot = batot + BACk
                        BB(ibo) = BACk
                        IF ( sum2.GT.0 ) THEN
                           BB_sig(ibo) = SQRT(sum1)/sum2
                        ELSE
                           BB_sig(ibo) = 0
                        ENDIF
                        BB_npix(ibo) = sum2
                        IF ( BACk.LT.bamin ) bamin = BACk
                        IF ( BACk.GT.bamax ) bamax = BACk
                     ENDIF
                  ELSE
                     ibackk = ibackk + 1
                     batot = batot + BACk
                     BB(ibo) = BACk
                     IF ( BACk.LT.bamin ) bamin = BACk
                     IF ( BACk.GT.bamax ) bamax = BACk
                  ENDIF
                 ELSE
                    tr = 1 + 6.*SQRT(BACk)
                    if ( tr.lt.5 ) tr = 5
                    iflag = 1
                    goto 220
                 ENDIF
               ENDIF
               GOTO 300
            ENDDO
         ENDIF
 300  CONTINUE
      if ( DRAw_boxes ) call pgebuf

      percentage = umbox/FLOAT(NBOxes)
      IF ( percentage.GT..8 ) THEN
         CALL XWRITE(' Too many (>80%) background boxes rejected ',10)
         Status = 1
         RETURN
      ENDIF

      IF ( ibackk.LE.1 ) THEN
         CALL XERROR(' Too few background boxes',5)
         write (ZWRite, *) ' ibackk = ', ibackk
         call XWRITE (ZWRite, 15)
         Status = 1
         RETURN
      ENDIF

      BACk = batot/FLOAT(ibackk)
      write(ZWRite,*) ' Avg background (cnts/image pix): ', BACk
      call xwrite(ZWRite, 20)

C calculating statistics of background boxes
      var = 0.
      DO 400 i = 1 , NBOxes
         IF ( BB(i).GT.-0.1 ) var = var + (BB(i)-BACk)**2.
 400  CONTINUE
      var = var/float(ibackk - 1)
      IF ( var.EQ.0 ) call XWRITE(' var = 0', 15)
      sig = SQRT(var)
c   BACK (cts/image pix)
      esig = SQRT(BACk)/FLOAT(IBBac)
      thrsig = SIGmult*esig
      write(ZWRite,*) ' Sigma multiplier: ', SIGmult
      call xwrite(ZWRite, 25)
      write(ZWRite,*) ' Sigma (sqrt(Variance)): ', sig
      call xwrite(ZWRite, 25)
      write(ZWRite,*) ' Sigma(e?) (sqrt(Back)/box width): ', esig
      call xwrite(ZWRite, 25)
      write(ZWRite,*) ' Threshold sigma (Sigma(e?)*Sigmult): ', thrsig
      call xwrite(ZWRite, 25)
      IF ( sig.GT.thrsig ) THEN
         write(ZWRite, *) ' Background min/max: ', bamin, bamax
         call xwrite(ZWRite, 25)
         range = bamax - bamin
         step = range/10.
c
c added check for bb(jk) is -1 6/17/93 (Nick)
c
         do i = 1, 11
            badist(i) = 0
         enddo
         DO 450 jk = 1 , NBOxes
            IF ( BB(jk).GT.-0.1 ) THEN
               auppb = bamin
               ibak = 0
               DO WHILE ( .TRUE. )
                  ibak = ibak + 1
                  auppb = auppb + step
                  IF ( BB(jk).LT.auppb ) THEN
                     badist(ibak) = badist(ibak) + 1
                     GOTO 450
                  ENDIF
               ENDDO
            ENDIF
 450     CONTINUE
         b1max = 0
         DO 500 ijk = 1 , 11
            write(ZWRite, *) ' Back histogram: ', 
     &                         bamin+step*float(ijk-1), badist(ijk)
            call xwrite(ZWRite, 25)
            IF ( badist(ijk).GT.b1max ) THEN
               b1max = badist(ijk)
               ajkm = ijk
            ENDIF
 500     CONTINUE
         amenn = bamin + ajkm*step
         write(ZWRite, *) ' Max bin index: ', ajkm
         call xwrite(ZWRite, 25)
         write(ZWRite, *) ' Max bin no. of boxes: ', b1max
         call xwrite(ZWRite, 25)
         write(ZWRite, *) ' Upper edge of max bin: ', amenn
         call xwrite(ZWRite, 25)
         tt = 20.*SQRT(amenn)/FLOAT(IBBac)
c original code (middle of next bin over from max)
         threl = bamin + (ajkm+0.5)*step - tt
c actual middle of max bin
c        threl = bamin + (ajkm-0.5)*step - tt
         qel = bamin + 2.5*step
         baint3 = badist(1) + badist(2) + badist(3)
         write(ZWRite,*) ' tt (20*sqrt(Upper max bin)/box width) : ', tt
         call xwrite(ZWRite, 25)
         write(ZWRite,*) ' A. Lower threshhold? (Mid max bin-tt) : ', 
     &                    threl
         call xwrite(ZWRite, 25)
         write(ZWRite,*) ' B. Lower threshhold? (Mid third bin) : ', qel
         call xwrite(ZWRite, 25)
         write(ZWRite,*) ' C. Counts in bins 1-3 : ', baint3
         call xwrite(ZWRite, 25)
         call xwrite(' If A<B and C=0, lower thr = B, else A', 25)
c     If Mid max bin-tt < Mid third bin and Counts in bins 1-3 is 0,
c       use Mid third bin as Lower threshold
         IF ( threl.LT.qel .AND. baint3.LT..5 ) threl = qel
      ELSE
         threl = BACk - SIGmult*sig
      ENDIF
      threh = BACk + SIGmult*sig
      BNEw = 0.
      a1 = 0.
      write(ZWRite,*) ' Lower/Upper threshhold: ', threl, threh
      call xwrite(ZWRite, 20)
C further rejection of boxes with bad statistics
      DO 600 i = 1 , NBOxes
         IF ( BB(i).GT.-0.1 ) THEN
            im = (i-1)/NBX
            imin1 = (i-im*NBX-1)*IBBac + 1
            imin2 = im*IBBac + 1
            imax1 = imin1 + IBBac - 1
            imax2 = imin2 + IBBac - 1
c           write(ZWRite,*) ' ',i,' - Background in box', BB(i)
c           call xwrite(ZWRite, 25)
            IF ( BB(i).GT.threh .OR. BB(i).LT.threl ) THEN
               write(ZWRite,*) ' ',i,' - Background too far from avg',
     &                         BB(i)
               call xwrite(ZWRite, 20)
               IF ( DRAw_boxes ) THEN
c                 ximg = (imin1+imax1)/2.-1
c                 yimg = (imin2+imax2)/2.-1
                  ximg = float(imin1+imax1)/2.
                  yimg = float(imin2+imax2)/2.
                  call calimgpix(Szx,Szy,zmx,zmy,xcen,ycen,c1,c2,
     &                           ximg,yimg,1)
                  color = 6
c                 Magenta : Background too far from avg.
                  call jrnbox(c1,c2,bbb*2.,bbb*2.,0.,color,-1,-1)
               ENDIF
               BB(i) = -1.
               BB_flags(i) = 3
            ELSE
C calculate weights for background boxes
               a1 = a1 + 1.

               ximg = imin1
               yimg = imin2
               write (ZWRite,*) ' ',i,' - BoxLL (img) ', ximg, yimg
               call xwrite(ZWRite, 30)
               call calimgpix(Szx,Szy,zmx,zmy,xcen,ycen,xpix,ypix,
     &                        ximg, yimg, 1)
               call calimgpix(BGSz,BGSz,bgzmx,bgzmy,dxoff,dyoff,
     &                        xpix,ypix,ximg,yimg,2)
               write (ZWRite,*) ' ',i,' - BoxLL (bgimg) ', ximg, yimg
               call xwrite(ZWRite, 30)
               min21 = nint(ximg)
               min22 = nint(yimg)

               ximg = imax1
               yimg = imax2
               write (ZWRite,*) ' ',i,' - BoxUR (img) ', ximg, yimg
               call xwrite(ZWRite, 30)
               call calimgpix(Szx,Szy,zmx,zmy,xcen,ycen,xpix,ypix,
     &                        ximg, yimg, 1)
               call calimgpix(BGSz,BGSz,bgzmx,bgzmy,dxoff,dyoff,
     &                        xpix,ypix,ximg,yimg,2)
               write (ZWRite,*) ' ',i,' - BoxUR (bgimg) ', ximg, yimg
               call xwrite(ZWRite, 30)
               max21 = nint(ximg)
               max22 = nint(yimg)
c
c  Bug in original detect_bgr, where pixels 0 to boxsize were considered
c  instead of 1 to boxsize.  These lines mimic that behavior
c
c              min21 = min21 - 1
c              min22 = min22 - 1

               write (ZWRite,*) ' ',i,' - BGMAP ', min21, max21,
     &                                             min22, max22
               call xwrite(ZWRite, 27)

               su = 0.
               w = 0.
               DO 510 im = min21 , max21
                  DO 505 jm = min22 , max22
                     su = su + 1.
                     if ( im.eq.0 ) then 
                        w = w + 100.
                     elseif ( jm.ne.0 ) then
                        w = w + BGMap(im,jm)
                     endif
 505              CONTINUE
 510           CONTINUE
               WW(i) = w/su
               BNEw = BNEw + BB(i)
            ENDIF
         ENDIF
 600  CONTINUE
      IF ( BNEw.GT.0. ) THEN
         BNEw = BNEw/a1
      ELSE
         CALL XWRITE(' WARNING all background boxes have been rejected',
     &               10)
         CALL XWRITE(' image background is NOT well estimated ',10)
         IZEro = 1
         bama = 0.
         DO 650 isj = 1 , 11
            IF ( badist(isj).GT.bama ) THEN
               bama = badist(isj)
               bima = isj
            ENDIF
 650     CONTINUE
         BNEw = bamin + (bima-0.5)*step
      ENDIF
      RETURN
99001 FORMAT (' ',i10,'  bgnd boxes exceed max of ',i10)
      END
