      subroutine mkccalc(rmap, gmap, bmap, outmap, szx, szy, 
     &                   cutmode, cut, rlvl, glvl, blvl, 
     &                   mxinten, base, datamin, datamax, status)
      implicit none
c
c  Calculate true color image
c
c  I  rmap     (i)  red image
c  I  gmap     (i)  green image
c  I  bmap     (i)  blue image
c  O  outmap   (i)  output map 
c  I  szx      (i)  image size in x
c  I  szy      (i)  image size in y
c  I  cutmode  (i)  0=hist, 1=linear, 2=maxcol, 3=log
c  I  cut      (r)  cut values
c  I  rlvl     (r)  red level values
c  I  glvl     (r)  green level values
c  I  blvl     (r)  blue level values
c  I  mxinten  (i)  maximum intensity to scale to
c  I  base     (i)  base of color encoding
c  O  datamin  (d)  minimum array value
c  O  datamax  (d)  maximum array value
c  O  status   (l)  error flag (0 = OK)
c  
      integer szx, szy, mxinten, base, status
      integer rmap(szx,szy), gmap(szx,szy), bmap(szx,szy)
      integer outmap(szx,szy)
      integer cutmode
      real rlvl(mxinten+1), glvl(mxinten+1), blvl(mxinten+1), cut(6)
      real*8 datamin, datamax
c
c  Local variables
c
      integer i, j, ir, ig, ib
      integer szlvl, location
      real rtmp, gtmp, btmp, maxtmp
      real rmin, rmax, gmin, gmax, bmin, bmax
      real rscl, gscl, bscl
      character(80) zwrite
c
      datamin = 1e14
      datamax = -1e14

      if ( cutmode.eq.0 ) then
         szlvl = mxinten+1

         call xwrite(' Using histogram of intensities to make cut', 10)
         call xwrite(' i, Red, Green, Blue (levels)', 15)
         do i = 1, szlvl
            write(zwrite, *) i, rlvl(i), glvl(i), blvl(i)
            call xwrite(zwrite, 15)
         enddo
 
         do i = 1, szx
            do j = 1, szy
               ir = MIN(location(rlvl,szlvl,float(rmap(i,j))),mxinten)
               ig = MIN(location(glvl,szlvl,float(gmap(i,j))),mxinten)
               ib = MIN(location(blvl,szlvl,float(bmap(i,j))),mxinten)
               outmap(i,j) =  ir + ig*base + ib*base*base
               datamin = MIN(datamin, dfloat(outmap(i,j)))
               datamax = MAX(datamax, dfloat(outmap(i,j)))
            enddo
         enddo
      elseif ( cutmode.eq.1 ) then
         call xwrite(' Using linear scaling of intensities', 10)
         call xwrite(' Red, Green, Blue (min/max values)', 15)
         write(zwrite, *) cut(1), cut(3), cut(5)
         call xwrite(zwrite, 15)
         write(zwrite, *) cut(2), cut(4), cut(6)
         call xwrite(zwrite, 15)
         do i = 1, szx
            do j = 1, szy
               ir = float(mxinten)*
     &              (float(rmap(i,j)) - cut(1))/(cut(2)-cut(1))
               ig = float(mxinten)*
     &              (float(gmap(i,j)) - cut(3))/(cut(4)-cut(3))
               ib = float(mxinten)*
     &              (float(bmap(i,j)) - cut(5))/(cut(6)-cut(5))
               if ( ir.lt.0 ) ir = 0
               if ( ig.lt.0 ) ig = 0
               if ( ib.lt.0 ) ib = 0
               if ( ir.gt.mxinten ) ir = mxinten
               if ( ig.gt.mxinten ) ig = mxinten
               if ( ib.gt.mxinten ) ib = mxinten
               outmap(i,j) =  ir + ig*base + ib*base*base
               datamin = MIN(datamin, dfloat(outmap(i,j)))
               datamax = MAX(datamax, dfloat(outmap(i,j)))
            enddo
         enddo
      elseif ( cutmode.eq.2 ) then
         call xwrite(' Using linear scaling by maximum color', 10)
         call xwrite(' Red, Green, Blue (cut values)', 15)
         write(zwrite, *) cut(1), cut(3), cut(5)
         call xwrite(zwrite, 15)
         write(zwrite, *) cut(2), cut(4), cut(6)
         call xwrite(zwrite, 15)
         do i = 1, szx
            do j = 1, szy
               rtmp = (float(rmap(i,j)) - cut(1))/(cut(2)-cut(1))
               gtmp = (float(gmap(i,j)) - cut(3))/(cut(4)-cut(3))
               btmp = (float(bmap(i,j)) - cut(5))/(cut(6)-cut(5))
               if ( rtmp.lt.0. ) rtmp = 0.
               if ( gtmp.lt.0. ) gtmp = 0.
               if ( btmp.lt.0. ) btmp = 0.
               maxtmp = MAX(rtmp,btmp,gtmp)
               if ( maxtmp.gt.0. ) then
                  ir = int(float(mxinten)*rtmp/maxtmp)
                  ig = int(float(mxinten)*gtmp/maxtmp)
                  ib = int(float(mxinten)*btmp/maxtmp)
               else
                  ir = 0
                  ig = 0
                  ib = 0
               endif
               outmap(i,j) =  ir + ig*base + ib*base*base
               datamin = MIN(datamin, dfloat(outmap(i,j)))
               datamax = MAX(datamax, dfloat(outmap(i,j)))
            enddo
         enddo
      elseif ( cutmode.eq.3 ) then
         call xwrite(' Using logarithmic scaling of intensities', 10)
         call xwrite(' Red, Green, Blue (min/max values)', 15)
         write(zwrite, *) cut(1), cut(3), cut(5)
         call xwrite(zwrite, 15)
         write(zwrite, *) cut(2), cut(4), cut(6)
         call xwrite(zwrite, 15)
         rmin = cut(1)
         rmax = cut(2)
         gmin = cut(3)
         gmax = cut(4)
         bmin = cut(5)
         bmax = cut(6)
         rscl = float(mxinten)/log10(MAX(rmax-rmin,1.))
         gscl = float(mxinten)/log10(MAX(gmax-gmin,1.))
         bscl = float(mxinten)/log10(MAX(bmax-bmin,1.))
         do i = 1, szx
            do j = 1, szy
               ir = rscl*log10(MAX(1.,float(rmap(i,j))-rmin))
               ig = gscl*log10(MAX(1.,float(gmap(i,j))-gmin))
               ib = bscl*log10(MAX(1.,float(bmap(i,j))-bmin))
               if ( ir.lt.0 ) ir = 0
               if ( ig.lt.0 ) ig = 0
               if ( ib.lt.0 ) ib = 0
               if ( ir.gt.mxinten ) ir = mxinten
               if ( ig.gt.mxinten ) ig = mxinten
               if ( ib.gt.mxinten ) ib = mxinten
               outmap(i,j) =  ir + ig*base + ib*base*base
               datamin = MIN(datamin, dfloat(outmap(i,j)))
               datamax = MAX(datamax, dfloat(outmap(i,j)))
            enddo
         enddo
      endif

      return
      end
