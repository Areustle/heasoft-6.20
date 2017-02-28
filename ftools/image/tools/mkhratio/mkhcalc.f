      subroutine mkhcalc(amap, bmap, outmap, szx, szy, sclmode, sclfact, 
     &                   nbins, cut, datamin, datamax, status)
      implicit none
c
c  Calculate hardness ratio image
c
c  I  amap     (i)  image a
c  I  bmap     (i)  image b
c  O  outmap   (i)  output image map 
c  I  szx      (i)  image size in x
c  I  szy      (i)  image size in y
c  I  sclmode  (i)  0=exp, 1=linear
c  I  sclfact  (r)  scaling factor (for all sclmode values)
c  I  nbins    (i)  number of bins (for sclmode=2)
c  I  cut      (i)  cut values (1)=amin (2)=bmin
c  O  datamin  (d)  minimum map value
c  O  datamax  (d)  maximum map value
c  O  status   (i)  error flag (0=OK)
c  
      integer szx, szy, status
      integer amap(szx,szy), bmap(szx,szy)
      real outmap(szx,szy), sclfact
      integer sclmode, nbins, cut(2)
      real*8 datamin, datamax

      include 'dynmem.inc'
c
c  Local variables
c
      logical isrnull
      real*4 rnull, rmin, rmax
      integer i, j, p_bins
      integer num, den, location
      character(80) zwrite
c
      if ( sclmode.eq.-1 ) then
         call xwrite(' No scaling applied to hardness values', 10)
      elseif ( sclmode.eq.0 ) then
         call xwrite(' Using exponent to scale hardness values', 10)
      elseif ( sclmode.eq.1 ) then
         call xwrite(' Using linear factor to scale hardness values',
     &                 10)
      elseif ( sclmode.eq.2 ) then
         call xwrite(' Using equally spaced bins to translate '//
     &               'hardness values', 10)
         call mkhralloc(1, nbins, 1, p_bins, status)
         if ( status.ne.0 ) return
         call mkhbin(memr(p_bins), nbins, status)
      endif

      rmin = rnull()
      rmax = rnull()

      do i = 1, szx
         do j = 1, szy
            num = bmap(i,j) - amap(i,j)
            den = bmap(i,j) + amap(i,j)
c           if ( num.eq.0 .or. den.eq.0 .or. 
c           if ( amap(i,j).eq.0 .or. bmap(i,j).eq.0 .or.
c    &           amap(i,j).lt.cut(1) .or. bmap(i,j).lt.cut(2)) then

            if ( den.eq.0 .or. 
     &           ( bmap(i,j).eq.0 .and.  amap(i,j).lt.cut(1) ) .or.
     &           ( amap(i,j).eq.0 .and.  bmap(i,j).lt.cut(2) ) ) then
               if ( sclmode.eq.-1 ) then
                  outmap(i,j) = rnull()
               else
                  outmap(i,j) = 0.
               endif
            else
               if ( sclmode.eq.-1 ) then
                  outmap(i,j) = float(num)/float(den)
               elseif ( sclmode.eq.0 ) then
                  outmap(i,j) = sclfact*exp(float(num)/float(den))
               elseif ( sclmode.eq.1 ) then
                  outmap(i,j) = sclfact*(float(num)/float(den)+2.0)
               else
                  outmap(i,j) = location(memr(p_bins),nbins,
     &                                   float(num)/float(den)) + 1.
c                 if ( outmap(i,j).eq.0 ) print*, i, j,
c    &                                    float(num)/float(den)
c                 outmap(i,j) = int(sclfact*(float(num)/float(den)+1.0))
               endif
            endif
            if ( isrnull(rmin) .or. outmap(i,j).lt.rmin ) 
     &         rmin = outmap(i,j)
            if ( isrnull(rmax) .or. outmap(i,j).gt.rmax ) 
     &         rmax = outmap(i,j)
         enddo
      enddo

      datamin = rmin
      datamax = rmax

      return
      end
