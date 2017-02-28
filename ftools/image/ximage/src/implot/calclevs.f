C **************************************************************************
c
c     R*4 image
c     Calculates levels based on entered parameters
c
c    subroutine pg_crtlevs(a,idim,jdim,i1,i2,j1,j2,minlev,maxlev,minval,
c                          maxval,levels_flag,levnum,levary)
c     R*4 a(idim,jdim) 
c     I*4 idim,jdim array bounds
c     I*4 i1,i2,j1,j2 image bounds
c     I*4 minlev - minimum level (and value)
c     I*4 maxlev - maximum level
c     I*4 maxval - maximum value
c     I*4 Levels flag (0 - linear 1 - histogram  2 - logarithmic
c                      3 - leave untouched  10 - square-root  )
c     I*4 levnum - Number of levels to calculate
c     R*4 levary - Calculated levels
c     I*4 status - Error flag ( 0 = OK )
c                     
c     10/21/98 - Added ability to specify min and max levels
c
c     12/11/97 - Modified for new imaging mechanism (XPGIMAG)
c     
C **************************************************************************
      subroutine calclevs(a,idim,jdim,i1,i2,j1,j2,minlev,maxlev,
     &                    minval,maxval,levels_flag,levnum,levary,
     &                    status)
      implicit none

* Import :

      integer idim,jdim,i1,i2,j1,j2
      real*4 a(idim,jdim)
      real*4 minlev, maxlev, minval, maxval
      integer levels_flag, levnum
      real*4 levary(*)
      integer status

      include '../include/io.inc'

c     levels_flag = 0 -> linear colour scale 
c     levels_flag = 1 -> image histogram colour scale 
c     levels_flag = 2 -> logarithmic colour scale 
c     levels_flag = 3 -> leave levary untouched
c     levels_flag = 10 -> square root colour scale
c
* Local constants :

      real*4 tmpblk,black,white
      real*4 offset, ahigh, alow
      real*4 step, arange
      integer i, j, efflevs
      real*4 temp_val
      logical ISRNULL

      status = 0

c  Initialize to avoid warning
      efflevs = 0
c  --

CvMSJ - Sept. 28, 1998 - Sets number of levels by entered argument
      if ( levels_flag.ne.3 ) then

         if ( ISRNULL(minlev) ) then
            black = minval
         else
            black = minlev
         endif
         if ( ISRNULL(maxlev) ) then
            white = maxval
            efflevs = levnum
         else
            white = maxlev
            efflevs = levnum - 1
            levary(levnum) = maxlev
         endif
      endif
c
c  Linear scaling
c
      if (levels_flag.eq.0) then
         arange = abs(white-black)
         step = arange/float(efflevs)
         do i=1,efflevs
               levary(i) = black + (i-1)*step
         enddo 

c
c  Histogram equalization
c
      else if (levels_flag.eq.1) then 

          call calchist (a,idim,jdim,i1,i2,j1,j2,black,white,
     &                   levnum,levary, status)
c
c Logarithmic scaling
c

      else if (levels_flag.eq.2) then 

         if ( black.lt.0. ) then

            offset = 1. - black
            ahigh = log10(white+offset)
            step = ahigh/float(efflevs)
            levary(1) = 1. - offset
            do i=2,efflevs
               temp_val = (float((i-1))*step)
               levary(i) = 10**temp_val - offset
            enddo 

         else

            if ( black.eq.0. ) then
c
c             Zero value undefined for log, find next lowest value
c             for levels
c
               tmpblk = 1.
               do i = i1, i2
                  do j = j1, j2
                     if ( .not.ISRNULL(a(i,j)) .and. a(i,j).ne.0. ) then
                        if ( a(i,j).lt.tmpblk ) then
                           tmpblk = a(i,j)
                        endif
                     endif
                  enddo
               enddo
               alow = log10(tmpblk)
            else
               alow = log10(black)
            endif

            ahigh = log10(white)
            arange = ahigh-alow
            step = arange/float(efflevs)
            levary(1) = 10**alow
            if ( black.eq.0. ) levary(1) = 0.
            do i=2,efflevs
               temp_val = (float((i-1))*step) +  alow
               levary(i) = 10**temp_val
            enddo 

         endif
c
c Square-root scaling
c
      else if (levels_flag.eq.10) then
         arange = abs(white - black)
         do i = 1, efflevs
            levary(i) = MIN(white,
     &                  black + (float(i-1)/efflevs)**2.0*arange)
         enddo

      elseif (levels_flag.eq.3 ) then

c        Do nothing

      else

         write (ZWRite, *) ' Invalid levels flag', levels_flag
         call XWRITE(ZWRite, 5)
         status = -10

      endif 

      return
      end
