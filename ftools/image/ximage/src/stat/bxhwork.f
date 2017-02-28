      subroutine bxhwork(Alist, Blist, Npix, Prob)
      implicit none
c
c  Performs chi-square calculation, comparing two groups of
c  sorted pixels
c
c  I  A/Blist  (i) Groups to be compared
c  I  Npix     (i) Number of elements in each group
c  O  Prob     (r) Probability that they match 
c
      integer Npix
      integer Alist(Npix), Blist(Npix)
      real*4 Prob
c
c  Local variables
c
      real*4 chsq
      integer df
      integer ai, bi, acnt, bcnt, val
      real*4 gammq

      chsq = 0.
      df = 0
      ai = 1
      bi = 1

      do while ( ai.le.Npix .or. bi.le.Npix )
         if ( ai.gt.Npix ) then 
            val = Blist(bi)
         elseif ( bi.gt.Npix ) then
            val = Alist(ai)
         elseif ( Blist(bi).lt.Alist(ai) ) then
            val = Blist(bi)
         else
            val = Alist(ai)
         endif
c
c       Count number of occurences of val in alist
c
         acnt = 0
         do while ( ai.le.Npix .and. Alist(ai).eq.val ) 
            acnt = acnt + 1
            ai = ai + 1
         enddo
         bcnt = 0
         do while ( bi.le.Npix .and. Blist(bi).eq.val ) 
            bcnt = bcnt + 1
            bi = bi + 1
         enddo

         df = df + 1
c        print*, val, ' - a:', acnt, ' b:', bcnt
         chsq = chsq + float(acnt-bcnt)**2/float(acnt+bcnt)
      enddo

      Prob = gammq(0.5*float(df),0.5*chsq)

      return
      end
