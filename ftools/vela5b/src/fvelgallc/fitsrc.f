C This subroutine extracts the light curve for each of up to 20 sources from
C the map data
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Version 0.9   2 Mar 1995
C          0.91  7 Mar 1995  Uses dynamically allocated data arrays to store
C                              results
C          1.1  12 Sep 1995  Checks for DOMAIN errors in SQRT, allows 20 
C                              sources to be fit
C          1.2   3 Nov 1995  Added monitoring write statements (temporary)
C          1.21 11 Dec 1995  Changed common to remove begin and end times
C          1.22 13 Feb 1996  Removed obsolete iex calculations
 
      subroutine fitsrc(row, mjdtime, ast, aerrst, csqst, ndofst, 
     +           numofrows)

      implicit none

C Common block declarations

      common /SOURCE/ long_src, lat_src, longcntr, latcntr, longmin, 
     +  longmax, latmin, latmax, longavg, latavg, counts, backgnd, 
     +  frate, numpnts, nsrc, imax, jmax
      common /NAME/ sourcename

      integer nsrc, imax, jmax, numpnts(20,20)
      real long_src(20), lat_src(20)
      real longcntr(20,20), latcntr(20,20)
      real counts(20,20), backgnd(20,20)
      real longmin, longmax, latmin, latmax, longavg, latavg, frate

      character(16) sourcename(20)

C Local variables

      logical nodata

      integer row, i, j, k, m, n, l, isign, numofrows
      integer ndof, ndofsv, ndofst(numofrows), ir(23)

      real longrange, latrange, dellong, dellat
      real ast(nsrc+3,numofrows), aerrst(nsrc+3,numofrows)
      real csqst(numofrows)

      double precision r(20,20,20), rs(20,20,23)
      double precision var(20,20), pred(20,20)
      double precision b(23,23), bsave(23,23)
      double precision a(23), asave(23), c(23), det(23)
      double precision csqcrit, csqMN, dcsq
      double precision alold, delal, c3
      double precision aerr(23), mjdtime(numofrows)

      character(80) message


C Calculate exposure in each bin

      call exposure(r, mjdtime(row))

C Program now fits the intensity map to the predicted count rate from 
C  the sources and background.  At each point (i,j), the predicted
C  count rate is the sum over k of a(k)*rs(i,j,k) where:
C
C  rs(i,j,1) = 1.0                 a(1) = constant background
C  rs(i,j,2) = 2 * relative long   a(2) = linear background trend in longitude
C  rs(i,j,3) = 2 * relative lat    a(3) = linear background trend in latitude
C  rs(i,j,k+3) = r(i,j,k)          a(k+3) = strength of source k
C
C There are thus 3 + nsrc terms in the fit
C
C To find the best fit, we minimumize chisq = 
C  The sum over i,j of (x(i,j) - pred)**2 / var(i,j)
C  where var(i,j) is the variance of x(i,j)

C Fill in the extended response matrix array rs(i,j,k)

      longrange = longmax - longmin
      latrange = latmax - latmin
      do 300 i = 1, imax 
         do 200 j = 1, jmax
            rs(i,j,1) = 1.0D0
            dellong = longcntr(i,j) - longavg
            dellat = latcntr(i,j) - latavg
            if (dellong .lt. -180.0) dellong = dellong + 360.0
            if (dellong .gt. 180.0) dellong = dellong - 360.0
            rs(i,j,2) = 2.0D0 * DBLE(dellong / longrange)
            rs(i,j,3) = 2.0D0 * DBLE(dellat / latrange)
            do 100 k = 1, nsrc
               rs(i,j,k+3) = r(i,j,k)
 100        continue
 200     continue
 300  continue

C Calculate the variance array; set to 0.0 if there is no data

      do 500 i = 1, imax
         do 400 j = 1, jmax
            var(i,j) = 0.0D0
            if (counts(i,j) .le. -100.0) go to 400
            var(i,j) = backgnd(i,j)**2
 400     continue
 500  continue

C Calculate terms of the matrix b(3+nsrc,3+nsrc) used to calculate the 
C minimum chisq

      call calcmat(rs, counts, var, b, c, nsrc, imax, jmax, nodata)
      if (nodata) then
         write(message,'('' Insufficient data at MJD = '', F13.7)') 
     +        mjdtime(row)
         call fcecho(message)
         go to 1999
      endif

      do 700 m = 1, nsrc+3
         do 600 n = 1, nsrc+3
            bsave(m,n) = b(m,n)
 600     continue
 700  continue

C Invert the matrix b (ir and det are arrays for inversion routines)

      call matinv(b, 23, nsrc+3, ir, det)

C Multiply b inverse and c to get the best fit intensities a

      do 1000 m = 1, nsrc+3
         a(m) = 0.0D0
         do 900 n = 1, nsrc+3
            a(m) = a(m) + b(m,n)*c(n)
            asave(m) = a(m)
 900     continue
 1000 continue

C Calculate minimum value of chisq

      call chisq(a, rs, counts, var, pred, nsrc, imax, jmax,
     +     csqMN, ndof)
      ndof = ndof - (nsrc + 3)
      ndofsv = ndof

C Now calculate the error bounds on a(1) - a(nsrc+3)

      dcsq = 1.0D0
      csqcrit = csqMN + dcsq

C Vary parameters a(1) - a(nsrc+3) and look for error bounds

      do 1400 l = 1, nsrc+3
         do 1300 isign = -1, 1, 2
            alold = a(l)
            a(l) = a(l) + (1.0D0 * DBLE(isign))
            delal = a(l) - alold
            call fixone(a, bsave, c, l, nsrc)
            call chisq(a, rs, counts, var, pred, nsrc, imax, jmax, 
     +           c3, ndof)
            if (c3 .eq. csqMN) go to 1250
            delal = delal * DSQRT(dcsq / (c3-csqMN))
 1250       a(l) = alold + delal
            call fixone(a, bsave, c, l, nsrc)
            call chisq(a, rs, counts, var, pred, nsrc, imax, jmax, 
     +           c3, ndof)
            if (isign .eq. -1) aerr(l) = a(l)
            if (isign .eq. 1) aerr(l) = DABS(a(l) - aerr(l)) / 2.0D0
            do 1275 m = 1, nsrc+3
               a(m) = asave(m)
 1275       continue
 1300    continue
 1400 continue

C Store fit summary data for data set row

      do 1500 k = 1, nsrc+3
         ast(k, row) = REAL(a(k))
         aerrst(k, row) = REAL(aerr(k))
 1500 continue
      csqst(row) = REAL(csqMN)
      ndofst(row) = ndofsv

 1999 return

      end

C----------------------------------------------------------------------------
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Version 0.0   2 Mar 1995  First draft
C          1.1  12 Sep 1995  Altered to allow 20 sources

      subroutine chisq(a, rs, counts, var, pred, nsrc, imax, jmax, 
     +           csq, ndof)

      implicit none

      integer i, j, k, nsrc, imax, jmax, ndof

      real counts(20,20)

      double precision a(23), rs(20,20,23), var(20,20), pred(20,20)
      double precision csq


      csq = 0.0D0
      ndof = 0
      do 300 j = 1, jmax
         do 200 i = 1, imax
            pred(i,j) = 0.0D0
            do 100 k = 1, nsrc+3
               pred(i,j) = pred(i,j) + a(k)*rs(i,j,k)
 100        continue
            if (var(i,j) .le. 0.0D0) go to 200
            ndof = ndof + 1
            csq = csq + (counts(i,j) - pred(i,j))**2 / var(i,j)
 200     continue
 300  continue

      return

      end


C----------------------------------------------------------------------
C Calculates the solution matrices b & c for least squares fit.
C b will be inverted in the main program to solve for the best least 
C squares fit.
C
C Note: See notebook, p. 24 (Bill Priedhorsky)
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Version 0.0               Priedhorsky's version
C  Version 0.9   3 Mar 1995  First draft for FVELGALLC FTOOL
C          1.1  12 Sep 1995  Altered to allow 20 sources

      subroutine calcmat(rs, counts, var, b, c, nsrc, imax, jmax,
     +           nodata)

      implicit none

      logical nodata

      integer i, j, m, n, nsrc, imax, jmax

      real counts(20,20)

      double precision rs(20,20,23), var(20,20), b(23,23), c(23)
 

      do 600 n = 1, nsrc+3
         nodata = .TRUE.
         c(n) = 0.0D0
         do 200 j = 1, jmax
            do 100 i = 1, imax
               if (rs(i,j,n) .eq. 0.0D0) go to 100
               if (var(i,j) .le. 0.0D0) go to 100
               nodata = .FALSE.
               c(n) = c(n) + (counts(i,j)*rs(i,j,n)/var(i,j)) 
 100        continue
 200     continue
         if (nodata) return
         do 500 m = 1, nsrc+3
            b(n,m) = 0.0D0
            do 400 j = 1, jmax
               do 300 i = 1, imax
                  if (var(i,j) .le. 0.0D0) go to 300
                  b(n,m) = b(n,m) + (rs(i,j,m)*rs(i,j,n)/var(i,j))
 300           continue
 400        continue
 500     continue
 600  continue

      return

      end


C----------------------------------------------------------------------
C This is a subroutine to calculate the best fit parameters a(1 .. nsrc+3),
C holding a(1) fixed at the input value.  One applies the minimumization
C criteron d(chisq)/d(a(k)) = 0 for all k != 1.  This requires a solution
C of a nsrc+3-1 matric equation, which is why ah, bh, ch are indexed up to
C 22 rather than 23.  Values from the larger original array are loaded into
C the "h" arrays with n = nold up to nold = l-1; n = nold - 1 for 
C nold >= l+1; and no value stored in the new n array for nold = 1.
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Version 0.0               Priedhorsky's version
C  Version 0.9   3 Mar 1995  First draft for FVELGALLC FTOOL
C          1.1  12 Sep 1995  Altered to allow 20 sources
C          1.11 21 Dec 1995  Corrected typographical error: a(1) -> a(l) 
C                             in line 4

      subroutine fixone(a, bsave, c, l, nsrc)

      implicit none

      integer n, m, if, l, nsrc, nold, mold, ir(23)

      double precision a(23), bsave(23,23), c(23)
      double precision ah(22), bh(22,22), ch(22)
      double precision det(23)

      do 200 n = 1, nsrc+2
         nold = n
         if (n .ge. l) nold = nold + 1               
         ch(n) = c(nold) - (a(l) * bsave(nold,l))
         do 100 m = 1, nsrc+2
            mold = m
            if (m .ge. l) mold = mold + 1
            bh(n,m) = bsave(nold, mold)
 100     continue
 200  continue
      call matinv(bh, 22, nsrc+2, ir, det)

      do 400 m = 1, nsrc+2
         ah(m) = 0.0D0
         do 300 n = 1, nsrc+2
            ah(m) = ah(m) + (bh(m,n)*ch(n))      
 300     continue
 400  continue

      if = 0
      do 500 m = 1, nsrc+2
         if (m .ge. l) if = 1
         a(m+if) = ah(m)
 500  continue

      return

      end


C----------------------------------------------------------------------
C This routine takes a double precision matrix given to it and inverts 
C that matrix
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Version 0.0               Priedhorsky's version
C  Version 0.9   3 Mar 1995  First draft for FVELGALLC FTOOL

      subroutine matinv(a, lda, n, ir, det)

      implicit none

      integer lda, n, ir(n)

      double precision a(lda,n), det(n)
      double precision rcond, sgdet(2), work(60)

      call Dgeco(a, lda, n, ir, rcond, det)
      call Dgedi(a, lda, n, ir, sgdet, work, 1)

      return

      end
