c
      FUNCTION xrrunmean(y, sy, n, n1, n2, iwidth, iflag)
c
c  ls 2/8/90 to calculate running mean for point n of an array: equivalent to
c            a square function smoothing
c
c     I    y  = array
c     I    sy = error array (data gaps are identified by sy<-1.1e34)
c     I    n  = index of point for which running mean must be calculated
c     I    n1 = index of first valid point
c     I    n2 = index of last  valid point
c     I    iwidth = full width of running mean (if avg. is calculated over
c                   less than iwidth/4 points routine returns a gap value)
c     I    iflag = 1     to use also point n in running mean
c                = 0 NOT  "  "    "    "   "  "    "     "
c
c Assumes that y are equispaced (except for gaps) and that the routine
c is called sequentially starting from n=n1, such that a fast algorithm
c storing previous results can be used.
c
      INTEGER*4 i, n, n1, n2, iwidth, iflag, iforw, iback, k
      REAL*4 y(*), sy(*), xrrunmean, sum
      SAVE
c      if(iwidth.le.1) return    !protection
c
c calculate sum on first call
c
      IF (n.EQ.n1) THEN
c set up no. of points forward
         iforw = iwidth/2
c set up no. of points backward
         iback = (iwidth-1)/2
         k = 0
         sum = 0.
c calculate sum from 1st point forward
         DO i = n1, min(n1+iforw, n2)
            IF (sy(i).GT.-1.1E34) THEN
               k = k + 1
               sum = sum + y(i)
            ENDIF
         ENDDO
c if iflag=0
         IF (iflag.EQ.0 .AND. sy(n1).GT.-1.1E34) THEN
c subtract y(n1)
            sum = sum - y(n1)
c decrease k
            k = k - 1
         ENDIF
c
c calculate sum on subsequent calls (using previous values of sum and k)
c
      ELSE
c if in allowed range
         IF (n+iforw.LE.n2) THEN
c if not a gap
            IF (sy(n+iforw).GT.-1.1E34) THEN
c sum next point forward
               sum = sum + y(n+iforw)
c increase k
               k = k + 1
            ENDIF
         ENDIF
c if in allowed range
         IF (n-iback-1.GE.n1) THEN
c if not a gap
            IF (sy(n-iback-1).GT.-1.1E34) THEN
c subtract point backward
               sum = sum - y(n-iback-1)
c decrease k
               k = k - 1
            ENDIF
         ENDIF
c if iflag=0
         IF (iflag.EQ.0) THEN
c if previous central point not gap
            IF (sy(n-1).GT.-1.1E34) THEN
c sum it
               sum = sum + y(n-1)
c increase k
               k = k + 1
            ENDIF
c if current central point not gap
            IF (sy(n).GT.-1.1E34) THEN
c subtract it
               sum = sum - y(n)
c decrease k
               k = k - 1
            ENDIF
         ENDIF
c
c
      ENDIF
c
c if k<width/4  return with gap value
      IF (k.LE.iwidth/4) THEN
         xrrunmean = -1.2E34
         RETURN
      ENDIF
c
c calculate mean
      xrrunmean = sum/float(k)
      RETURN
      END
c
c
c
