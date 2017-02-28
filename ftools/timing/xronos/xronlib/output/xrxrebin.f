c
      SUBROUTINE xrxrebin(dxsta, dxstep, k, rebin, iflags, x, sx, j,
     &                    ireb)
c
c LS 6/12/88 to rebin indep. variable(s) linearly and logarithmically
c
c Note: error bars summed in the rebinning. Make one call to this
c       subroutine to get 1 rebinned (x) point
c
c   I   dxsta = start value for 'x-variable' (equispaced)
c   I   dxstep = step for 'x-variable' (equispaced)
c   I   k = index of first "x" to be rebinned
c   I   rebin = 0 no rebinning,>1 step for const rebinning
c               <-1 step for geom. series rebinning, -1000 for octaves
c   I   iflags = flags for plot, file type, analysis type
c                (iflags(9) no. of indep. variable columns)
c                (iflags(14) = nanal)
c   O   x,sx = value and error of 'x-variable' after rebinning
c   O   j = progressive index for rebinned values
c   O   ireb = no. of good orig. values rebinned in current output values
c
      INTEGER*4 j, ireb, iflags(*), m, k
      REAL*8 x(*), sx(*)
      real*4 rebin, rbe
c       real*8 dxsta(iflags(9)/2),dxstep(iflags(9)/2)
      REAL*8 dxsta(*), dxstep(*)
c
c the save command here was inserted because for some strange reason 
c under sgi the current value of the variable is not save afte the 
c call is done. Is this a compiler problem ? note the xanadu
c version does not suffer of the same problem.
c
      save rbe
c
c means start rebinning new frame
      IF (k.EQ.1) THEN
c initialise other values
         rbe = 1.
         j = 0
      ENDIF
c
c no. rebinning
      ireb = 1
c determine ireb= current rebinning factor
      IF (rebin.GT.1.) THEN
c constant rebinning
         ireb = int(rebin)
      ELSEIF (rebin.GT.-999 .AND. rebin.LT.-1.) THEN
c geometr. series rebinning
         rbe = rbe*abs(rebin)
         ireb = int(rbe)
      ELSEIF (rebin.LT.-999.) THEN
c octave spacing
         ireb = 1
      ENDIF
c
c      do rebinning
c
      DO m = 1, iflags(9)/2
c
c for const. and log rebinning
c      write(*,*)'xrxrebin dxsta, dxstep', dxsta(m), dxstep(m), k
c      write(*,*)'xrxrebin rebin, iflags',rebin, iflags(14)
c      write(*,*)'xrxrebin x, sx, j, ireb',x(m),sx(m),j,ireb
c
c
         IF (rebin.GT.-999.) THEN
            x(m) = (2.D0*dxsta(m)+
     &         (dble(k-1+min(k+ireb-1,iflags(14))-1))*dxstep(m))/2.D0
            sx(m) = dble(min(k+ireb-1,iflags(14))-k+1)*dxstep(m)/2.D0
c
c for octave spacing
         ELSE
            x(m) = dxsta(m)*2.**(k-1)
            sx(m) = dxstep(m)*2.**(k-1)/4.
         ENDIF
      ENDDO
c
c increase counters
c
      j = j + 1
      k = min(k+ireb-1, iflags(14))
c      write(*,*)'xrxrebin x, sx, j',x(m),sx(m),j,k,ireb
      RETURN
      END
c
c
