c
      SUBROUTINE xrxaxis (dxsta, dxstep, doo, iflags, rflags,
     &                     xr, sxr, jmax)
c
c  dxsta      I   R  first value for xaxis
c  dxstep     I   R  step for x axis
c  doo        I   R  unit convertion factor
c  iflags     I   I  Integer flags array
c  rflags     I   R  real flags array
c  rebin      I   R  rebin flag (0=no, > 0=linear, < 0 =geometric)
c  xr         O   R  output on xaxis values
c  sxr        O   R  output error on xaxis values
c  jmax       O   I  size of xr array 
c
c Note for folding xr contains the phases 
c
c Input variable
        INTEGER*4 iflags(*)
        REAL*4 rflags(*)
        REAL*8 dxsta(*), dxstep(*), doo
c output variable
        REAL*8 xr(*), sxr(*)
        INTEGER*4 jmax 
c local variable
        INTEGER*4 m, ireb
        REAL*8 dco(0:4)
        REAL*8 x(8), sx(8)
        INTEGER*4 k, j , i 
c
        DATA dco/1.D0, 1.D0, 3600.D0, 86400.D0, 1.D0/ 
c
c Initialize variables. The x and sx currently are dimension 8.
c The dimension should correspond to something more sensible.
c Note the xr ans sxr should be done in a different place
c 
         k=0
         jmax=0
         j=0
         DO i=1,8
           x(i)=0.0
           sx(i)=0.0
         ENDDO
c         write(*,*)'xrxaxis k,iflags(14)',k,iflags(14)
         DO WHILE (k .LT. iflags(14))
            k=k+1
            CALL xrxrebin(dxsta, dxstep, k, rflags(1), iflags,  
     &                    x, sx, j, ireb)
            DO m = 1, iflags(9)/2
               xr(j)=0.0
               sxr(j)=0.0
               IF (iflags(2).GT.0) THEN
c secs from start in plot
                  x(m) = x(m) + doo*86400.D0
C convert to requested unit
                  x(m) = x(m)/dco(iflags(2))
c convert to requested unit
                  sx(m) = sx(m)/dco(iflags(2))
               ENDIF
c               write(*,*)'xrxaxis x(m), sx(m)',x(m), sx(m)
               xr(j)=x(m)
               sxr(j)=sx(m)
c               write(*,*)'xrxaxis xr(m),sxr(m)',xr(m),sxr(m),j,jmax
            ENDDO
         ENDDO
         jmax=j
         RETURN
         END
