      
      SUBROUTINE xrplt (cpro, yr, syr, expr, xr, sxr, nkount, npsize,
     &     chival, chierr, kmax, dpera, nper, iflags, cmd, 
     &     ncmd, plot, ier)
c     
c     cpro    I   program type
c     yr      I   array result to be plot 
c     syr     I   error array to be plot
c     expr    I   expr array
c     xr      I   xaxis array
c     sxr     I   error xaxis array
c     nkount  I   size of input arrays
c     chival  I   array of chival
c     chierr  I   array of chierr
c     kmax    I   index for best period array 
c     dpera   I   array of periods
c     nper    I   number of periods
c     iflags  I   array of iflags
c     cmd     I   array of plt commands 
c     ncmd    I   number of commands
c     ier     O   error flag (0=OK)
c     
c     
c     Note jmax probably is not needed !! because it should be already in 
c     iflgs(13)
c     
      implicit none
      INTEGER*4 nanal, iflags(*), kmax(*), nper , ncmd, nkount, npsize
      REAL*4 yr(nkount, *), syr(nkount, *), expr(nkount)
      REAL*4  chival(nper,*), chierr(nper,*)
      real*8 xr(*), sxr(*),dpera(*)
      character(132) cmd(*)
      CHARACTER*(*) cpro
      INTEGER*4 ier
c     
c     Local variable
c     the maximun number of column for the plotting = 17
c     is calculated based on the 4 series analysis plus the xaxis
      REAL*4 plot(npsize,*) 
      REAL*4 step 
      INTEGER*4 i, inew, m, k, j, iery(17)
      INTEGER npts, nvec, iv
      INTEGER final, cycle, jc, jf
      LOGICAL chkgap, isgap
c     
c     for efs used the chival array array and the dper for indenpent variable
c     
      IF (cpro(1:2).eq. 'es') THEN
         step=(dpera(2)-dpera(1))/2.
         DO i=1, nper
c     
c     load xaxis +error
            plot(i,1)= dpera(i) - dpera(kmax(1))
            plot(i,2)= step
            k=2
            DO m=1, iflags(10) 
               k=k+1
c     
c     load yaxis +error
               plot(i,k) = chival(i,m)
               k=k+1
               plot(i,k) = chierr(i,m)
            ENDDO
         ENDDO
c     
c     prepare the other plt variable
c         mxrow=nper+1
         npts=nper
         nvec=iflags(10)+1
         DO j=1,nvec
            iery(j)= 1
         ENDDO
      ELSE
c     two cycles for folding 
c     
         cycle=1
         IF(cpro(1:2).eq.'ef')cycle=2
         chkgap=.false.
         IF(cpro(1:2).eq.'lc')chkgap=.true.

         jf=1
         npts=0
         DO jc=1,cycle 
            DO j=1, iflags(13)
c     
c     load xaxis + error
               plot(jf,1)= xr(j)+(jc-1)
               plot(jf,2)= sxr(j)
c     load yaxis + error
               k=2
c     start out assuming gap. first non-gap value indicates no gap
               isgap = .true.
               DO i =1,(iflags(8)-1)/2
                  k=k+1
                  if ( yr(j,i).ne.-1.2E-34 ) isgap = .false.
                  plot (jf,k)=yr(j,i)
                  k=k+1
                  plot(jf,k)=syr(j,i)
               ENDDO
c     load last array
               plot(jf,k+1)=expr(j)
c     if not checking gap or checking gap and not a gap, increment jf
c     otherwise, jf remains the same and overwrites gap on next loop
               if ( .not.chkgap .or. .not.isgap ) then
                  npts = jf
                  jf = jf + 1
               endif
            ENDDO
         ENDDO
c     
c     nvec is the number of results vectors=(iflags(8)-1)/2 + xaxis +exposure
c     (used to be npts=iflags(13)*cycle, but now account for gap removal)
c
         nvec=(iflags(8)-1)/2+2
         DO j=1,nvec-1
            iery(j)= 1
         ENDDO
         iery(nvec)=0
      ENDIF
c     
c     plot array to plot
c     iery array pointer to error dimension 
c     m=1 xaxis + n yaxis + last (iery (m) value all set to 1 but last =0) 
c     mxrow =nanal, npts =mxrow, nvec=xaxis+yaxis
c     cmd (array of commands) ncmd number of commands in cmd array)
c     
      do iv = 1,ncmd
         call xwrite(cmd(iv),15)
      enddo
      CALL PLT (plot, iery, npsize, npts, nvec, cmd, ncmd, ier)
      IF (ier.NE.0) THEN
         CALL XWRITE(' PLT exit with ier different from zero',5)
      ENDIF
c
c     PLT can leave open logical units when plot script contains
c     exit command, which can cause problems with future PLT calls. 
c     Cleanup with GTDEST.
c
      CALL GTDEST

      RETURN
      END
      


