
      subroutine nsatmos (ear,near,param,ifl,photar,photer)

      implicit none

      INTEGER near, ifl
      REAL ear(0:near),param(4),photar(near),photer(near)

c     Subroutine for use with XSPEC

c     Outputs a model NS atmosphere spectrum as calculated by George
c     Rybicki for given input parameters

c     The array ear(0:near) contains the energy values of the bins
c     The array param(4) gives the input parameter values:
c         param(1) = log(effective temperature) at the surface
c         param(2) = Mass of the NS in solar mass units
c         param(3) = Radius in km
c         param(4) = Distance to the NS in kpc

c     The output spectrum, in counts per bin, is given in photar (near)

c     The subroutine reads in a grid of model spectra from the datafile
c     nsatmos.dat in the first pass.  It then saves the information for
c     later passes.

      INTEGER nparT, nparg, nparm, nfreq
      parameter (nparT=11,nparg=26,nparm=8,nfreq=64)

      REAL Tlog(nparT),gravlog(nparg),critmu(nparm),
     &     EkeV(nfreq),fluxl(nparT,nparg,nparm,nfreq)
      REAL flTg(2,2,nfreq),flT(2,nfreq),fl(nfreq),Eshft(nfreq),
     &     countsl(nfreq),countsl2(nfreq)

      REAL Tl, eMsun, RS, radius, RoverRS, Dkpc, RoverDsql
      REAL zred1, gravity, glog, sinthetac, cmu, wTlo, wThi
      REAL wglo, wghi, wclo, wchi, Tfactor, fluxshift
      REAL fluxfactorlog, countsfactorlog, deriv1, derivN
      REAL e1, e2, e3, e4, e5
      REAL c1, c2, c3, c4, c5

      INTEGER ilun, nread, nmu, nf, ngrav, ntemp
      INTEGER n, l, i, j, k, iT, ig, ic, ios, lfil

      character(255) datdir, filenm, outstr
      character(128) pname

      integer lenact
      character(255) fgmodf, fgmstr
      external lenact, fgmodf, fgmstr

      save Tlog,gravlog,critmu,EkeV,fluxl,nTemp,ngrav,nmu,nf,nread

      data nread/0/

c suppress a warning message from the compiler
      i = ifl

c this model does not calculate errors
      DO i = 1, near
         photer(i) = 0.0
      ENDDO

c Open and read nsatmos.dat the first time round.  The information is saved

      if (nread.ne.99) then

         pname = 'NSATMOS_FILE'
         filenm = fgmstr(pname)
         lfil = lenact(filenm)

         IF ( lfil .EQ. 0 ) THEN
            datdir = fgmodf()
            filenm = datdir(:lenact(datdir))//'nsatmos.dat'
         ENDIF

         CALL getlun(ilun)
         CALL openwr(ilun, filenm, 'old', ' ', ' ', 0, 0, ios)
         IF ( ios .NE. 0 ) THEN
            outstr = 'NSATMOS : failed to open '//
     &               filenm(:lenact(filenm))
            CALL xwrite(outstr, 10)
            RETURN
         ENDIF
         outstr = 'Opened and reading '//
     &               filenm(:lenact(filenm))
         CALL xwrite(outstr, 10)
         rewind (ilun)

c     nTemp is the number of temperatures covered by the grid and
c     Tlog(n) are the corresponding log(temperature) values

         read (ilun,*,iostat=ios) nTemp
         IF ( ios .NE. 0 ) THEN
            outstr = 'NSATMOS : failed to read '//
     &               filenm(:lenact(filenm))
            CALL xwrite(outstr, 10)
            RETURN
         ENDIF
         if (nTemp.gt.nparT) then
            CALL xwrite(' Too many temperatures !! ', 10)
            RETURN
         endif
         read (ilun,*) (Tlog(n),n=1,nTemp)

c     ngrav is the number of gravities covered by the grid and
c     gravlog(n) are the corresponding log(gravity) values

         read (ilun,*) ngrav
         if (ngrav.gt.nparg) then
            CALL xwrite(' Too many gravities !! ', 10)
            RETURN
         endif
         read (ilun,*) (gravlog(n),n=1,ngrav)

c     nmu is the number of mu_crit values covered by the grid and
c     critmu(n) are the corresponding mu_crit values

         read (ilun,*) nmu
         if (nmu.gt.nparm) then
            CALL xwrite(' Too many mu_crits !! ', 10)
            RETURN
         endif
         read (ilun,*) (critmu(n),n=1,nmu)

c     nf is the number of frequencies, or photon energies, in the model
c     spectra and EkeV(l) are the photon energies in keV

         read (ilun,*) nf
         if (nf.gt.nfreq) then
            CALL xwrite (' Too many frequencies !! ', 10)
            RETURN
         endif
         read (ilun,*) (EkeV(l),l=1,nf)

c     Read in model spectra [fluxl(i,j,k,l), l=1,nf] for the entire grid
c     of models.  Note that all the spectra have been artifically scaled
c     to a nominal temperature of 10^6 K to enable more accurate
c     interpolation.

         do 10 i=1,nTemp
         do 10 j=1,ngrav
         do 10 k=1,nmu
            read (ilun,*) (fluxl(i,j,k,l),l=1,nf)
 10      continue

         close (ilun)
         CALL frelun(ilun)
         nread=99

      endif

c     Identify parameter values and calculate various related quantities
c        RoverRS is the stellar radius in Schwarzschild units
c        zred1 = 1+z (redshift)
c        glog = log(gravity) in cgs units
c        cmu = mu_crit for the current model

      Tl=param(1)

      eMsun=param(2)

      RS=2.95e5*eMsun
      radius=1.e5*param(3)
      RoverRS=radius/RS
      if (RoverRS.lt.1.125) go to 170

      Dkpc=param(4)

      RoverDsql=2.*log10(radius/(3.09e21*Dkpc))

      zred1=1./sqrt(1.-(1./RoverRS))

      gravity=(6.67e-8*1.99e33*eMsun/radius**2)*zred1
      glog=log10(gravity)

      if (RoverRS.lt.1.5e0) then
         sinthetac=2.598/(RoverRS*zred1)
         cmu=sqrt(1.-sinthetac**2)
         cmu=sqrt(1.-6.75/RoverRS**2+6.75/RoverRS**3)
      else
         cmu=0.d0
      endif

c      write (*,*) ' radius, 1+z, glog, mucrit = ',radius,zred1,glog,cmu

c     Find location of the current parameters within the grid of models,
c     and compute weights for linear interpolation in the grid.  The
c     model lies between iT and iT+1 along the temperature axis, between
c     ig and ig+1 along the gravity axis, and between ic and ic+1 along
c     the mu_crit axis.

      it = 1
      if (Tl.lt.Tlog(1)) then
         iT=1
      elseif (Tl.ge.Tlog(nTemp)) then
         iT=nTemp-1
      else
         do 20 i=1,nTemp-1
            if (Tl.ge.Tlog(i).and.Tl.lt.Tlog(i+1)) then
               iT=i
               go to 30
            endif
 20      continue
      endif

 30   continue
      wTlo=(Tlog(iT+1)-Tl)/(Tlog(iT+1)-Tlog(iT))
      wThi=1.-wTlo

      ig = 1
      if (glog.lt.gravlog(1)) then
         ig=1
      elseif (glog.ge.gravlog(ngrav)) then
         ig=ngrav-1
      else
         do 40 i=1,ngrav-1
            if (glog.ge.gravlog(i).and.glog.lt.gravlog(i+1)) then
               ig=i
               go to 50
            endif
 40      continue
      endif

 50   continue
      wglo=(gravlog(ig+1)-glog)/(gravlog(ig+1)-gravlog(ig))
      wghi=1.-wglo

      ic = 1
      if (RoverRS.ge.1.5) then
         ic=1
      elseif (cmu.ge.critmu(nmu)) then
         ic=nmu-1
      else
         do 60 i=1,nmu-1
            if (cmu.ge.critmu(i).and.cmu.lt.critmu(i+1)) then
               ic=i
               go to 70
            endif
 60      continue
      endif

 70   continue
      wclo=(critmu(ic+1)-cmu)/(critmu(ic+1)-critmu(ic))
      wchi=1.-wclo

c     Interpolate first on mu_crit.  Since most often we expect
c     mu_crit=0 (corresponding to R/RS >= 1.5), we check for this first.

      if (RoverRS.ge.1.5) then

         do 80 l=1,nf
            flTg(1,1,l)=fluxl(iT,ig,1,l)
            flTg(1,2,l)=fluxl(iT,ig+1,1,l)
            flTg(2,1,l)=fluxl(iT+1,ig,1,l)
            flTg(2,2,l)=fluxl(iT+1,ig+1,1,l)
 80      continue

      else

         do 90 l=1,nf
            flTg(1,1,l)=wclo*fluxl(iT,ig,ic,l)+
     &           wchi*fluxl(iT,ig,ic+1,l)
            flTg(1,2,l)=wclo*fluxl(iT,ig+1,ic,l)+
     &           wchi*fluxl(iT,ig+1,ic+1,l)
            flTg(2,1,l)=wclo*fluxl(iT+1,ig,ic,l)+
     &           wchi*fluxl(iT+1,ig,ic+1,l)
            flTg(2,2,l)=wclo*fluxl(iT+1,ig+1,ic,l)+
     &           wchi*fluxl(iT+1,ig+1,ic+1,l)
 90      continue

      endif

c     Interpolate next on glog

      do 100 l=1,nf
         flT(1,l)=wglo*flTg(1,1,l)+wghi*flTg(1,2,l)
         flT(2,l)=wglo*flTg(2,1,l)+wghi*flTg(2,2,l)
 100      continue

c     Finally interpolate on Tlog

      do 110 l=1,nf
         fl(l)=wTlo*flT(1,l)+wThi*flT(2,l)
 110  continue

c     The spectrum fl(l) calculated above gives flux/keV scaled
c     artifically to log(T)=6.0.  Shift the photon energies and fluxes
c     back to the correct local temperature

      Tfactor=10.**Tl/1.e6
      fluxshift=3.*log10(Tfactor)
      do 120 l=1,nf
         Eshft(l)=EkeV(l)*Tfactor
         fl(l)=fl(l)+fluxshift
 120  continue

c     Next shift to infinity by applying the appropriate redshift
c     factor.  The photon energy is divided by (1+z).  The flux is also
c     divided by (1+z) because the luminosity goes down by (1+z)^2.

      fluxshift=-log10(zred1)
      do 130 l=1,nf
         Eshft(l)=Eshft(l)/zred1
         fl(l)=fl(l)+fluxshift
 130  continue

c     Convert to counts/keV (which corresponds to dividing by
c     1.602e-9*EkeV), multiply by the area of the star, and calculate
c     the count rate at the observer

      fluxfactorlog=RoverDsql
      countsfactorlog=-log10(1.602e-9)

      do 140 l=1,nf
         fl(l)=fl(l)+fluxfactorlog
         countsl(l)=fl(l)+countsfactorlog-log10(Eshft(l))
 140  continue

c     Calculate the spline coefficients countsl2 for the array countsl

      deriv1=1.e32
      derivN=1.e32
      call NRspline (Eshft,countsl,nf,deriv1,derivN,countsl2)

c     We are finally ready to calculate the counts per bin corresponding
c     to the input energy array ear(0:near).  We use Simpson's
c     five-point formula.  This is probably overkill --- trapozoidal
c     rule should be enough --- but we do it just to be safe.

      do 160 i=1,near

c     Calculate the energies and count rates at five equally spaced
c     points within the current energy bin

         e1=ear(i-1)
         if (e1.lt.Eshft(1).or.e1.gt.Eshft(nf)) then
            c1=0.
         else
            call NRsplint(Eshft,countsl,countsl2,nf,e1,c1)
            c1=10.**c1
         endif

         e2=0.75*ear(i-1)+0.25*ear(i)
         if (e2.lt.Eshft(1).or.e2.gt.Eshft(nf)) then
            c2=0.
         else
            call NRsplint(Eshft,countsl,countsl2,nf,e2,c2)
            c2=10.**c2
         endif

         e3=0.5*ear(i-1)+0.5*ear(i)
         if (e3.lt.Eshft(1).or.e3.gt.Eshft(nf)) then
            c3=0.
         else
            call NRsplint(Eshft,countsl,countsl2,nf,e3,c3)
            c3=10.**c3
         endif

         e4=0.25*ear(i-1)+0.75*ear(i)
         if (e4.lt.Eshft(1).or.e4.gt.Eshft(nf)) then
            c4=0.
         else
            call NRsplint(Eshft,countsl,countsl2,nf,e4,c4)
            c4=10.**c4
         endif

         e5=ear(i)
         if (e5.lt.Eshft(1).or.e5.gt.Eshft(nf)) then
            c5=0.
         else
            call NRsplint(Eshft,countsl,countsl2,nf,e5,c5)
            c5=10.**c5
         endif

c     Simpson's five-point formula for the integral within the bin

         photar(i)=(14.*(c1+c5)+64.*(c2+c4)+24.*c3)*
     &        (ear(i)-ear(i-1))/180.

 160  continue

      return

 170  do 180 i=1,near
         photar(i)=1.e10
 180  continue
      return

      end




c     Compute spline interpolation coefficients: from Numerical Recipes,
c     by Press, Flannery, Teukolsky, Vetterling

      SUBROUTINE NRspline(x,y,n,yp1,ypn,y2)

      IMPLICIT NONE

      INTEGER n,NMAX
      REAL yp1,ypn,x(n),y(n),y2(n)
      PARAMETER (NMAX=500)
      INTEGER i,k
      REAL p,qn,sig,un,u(NMAX)
      if (yp1.gt..99e30) then
        y2(1)=0.
        u(1)=0.
      else
        y2(1)=-0.5
        u(1)=(3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
      endif
      do 11 i=2,n-1
        sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
        p=sig*y2(i-1)+2.
        y2(i)=(sig-1.)/p
        u(i)=(6.*((y(i+1)-y(i))/(x(i+
     *1)-x(i))-(y(i)-y(i-1))/(x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*
     *u(i-1))/p
11    continue
      if (ypn.gt..99e30) then
        qn=0.
        un=0.
      else
        qn=0.5
        un=(3./(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
      endif
      y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.)
      do 12 k=n-1,1,-1
        y2(k)=y2(k)*y2(k+1)+u(k)
12    continue
      return
      END



c     Carry out spline interpolation using previously computed
c     coefficients: from Numerical Recipes, by Press, Flannery,
c     Teukolsky, Vetterling

      SUBROUTINE NRsplint(xa,ya,y2a,n,x,y)
      INTEGER n
      REAL x,y,xa(n),y2a(n),ya(n)
      INTEGER k,khi,klo
      REAL a,b,h
      klo=1
      khi=n
1     if (khi-klo.gt.1) then
        k=(khi+klo)/2
        if(xa(k).gt.x)then
          khi=k
        else
          klo=k
        endif
      goto 1
      endif
      h=xa(khi)-xa(klo)
      if (h.eq.0.) write(*,*) 'bad xa input in splint'
      a=(xa(khi)-x)/h
      b=(x-xa(klo))/h
      y=a*ya(klo)+b*ya(khi)+((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**
     *     2)/6.
      return
      END
