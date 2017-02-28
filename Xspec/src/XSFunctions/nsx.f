      subroutine nsx(ee,ne,param,ifl,photar,photer)

c-----------------------------------------------------------------------------
c     Model neutron star spectrum for unmagnetized partially ionized atmosphere
c     (see Ho, WCG, Heinke, CO 2009, Nature, 462, 71)
c     Compatible with NSMAXG magnetic atmosphere tables
c
c     ee(0:ne) = input energy bins
c     ne = input number of energy and spectral bins
c     param(5) = (fit) parameters
c       param(1) = log(unredshifted effective temperature, in K)
c       param(2) = neutron star mass, in solar masses
c       param(3) = neutron star radius, in km
c       param(4) = distance to neutron star, in kpc
c       param(5) = model to use, see nsx.dat
c       param(x) = 1, normalization (automatically added by XSPEC)
c     ifl = (not used)
c     photar(1:ne) = output count spectrum (in counts per bin)
c     photer(1:ne) = output count spectrum uncertainty (not used)
c-----------------------------------------------------------------------------

      implicit none
      INTEGER ne, ifl
      REAL ee(0:ne), param(5), photar(ne), photer(ne)

      INTEGER MAXFILES
      PARAMETER (MAXFILES=100)

      INTEGER ilun
      INTEGER ios
      CHARACTER(255) outstr, pstring
      CHARACTER(255) datfile
      INTEGER nspfiles, ispfile(MAXFILES)
      CHARACTER(255) spfile(MAXFILES)
      CHARACTER(255) infile
      INTEGER ntemp
      INTEGER ngrav
      INTEGER nen
      REAL temp(30)
      REAL grav(20)
      REAL en(2000)
      REAL y(30,20,2000)

      INTEGER i, j, n
      REAL tt
      REAL redshift
      REAL gg
      INTEGER ifile, ifilesv
      INTEGER it1, it2
      INTEGER ig1, ig2
      REAL e1, e2, e3, e4, e5
      REAL ct1, ct2
      REAL cg1, cg2
      REAL ce1, ce2, ce3, ce4, ce5
      REAL flux
      REAL fluxgt(2000), fluxgt2(2000)
      REAL deriv1, derivN

      INTEGER lenact
      CHARACTER(255) fgmstr, fgmodf
      external lenact, fgmstr, fgmodf
      INTEGER lendir
      CHARACTER(255) pname, datdir

      LOGICAL qfirst, qnew

      SAVE qfirst, nspfiles, ispfile, spfile, datdir, lendir
      SAVE ntemp, ngrav, nen, temp, grav, en, y, ifilesv

      DATA qfirst /.TRUE./
      DATA ifilesv /0/
      DATA datdir /''/

c Included to suppress a compiler warning
      i = ifl

c If the first time or NSX_DIR has changed read the available input files 
c from nsx.dat

      pname = 'NSX_DIR'
      pstring = fgmstr(pname)
      IF ( lenact(pstring) .EQ. 0 ) pstring = datdir

      qnew = .FALSE.
      IF ( qfirst .OR. (pstring .NE. datdir) ) THEN

         datdir = pstring
         lendir = lenact(datdir)
         if (lendir.eq.0) then
            datdir = fgmodf()
            lendir = lenact(datdir)
         endif
         datfile = datdir(:lendir)//'nsx.dat'

c Read nsx.dat, which contains list of model spectra files;
c  format of nsx.dat (2 columns): index number and spectrum file

         call getlun(ilun)
         call openwr(ilun,datfile,'old',' ',' ',0,0,ios)
         if (ios.ne.0) then
            outstr = 'Cannot open list file '//datfile(:lenact(datfile)) 
            call xwrite(outstr,10)
            return
         else
            outstr = 'Reading list of model spectra files from '//
     &           datfile(:lenact(datfile))
            call xwrite(outstr,25)
         endif
         
         nspfiles = 0
         read(ilun, *, end=20) ispfile(nspfiles+1), spfile(nspfiles+1)
         do while (.TRUE.)
            nspfiles = nspfiles + 1
            read(ilun,*,end=20) ispfile(nspfiles+1), spfile(nspfiles+1)
         enddo

 20      continue
         close(ilun)

         qnew = .TRUE.
         qfirst = .FALSE.
      ENDIF

      tt = param(1)
      redshift = 1.0/sqrt(1.0-2.95316*(param(2)/param(3)))
      gg = 1.3271e16*((param(2)/param(3)**2.0)*redshift)
      gg = log10(gg)
      ifile = NINT(param(5))

c if necessary read the model spectra file

      IF ( qnew .OR. ifile .NE. ifilesv ) THEN

         infile = 'not found'
         DO i = 1, nspfiles
            IF ( ifile .EQ. ispfile(i) ) THEN
               infile = datdir(:lendir)//spfile(i)
            ENDIF
         ENDDO
         IF ( infile .EQ. 'not found' ) THEN
            WRITE(outstr,'(a,i3)') 
     & 'No file found for input specfile parameter value of ', ifile
            call xwrite(outstr,10)
            WRITE(outstr,'(a,100(i3,1x))') 'Valid options are : ', 
     &           (ispfile(i),i=1,nspfiles)
            call xwrite(outstr,10)
            return
         ENDIF

c Read model spectra file, where ifile (parameter 5) = index number

         call openwr(ilun,infile,'old',' ',' ',0,0,ios)
         if (ios.ne.0) then
            outstr = 'Cannot open '//infile(:lenact(infile))
            call xwrite(outstr,10)
            return
         else
            outstr = 'Using '//infile(:lenact(infile))
            call xwrite(outstr, 25)
         endif
         read(ilun,*) ntemp
         read(ilun,*) (temp(n),n=1,ntemp)
         read(ilun,*) ngrav
         read(ilun,*) (grav(n),n=1,ngrav)
         read(ilun,*) nen
         read(ilun,*) (en(n),n=1,nen)
         do i = 1, ntemp
            do j = 1, ngrav
               read(ilun,*) (y(i,j,n),n=1,nen)
               do n = 1, nen
                  y(i,j,n) = log10(y(i,j,n))
               enddo
            enddo
         enddo
         close(ilun)
         call frelun(ilun)

         ifilesv = ifile
      ENDIF

c Locate nearest two model temperatures and calculate weights
      if (tt.lt.temp(1)) then
        it1 = 1
        it2 = 2
        ct1 = 1.0
        ct2 = 0.0
      elseif (tt.gt.temp(ntemp)) then
        it1 = ntemp - 1
        it2 = ntemp
        ct1 = 0.0
        ct2 = 1.0
      else
        it1 = 1
        do i = 2, (ntemp-1)
          if (tt.lt.temp(i)) goto 40
          it1 = i
        enddo
 40     it2 = it1 + 1
        ct1 = (temp(it2) - tt) / (temp(it2) - temp(it1))
        ct2 = 1.0 - ct1
      endif
c Locate nearest two model gravities and calculate weights
      if (ngrav.eq.1) then
        ig1 = 1
        ig2 = 2
        cg1 = 1.0
        cg2 = 0.0
      else
        if (gg.lt.grav(1)) then
          ig1 = 1
          ig2 = 2
          cg1 = 1.0
          cg2 = 0.0
        elseif (gg.gt.grav(ngrav)) then
          ig1 = ngrav - 1
          ig2 = ngrav
          cg1 = 0.0
          cg2 = 1.0
        else
          ig1 = 1
          do i = 2, (ngrav-1)
            if (gg.lt.grav(i)) goto 50
            ig1 = i
          enddo
 50       ig2 = ig1 + 1
          cg1 = (grav(ig2) - gg) / (grav(ig2) - grav(ig1))
          cg2 = 1.0 - cg1
        endif
      endif
c Interpolated flux over temperature and gravity
      do n = 1, nen
        flux = ct1 * y(it1,ig1,n) + ct2 * y(it2,ig1,n)
        if (ngrav.gt.1) then
          flux = cg1*flux+cg2*(ct1*y(it1,ig2,n)+ct2*y(it2,ig2,n))
        endif
        fluxgt(n) = flux
      enddo
c Calculate the spline coefficients fluxgt2 for the array fluxgt
      deriv1 = 1.0e32
      derivN = 1.0e32
      call NRspline(en,fluxgt,nen,deriv1,derivN,fluxgt2)
      do n = 1, ne
        e1 = ee(n-1) * redshift
        if (e1.lt.en(1).or.e1.gt.en(nen)) then
          ce1 = 0.0
        else
          call NRsplint(en,fluxgt,fluxgt2,nen,e1,ce1)
        endif
        e2 = (0.75 * ee(n-1) + 0.25 * ee(n)) * redshift
        if (e2.lt.en(1).or.e2.gt.en(nen)) then
          ce2 = 0.0
        else
          call NRsplint(en,fluxgt,fluxgt2,nen,e2,ce2)
        endif
        e3 = (0.5 * ee(n-1) + 0.5 * ee(n)) * redshift
        if (e3.lt.en(1).or.e3.gt.en(nen)) then
          ce3 = 0.0
        else
          call NRsplint(en,fluxgt,fluxgt2,nen,e3,ce3)
        endif
        e4 = (0.25 * ee(n-1) + 0.75 * ee(n)) * redshift
        if (e4.lt.en(1).or.e4.gt.en(nen)) then
          ce4 = 0.0
        else
          call NRsplint(en,fluxgt,fluxgt2,nen,e4,ce4)
        endif
        e5 = ee(n) * redshift
        if (e5.lt.en(1).or.e5.gt.en(nen)) then
          ce5 = 0.0
        else
          call NRsplint(en,fluxgt,fluxgt2,nen,e5,ce5)
        endif
c Simpson five-point formula for integral within the bin
        flux = (14.0*(ce1+ce5)+64.0*(ce2+ce4)+24.0*ce3)/180.0
c Scale by redshift
        flux = flux - log10(redshift)
c Scale by (radius/distance)^2, where radius (in km) and distance (in kpc)
c PC=3.0856776e18 cm
        flux = flux - 32.9787011 + 2.0*log10(param(3)/param(4))
c Convert from ergs/(s cm^2 Hz) to counts/(s cm^2 keV)
c HH=6.62606957e-27 ergs s
        flux = flux + 26.1787440 - log10(ee(n))
        photar(n) = 10.0**flux
c Convert counts/(s cm^2 bin)
        photar(n) = photar(n) * (ee(n) - ee(n-1))
        photer(n) = 0.0
      enddo
      end
