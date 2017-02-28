C
CH1  Routine Name:  SPECMAT
C  @(#) specmat.f 1.9@(#)
CH1
CH1  Version: 1.00                  Date: 7 August 1990
CH1
CH1  Programmer(s) and Completion Date:
CH1     Mark Fardal - Stanford University - 7 August 1990
CH1     Patrick Nolan - converted to SunOS - January 1991
CH1        "      "   - released version 2.0 including pulsar
CH1                       and likelihood capability - May 1992
CH1        - greatly reorganized version 2.7 - Dec 1994
CH1
CH1  Function:  Interpolate from the calibration files to get a 
CH1     instrument response matrix for a particular observation.
CH1     This matrix times a vector representing a hypothetical
CH1     source spectrum produces the binned expected response
CH1     for the observation.  Data on the source position 
CH1     and time intervals considered comes from a SELECT file
CH1     or from the output file produced by Jim Chiang's
CH1     likelihood analysis program; the
CH1     Timeline and Exposure History files are also consulted
CH1     to get the orientations of the spacecraft and active times
CH1     in the various viewing modes.  Acceptance cone angles are
CH1     contained in the SPECTRAL default settings file.
CH1     If the SELECT file contains pulsar phase values, they can
CH1     be used to subtract off-pulse background from a pulsed spectrum.
CH1
CH1  Software System and Spacecraft:  SPECTRAL, EGRET project
CH1
CH1  Computer and Language:  VAXSTATION II - VAX FORTRAN V4.0
CH1
CH2  Calling Sequence:  not applicable for main program
CH2
CH2  Calls:
CH2   OBSDAT: Read basic observational data from input file
CH2   FKNIF: Empirical "Kniffen" correction factors for exposure
CH2   TIMELN: Reads Timeline file for orientations
CH2   ORNCOMB: Combine adjacent "orientation" intervals if possible
CH2   EXHIST: Reads Exposure file for time in viewing modes
CH2   LINTERP: Gets theta, phi interpolation coeffs
CH2   ALTMODE: Determines symmetric obs. mode when possible
CH2   DETEFF_INTERP: Interpolation in angle for sensitive area
CH2   DETRES_INTERP: Interpolation in angle for energy resolution
CH2   PSF_INTERP: Interpolation in angle for point spread function
CH2   PSF_INTEGRATE: Makes table of angular acceptance prob's
CH2   fn DETEFF: Value of sensitive area
CH2   RESPFUN: Interpolates energy resolution in incident energy
CH2   CSINT: Integrates a function
CH2   CELGALD: Convert coordinates from galactic to celestial system
CH2   SCALEF: Empirical area correction ("Kumar") factors
CH2   WRITEMAT: Writes resulting response matrix in FITS file
CH2                                                   
CH3  COMMON Block Name:  TABLES (Values of energy, angle in cal'n files)
CH3   Variable
CH3     Name            Type                   Definition
CH3  -----------       -------  ---------------------------------------------
CH3    E_TABLE(20)      Real    Incident energy values in calibration files
CH3    THETA_TABLE(9)   Real    Tip angle values in calibration files
CH3
CH3  Significant Local Variables:
CH3  Variable                Type                Description
CH3  --------                ----     --------------------------------
CH3  NREGNS                 Integer   Number of acceptance cones
CH3  IMOD                   Integer   Viewing mode number
CH3  ITHLO, IPHLO           Integer   Index to theta and phi of source
CH3  NINTS                  Integer   Number of selected time intervals
CH3  NORNS                  Integer   Number of constant-orientation
CH3                                     time intervals
CH3  NMODES                 Integer   Number of observation modes
CH3  EINC(FIRST-1:LAST)      Real     Incident energy bin boundary
CH3  EOBS(0:JMAX)            Real     Observed energy bin boundary
CH3  EC(FIRST:LAST)          Real     Center of incident energy bin
CH3  RESP(FIRST:LAST,0:JMAX) Real     Instrument response matrix
CH3  AREA_A(20)              Real     Class A sensitive area
CH3  AREA_C(20)              Real     Class C sensitive area
CH3  CONETHETA(0:JMAX)       Real     Acceptance cone angles
CH3  EREGN(0:JMAX)           Real     Energy intervals for cones
CH3  CUTANGLE(0:JMAX)        Real     Zenith cutoff angles for bins
CH3  FUDGE,SFUDGE(0:JMAX)    Real     Empirical "kniffen" factors
CH3  ACCPROB(FIRST:LAST,0:JMAX) Real  Table of angular acceptance probs
CH3  ORN(2,2,NORNMAX)        Real     Spacecraft orientation (ra/dec,
CH3                                    Z/X,interval number)
CH3  TMODE(MODEMAX,0:JMAX)   Real     Time spent in viewing mode per band
CH3  EMIN, EMAX              Real     Measured energy range
CH3  THETA                   Real     Tip angle in spacecraft coords
CH3  PHI                     Real     Azimuth angle in spacecraft coords
CH3  U, V                    Real     Interpln coeffs for theta and phi
CH3  CUTOFF                  Real     Zenith cut if not energy-dependent
CH3  SCALEFAC                Real     Empirical "Kumar" area correction
CH3  TS                      Real     Time in seconds
CH3  AREATIME                Real     Effective area*time
CH3  SRCLON, SRCLAT          Real     RA/DEC of source
CH3  THETAMAX                Real     Maximum angular devn from source
CH3  RES_TABLE(100,20)      Real*8    Energy resolution table
CH3  RESOLN(100)            Real*8    En res for a true energy value
CH3  ERESOV(20)             Real*8    En res overflow bin table
CH3  OVERFLOW               Real*8    Overflow bin for true energy value
CH3  EROW(100)              Real*8    X-values for en res table
CH3  INTTIME(2,NINTMAX)     Real*8    Selected time intervals
CH3  ORNTIME(2,NORNMAX)     Real*8    Constant-orientation time intervals
CH3  ORNVIEW(NORNMAX)       Char*4    Viewing period for each orientation
CH3  CALSET(NORNMAX)        Char*2    Which set of calibration modes
CH3  PULSAR                 Char*100  Pulsar or DC mode?
CH3  PSTYLE                 Char*100  If pulsar, new or old style file?
CH3  KDONE                  Char*30   Is Kniffen correction already done?
CH3  CELEQ                  Logical   Celestial or Galactic coords
CH3
CH3  Significant Local Constants found in spectral.inc:
CH3     Name       Type                   Definition
CH3  -----------  -------  ---------------------------------------------
CH3  NINTMAX      Integer  Maximum number of time intervals
CH3  NORNMAX      Integer  Maximum number of constant-orientn intervals
CH3  JMAX         Integer  Maximum number of observed energy bands
CH3  FIRST,LAST   Integer  Bounds of true-energy array
CH3
CH4  Method:
CH4    Read input files to get observation data
CH4    Set up energy bins
CH4    Loop over orientations:
CH4      Loop over viewing modes:
CH4        Interpolate in angle to get instrument functions
CH4        Loop over incident energies:
CH4          Calculate class C response, add to matrix
CH4        End loop
CH4        Loop over incident energies:
CH4          Loop over bins:
CH4            Integrate class A response over bin, add to matrix
CH4          End loop
CH4        End loop
CH4      End loop
CH4    End loop  
CH4    Write out matrix
CH4
CH4  Requirements, Assumptions, Comments:
CH4    The environmental variables SELECTFILE, RESPONSE, EVCLASS,
CH4      CALIB_DIR, CONES, EXHIST, PULSAR, PSTYLE, PSRCOMBO,
CH4      EGRET_PROGRAMS, TIMELINE, and USERNAME must be defined.
CH4    Class C events are denoted by bin number 0.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

c      program matrix
      subroutine specmat

      implicit none

*     PARAMETERS
      include '../SPECMAT_COMMON/spectral.inc'   
			!  for first,last,spacing,jmax,nornmax,nintmax

*     COMMON BLOCKS
      real e_table(20), theta_table(9), sind, cosd
      common/tables/e_table,theta_table

      EXTERNAL  sind, cosd

      save


*     VARIABLES
      integer i,j,k,l,nregns,ithlo,iphlo,ierr,nints,norns,
     >     kz,iret,nmodes,clobber,status
      integer iclass, evclass
*      integer ieee_handler
      real einc(first-1:last),eobs(0:jmax),ec(first:last),
     >     resp(first:last,0:jmax),area_a(20),
     >     area_c(20),
     >     eregn(0:jmax),conetheta(0:jmax),cutangle(0:jmax),
     >     fudge(0:jmax),sfudge(0:jmax),
     >     accprob_a(first:last,0:jmax),accprob_c(first:last,0:jmax),
     >     orn(2,2,nornmax),latY,lonY,
     >     tmode(modemax,0:jmax)
      real emin,emax,lnemin,lnemax,deltae,expos,
     >     u,v,theta,phi,ts,areatime,area,f,
     >     srclon,srclat,thetamax,srcx,srcy,srcz,
     >     slat,slon,cutoff
      real scalefac(0:jmax)
      real*8 res_table(100,20),resoln(100),eresov(20),
     >     erow(100),inttime(2,nintmax),orntime(2,nornmax) 
      real*8 overflow,ai,bi,result
      logical celeq,orntasc(nornmax)
      character(4) ornview(nornmax)
      character(2) calset(nornmax)
      character(100) pulsar,pstyle
      character(30) kdone,noclassb
c      character(16) out
      character(80) misc_dir, data_dir, calib_dir, cal_bin_dir
      character(80) output_dir, exphistfilebase, selfile
      character(80) scalefacfile, conefile, outputfile
      character(80) rmffile, phafile, objname
      real todeg
      integer  ind1, ind2

* VERSION
      character(12) version

*     FUNCTIONS
      real deteff

*------------------------------------------------------------------------------*

                       
      data e_table/15.,20.,30.,35.,50.,60.,70.,100.,150.,200.,300.,
     >     500.,700.,1000.,2000.,3000.,4000.,6000.,7000.,10000./

      data theta_table/0.,5.,10.,15.,20.,25.,30.,35.,40./

      version  = 'Version 2.12'
      pulsar   = 'DC'
      kdone    = 'T'
      noclassb = ' '

*      i = ieee_handler("set","common",%val(2))

      todeg = 45.0/atan(1.0)

c  Read input from input parameter file
      status = 0
      call read_input(misc_dir, data_dir, calib_dir, cal_bin_dir, 
     *		      output_dir, exphistfilebase, outputfile, selfile, 
     *		      scalefacfile, rmffile, objname, evclass, clobber, 
     *		      status)

      ind1 = index(output_dir, ' ') - 1
      ind2 = index(outputfile, ' ') - 1
      outputfile = output_dir(1:ind1) // 
     *		outputfile(1:ind2) // ".fits"

      call FileExists(outputfile, clobber, status)
      if (status .ne. 0) return

*GET DATA REGARDING OBSERVATION

      call obsdat(nregns,pulsar,pstyle,emin,emax,nints,inttime,
     >     celeq,srclon,srclat,thetamax,cutoff,
     >     cutangle,eregn,conetheta,data_dir,misc_dir,selfile,conefile)


*     calculate Kniffen factors for low-energy bins

csb-02/07      call getenv('KNIFFEN_DONE',kdone)

c      if (kdone(1:1).eq.'T'.or.kdone(1:1).eq.'t') then
c	 write (*,*) 'Not doing Kniffen factor correction.'
         do i = 0,nregns
            fudge(i) = 1.
            sfudge(i) = 0.
         end do
c      else
c	 write (*,*) 'Doing Kniffen factor correction.'
c         do i = 1,nregns
c            call fknif(eregn(i-1),eregn(i),fudge(i),sfudge(i))
c         end do
c         call fknif(30.,eregn(0),fudge(0),sfudge(0))
c      end if

csb-02/07      call getenv('EVCLASS',evclass)
csb-02/07      call getenv('NOCLASSBFUDGE', noclassb)

c      if (evclass.eq.2.and.noclassb(1:1).eq.' ') then
      if (evclass.eq.2) then
c	 write (*,*) 'Correcting for Class B energy error.'
	 do i=1,nregns
	    call fclassb(eregn(i-1),eregn(i),f)
	    fudge(i) = fudge(i) * f
         end do
	 call fclassb(30.,eregn(0),f)
	 fudge(0) = fudge(0) * f
      end if
      if (emax.gt.30000.) emax = 30000.

C     change source coords to RA/DEC if necessary
      if (celeq) then
         slat = srclat
         slon = srclon
      else
         call celgald('GC',slon,slat,srclon,srclat,iret)
      end if

      write (6,*) ' Time intervals:'
      do j = 1,nints
         write (6,*) j,real(inttime(1,j)),real(inttime(2,j)),
     >        real(inttime(2,j)-inttime(1,j))
      end do

* Read timeline file to find the necessary orientations.
      call timeln(inttime,nints,orntime,ornview,orntasc,orn,
     >      calset,norns,misc_dir)

      write (*,*) 'norns=',norns
      write (6,*) 
      write (6,*) 'Orientation intervals:'
      do j = 1,norns
         write (6,*) j,orntime(1,j),orntime(2,j),'  ',ornview(j),
     >      '  ',orntasc(j),' ',calset(j)
      end do

*SET UP THE OBSERVED ENERGY BINS.
      lnemin=log(emin)
      lnemax=log(emax)
      deltae=(lnemax-lnemin)/nregns

      do j = 0,nregns
         eobs(j) = eregn(j)  ! max energy of bin j
      end do

*INCIDENT BINS:
      deltae=(lnemax-lnemin)/spacing
      expos = exp(-deltae/2)
      einc(first-1) = exp(lnemin + deltae*(first-1))
      do i = first,last
         einc(i) = exp(lnemin + deltae*i)
         ec(i) = expos*einc(i)
      end do

* combine adjacent intervals with same pointing direction. 
         call orncomb(norns,orn,ornview,orntasc,orntime,calset)

*INITIALIZE THE MATRIX:
      do i= first,last
         do j=0,nregns
            resp(i,j)=0.0
         end do
      end do

*LOOP OVER ALL ORIENTATIONS
      do l = 1,norns

*INTERPOLATE FROM DATA FILES TO GET SENSITIVE AREA (A AND C), 
*ENERGY RESOLUTION, AND POINT SPREAD FUNCTION

c Source direction cosines in S/C XYZ coordinates
      
	 latY = cosd(orn(2,1,l))*cosd(orn(2,2,l))*  
     >        sind(orn(1,2,l)-orn(1,1,l))
         latY = todeg*asin(latY)
     	 lonY=atan2(cosd(orn(1,2,l))*sind(orn(2,1,l))*cosd(orn(2,2,l))
     >	  - cosd(orn(1,1,l))*cosd(orn(2,1,l))*sind(orn(2,2,l)) ,
     >	  sind(orn(1,1,l))*cosd(orn(2,1,l))*sind(orn(2,2,l))
     >    - sind(orn(1,2,l))*sind(orn(2,1,l))*cosd(orn(2,2,l)) )
         lonY = todeg*lonY
         srcz = sind(orn(2,1,l))*sind(slat)+cosd(orn(2,1,l))
     >        *cosd(slat)*cosd(orn(1,1,l)-slon)
         srcx = sind(orn(2,2,l))*sind(slat)+cosd(orn(2,2,l))
     >        *cosd(slat)*cosd(orn(1,2,l)-slon)
	 srcy = sind(slat)*cosd(orn(2,1,l))*cosd(orn(2,2,l))
     >        *sind(orn(1,2,l)-orn(1,1,l))
     >        +    cosd(slat)* (cosd(orn(2,1,l))*sind(orn(2,2,l))
     >        *sind(orn(1,1,l)-slon) + 
     >        sind(orn(2,1,l))*cosd(orn(2,2,l))*sind(slon-orn(1,2,l)))
         theta = todeg*acos(srcz)
         phi = todeg*atan2(srcy,srcx)
         if (phi.lt.0.) phi = phi+360.
         call linterp(theta,phi,ithlo,iphlo,u,v,calset(l))
         write (6,*) 'Orientation #',l,' out of ',norns
         write (6,*) 'Duration ',86400.d0*(orntime(2,l)-orntime(1,l)),
     >     ' seconds'
         write (6,*) 'Z axis lat,long:',orn(2,1,l),orn(1,1,l)
         write (6,*) 'X axis lat,long:',orn(2,2,l),orn(1,2,l)
	 write (6,*) 'Y axis lat,long:',latY,lonY
         write (6,*) 'Source lat,long:',slat,slon
         write (*,*) 'XYZ direction cosines:',srcx,srcy,srcz
         write (*,*) 'u,v:',u,v
         call scalef(ornview(l),scalefac,nregns,eobs,data_dir,status)
         if (status .eq. 1) return

         write (*,*) 'scale factors =',(scalefac(j),j=0,nregns)

         if (ithlo.ge.0.and.iphlo.ge.0) then
            do i = 1,modemax
               do j = 0,jmax
                  tmode(i,j) = 0.
               end do
            end do

* Read exposure history file to get live times
            call exhist(orntime,ornview,norns,tmode,cutangle,
     >           slon,slat,nregns,l,data_dir,exphistfilebase)

*    LOOP OVER ALL VIEWING MODES
            if (calset(l).eq.'00') then  ! should this be a subr?
               nmodes = 74
	    else if (calset(l).eq.'15'.or.calset(l).eq.'20') then
	       nmodes = 87
            else
               nmodes = 75
            end if
            do k = 1,nmodes
               ts = tmode(k,nregns)
               if (ts.gt.0.0) then
		  kz = k
                  if (calset(l).eq.'00') call altmode(k,kz,phi)
                  write (*,*) 'Adding mode ',k,'(',kz,')',ts,' seconds'
		  iclass = 1
                  call deteff_interp(data_dir,cal_bin_dir,calib_dir,
     >		       evclass,kz,ithlo,iphlo,u,v,area_a,iclass,
     >		       orntasc(l),calset(l))
		  iclass = 3
                  call deteff_interp(data_dir,cal_bin_dir,calib_dir,
     >		       evclass,kz,ithlo,iphlo,u,v,area_c,iclass,
     >		       orntasc(l),calset(l))
                  call detres_interp(data_dir,cal_bin_dir,calib_dir,
     >		       evclass,kz,ithlo,iphlo,u,v,res_table,erow,eresov,
     >		       orntasc(l),calset(l))
c                  if (pulsar.eq.'PULSAR') then
c                     call psf_interp(data_dir,evclass,kz,ithlo,iphlo,u,
c     *			  v,psa_table,3,orntasc(l),calset(l))
c                     call psf_interp(data_dir,evclass,kz,ithlo,iphlo,u,
c     *			  v,psc_table,1,orntasc(l),calset(l))
c                     call psf_integrate(accprob_a,conetheta,ec,first,
c     >                    last,jmax,nregns,psa_table)
c                     call psf_integrate(accprob_c,conetheta,ec,first,
c     >                    last,jmax,nregns,psc_table)
c                  else
                     do i = first,last
                        do j = 0,nregns
                           accprob_a(i,j) = 1.
                           accprob_c(i,j) = 1.
                        end do
                     end do
c                  end if

*Class C vector
                  do i = first,last
                     areatime = scalefac(0) * tmode(k,0) *
     >                    deteff(ec(i),area_c)
                     resp(i,0) = accprob_c(i,0)*areatime
                  end do
               
*Class A matrix      
                  do i = first,last
                     area = deteff(ec(i),area_a)
c     interpolate in true energy for resoln, overflow
                     call respfun(res_table,resoln,eresov,overflow,
     >                    e_table,ec(i))
                     do j = 1,nregns
                        areatime = scalefac(j) * area * tmode(k,j)
                        ai = eregn(j-1)/ec(i)
                        bi = min(eregn(j)/ec(i),2.)
                        if (bi.gt.ai) then
                           call csint(erow,resoln,100,
     >                          ai,bi,0,result,ierr)
                        else
                           result = 0.d0
                        end if
                        result = result*areatime*50. ! Width of chan in EDP
c                        resp(i,j) = resp(i,j) + result*accprob_a(i,jr)
                        resp(i,j) = resp(i,j) + result*accprob_a(i,j)
                     end do
                  end do
               end if
            end do
            
         end if
      end do

* Write out the matrix
      call writemat(norns,orntime,nregns,celeq,srclon,srclat,cutoff,
     *	   orntasc,resp,eregn,conetheta,cutangle,fudge,sfudge,einc,
     *     pulsar,pstyle,evclass,data_dir,selfile,outputfile)

      phafile = rmffile
      if (rmffile .ne. 'NONE' .and. rmffile .ne. 'none' .and.
     *	  rmffile(1:1) .ne. ' ') then

c Convert EGRET .resp file into FITS file in XSPEC format

         ind1 = index(output_dir, ' ') - 1
         ind2 = index(rmffile, ' ') - 1 
         rmffile = output_dir(1:ind1) // 
     *			rmffile(1:ind2) // '.rmf'
         call FileExists(rmffile, clobber, status)
         if (status .ne. 0) return

	 write(*,*) 'Write to RMF file: ', rmffile
         call speconvrmf(outputfile, rmffile, evclass)
      endif

      if ((objname .ne. 'NONE') .and. (objname .ne. 'none')) then

c Convert EGRET .spec file into FITS file in XSPEC format

         ind1 = index(output_dir, ' ') - 1
         ind2 = index(phafile, ' ') - 1 
         phafile = output_dir(1:ind1) // 
     *			phafile(1:ind2) // '.pha'
         call FileExists(phafile, clobber, status)
         if (status .ne. 0) return

	 write(*,*) 'Write to PHA file: ', phafile
         call speconvpha(selfile, phafile, objname)
      endif

      return

* Prevent annoying retrospective error message reporting
* trivial floating-point errors.
*     i = ieee_flags('clear','exception','underflow',out)
*     i = ieee_flags('clear','exception','inexact',out)
      end

* Routines sind, cosd, asind, acosd, and atan2d are not available in 
* Linux.  The calls to the arc functions are easily changed by
* a conversion since there are only a few calls.  sind and cosd
* occur in several complicated expressions so it is convenient
* to define functions for these.


      function sind(a)

      real      a, torad
      parameter (torad = 0.01745329)
      
      sind = sin(torad*a)
      return
      end

      function cosd(a)

      real      a, torad
      parameter (torad = 0.01745329)
      
      cosd = cos(torad*a)
      return
      end
