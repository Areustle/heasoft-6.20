      SUBROUTINE SENSTV2(calib_dir,cal_bin_dir,ENERGY,NBRENR,GAMMA,
     *		         FILEID,EFFSAR,FLUX,IERR,caltbl,calfil)
C=======================================================================
C* Version: 2.3                        Date: 21 Nov 1994                
C*
C* Variable 'caltbl' included in order to select between 'old Calibration
C* files' (caltbl=0), 'Vertical only' .and. 'PMT C43 off' (caltbl=5)
C* as well as 'ALL modes enabled' .and. PMT C43 off' (caltbl=10). The 
C* value of 'caltbl' determines whether the calibration files contain 
C* 3 azimuth angles or 16. The FANMODE (caltbl=15 and 20) is included.
C*
C*
C* This program provides the effective sensitive area EFFSAR for all
C* trigger modes (IMOD) and all incidence angles (INCL,IAZI).
C*
C* The effective sensitive area is determined by weighting the sensitive
C* area by an assumed true sky spectrum corrected for the energy
C* dispersion in the selected energy range (ENERGY) and integrating over
C* all true energies. This preliminary eff. sens. area is then divided
C* by the flux of the assumed sky spectrum in the selected energy range.
C* For more detailed information see document: EGRET/SU/JMF/94/AUG/02
C*
C* Input:
C*   ENERGY(2,NBRENR) - real*4; array containing the lower and upper
C*                      limits of the desired energy intervals.
C*   NBRENR           - integer*4; number of energy intervals (max=10).
C*   GAMMA(NBRENR)    - real*4; array containing the spectral indices
C*                      for each energy interval.
C*   FILEID(2)        - character(2); two digit number from which
C*                      calibration file names are created.
C*   CALTBL           - indicator which kind of calibration files is
C*                      used (3 or 16 azimuths, resp)
C*			   0 = old calibration files (azimuth=3)
C*			   5 = Vertical only, C43 off (azimuth=16)
C*			  10 = All open, C43 off (azimuth=16)
C*			  15 = Fanmode, along X-axis
C*			  20 = Fanmode, along Y-axis
C*
C* Output:
C*   EFFSAR           - real*4; array containig the EFFECTIVE SENSITIVE
C*    (NBRENR,74,9,16)   AREA.
C*   FLUX(NBRENR)     - real*4; contains the flux of the given spectrum
C*   IERR             - integer*4; return code:
C*                         0 = o.k.
C*                         1 = open error for energy calibration file
C*                         2 = true energy out of range 15 MeV - 10 GeV
C*
C* Logical Units:
C*        6 - output unit for protokol
C*
C* Calls:
C*   WEIGHT - calculates the weighting factors corresponding to the
C*            probability that a photon is measured in energy
C*            interval DE for each calibration energy.
C*
C* Commons:   none
C*
C=======================================================================
C+ ISSUE: 2   STARTED: 03 AUG 1994    PROGRAMMER: J.M. FIERRO
C+ $Id: senstv2.f,v 3.2 2013/05/21 19:08:27 irby Exp $
C+ $Id: senstv2.f,v 3.2 2013/05/21 19:08:27 irby Exp $
C=======================================================================
C%  CHANGES:
C%
C+ $Log: senstv2.f,v $
C+ Revision 3.2  2013/05/21 19:08:27  irby
C+ Change character*n to character(n) to silence warnings: "Obsolescent
C+ feature: Old-style character length".
C+
C+ Revision 3.1  2002/04/16 20:32:13  irby
C+ Additions to libgro - previously these codes existed in the following
C+ libraries:
C+
C+   libsenstv
C+   libsysutil
C+   libutil
C+   libftio
C+
c Revision 1.1  1997/01/23  17:01:32  programs
c Initial revision
c
c Revision 1.2  1996/08/15  17:57:57  programs
c implemented FAN-modes
c
c Revision 2.1  1991/09/09  17:42:41  nancy
c First controlled version on the Sun.
c
C%  21 Nov 1994 BY CVM: -variable CALTBL included  
C%                      -set number of azimuth angles according to CALTBL
C%                      -variable beginning for loop over viewing modes
C%                       (if EGRET is in 'vertical only' mode there are
C%                       only two modes possible at all: all closed (imod=1)
C%                       or all open (imod=74). Therefore, save time and 
C%                       do loop only for imod=74).
C%  07 Dec 1994 BY CVM: Don't know how changing the loop variable 'iazi'
C%                      in line 206 could ever work. Therefore: different
C%                      approach (see lines 148 - 150) 
C%  12 Dec 1994 BY CVM: take care of increased number of viewing modes
C%                      in new calibration files (i.e. IMOD=75 
C%                      corresponds to 'vertical only') by reading the
C%                      appropriate records from the calibration files.
C%  09 Jan 1995 BY CVM: cosmetics
C%  05 Oct 1995 BY CVM: Fanmodes included
C%  31 Jul 2001 by DLB: Linux version.
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


      integer nmod, nincl, nazim, ncal, nmax, nbrenr, ierr
      parameter(nmod=74,nincl=9,ncal=20,nmax=10)
      real*4 energy(2,nbrenr),gamma(nbrenr)
      real*4 effsar(nbrenr,nmod,nincl,16),flux(nbrenr)
      real*4 ienrgy(ncal+1),dstrbn(0:101,ncal),a(ncal)
      real*4 g(nmax,ncal,76),h(nmax,ncal,76)
      real*4 emid,kniffen,factor(nmax)
      real*4 e1,e2
      real*4 s1,s2,s3
      integer i,i0,n1,n2, begmod,caltbl,iincl,iiazi,ind1,ind2,ind3
      integer imod, incl, iazi, ical, ide, iaz, ios, nrec
      integer fileindex, clobber
      character(2) fileid(2)
      character(100) fname, oname(2)
      character*(*)  calfil(*), cal_bin_dir
      character(70)  tmpfnm
      character*(*) calib_dir



      save

      data ienrgy/15.,20.,30.,35.,50.,60.,70.,100.,150.,200.,300.,
     $    500.,700.,1000.,2000.,3000.,4000.,6000.,7000.,10000.,100000./

      character(80)	id
      common	/id/	id
      id = '$Id: senstv2.f,v 3.2 2013/05/21 19:08:27 irby Exp $'


      ierr=0
      NAZIM=3
      clobber = 1

c---> set number of azimuth angles and number of modes in 
c     calibration files according to value of 'caltbl':      
      IF (caltbl .ge. 5) THEN
         NAZIM=16     
      ENDIF
      
C                                OPEN ENERGY DISPERSION CALIBRATION FILE
c      call getenv('CALIB_DIR', dirnam)
      ind1 = index(calib_dir, ' ')-1
      ind2 = index(cal_bin_dir, ' ')-1
       

      fileindex = 2
      ind3 = index(calfil(2),'.fits') - 1
      oname(2) = cal_bin_dir(1:ind2) // calfil(2)(1:ind3)
      fname = calib_dir(1:ind1) // calfil(2)(1:ind3+5)


      call fileexists(oname(2), clobber, status)
      call fits2cal(fileid(2), fname, oname(2))

      open (10,file=oname(2),access='direct',form='unformatted',
     &      status='old',iostat=ios,err=8000,recl=408*20)


C                                   OPEN SENSITIVE AREA CALIBRATION FILE

      fileindex = 1
      ind3 = index(calfil(1),'.fits') - 1
      oname(1) = cal_bin_dir(1:ind2) // calfil(1)(1:ind3)
      fname = calib_dir(1:ind1) // calfil(1)(1:ind3+5)

      call fileexists(oname(1), clobber, status)
      call fits2cal(fileid(1), fname, oname(1))


      open (11,file=oname(1),access='direct',form='unformatted',
     &      status='old',iostat=ios,err=8000,recl=80)



C                CHECK ENERGY LOWER LIMITS AND DETERMINE KNIFFEN FACTORS
      do ide=1,nbrenr
         do i=1,2
            if(energy(i,ide).lt.15.) then
               write(6,*)'SENSTV2: measured energy intervals should not
     & be outside of measuring range of EGRET:'
               write(6,*)'         E =',energy(i,ide),' MeV set to 
     & E = 15 MeV'
               energy(i,ide)=15.
            endif
         enddo
         kniffen=1.0
         emid=(energy(1,ide)+energy(2,ide))/2.
         if (emid.lt.70.32) then
            kniffen=9.97*exp(-0.0327*emid)
 	    print *,' SENSTV2: Emid=',emid,'  factor=',kniffen
         endif
         factor(ide)=1/kniffen
      enddo

      call weight(g,h,energy,nbrenr,gamma,flux,ienrgy,ncal)

C-----------------------------------------------------------------------
C                                            LOOP OVER ALL VIEWING MODES
        begmod=2
	IF (caltbl .eq. 5)  begmod=74   !only mode 74 exists (all open)
 	IF (caltbl .ge. 15) begmod=63   !fanmode save memory space
      DO IMOD=begmod,NMOD
      do iincl=1,nincl
      do iiazi=1,nazim
      INCL=IINCL
      IAZI=IIAZI

      IF(IINCL.EQ.1 .AND. IIAZI.GT.1) IAZI=1
C                     READ IN APPROPRIATE RECORDS FROM CALIBRATION FILES
         if (caltbl .eq. 5) then
             nrec=incl+((iazi-1)+(imod)*nazim)*nincl
         else if (caltbl .ge. 15) then
             nrec=incl+((iazi-1)+(imod+12)*nazim)*nincl
         else 
             nrec=incl+((iazi-1)+(imod-1)*nazim)*nincl
         endif



         read(10,rec=nrec,err=9000,iostat=ios) dstrbn
c          convert from unix to linux format
csb           call reflect( dstrbn, 4, 4*102*ncal )
         read(11,rec=nrec,err=9001,iostat=ios) a
c          convert from unix to linux format
csb           call reflect( a, 4, 4*ncal )

C          FOR EACH ENERGY RANGE, USE WEIGHTS TO ADD CALIBRATION RESULTS
         do ide=1,nbrenr
            effsar(ide,imod,incl,iazi)=0.0
            e1=50.0*energy(1,ide)
            e2=50.0*energy(2,ide)

            i0=2
            do while (1.5*ienrgy(i0).le.energy(1,ide))
               i0=i0+1
            enddo
            i0=i0-1
            n2=1+e2/ienrgy(i0)
            do ical=i0,ncal-1
               n1=1+(e1/ienrgy(ical+1))
               if (n2.ge.76) n2=75
               s1=0.
               s2=0.
               s3=0.
               do i=n1,n2
                  s1=s1+g(ide,ical,i)*dstrbn(i,ical)
                  s2=s2+h(ide,ical,i)*dstrbn(i,ical)
                  s3=s3+h(ide,ical,i)*dstrbn(i,ical+1)
               enddo
               effsar(ide,imod,incl,iazi)=effsar(ide,imod,incl,iazi)+
     &          a(ical)*(s1+s3)+a(ical+1)*s2
               n2=1+(e2/ienrgy(ical))
            enddo

C                                           ADD HIGH ENERGY CONTRIBUTION
            n1=1+(e1/ienrgy(ncal+1))
            if (n2.ge.76) n2=75
            s1=0.
            do i=n1,n2
               s1=s1+g(ide,ncal,i)*dstrbn(i,ncal)
            enddo
            effsar(ide,imod,incl,iazi)=effsar(ide,imod,incl,iazi)+
     &       a(ncal)*s1

C                  EFFECTIVE AREA IS MEASURED COUNT RATE DIVIDED BY FLUX
            effsar(ide,imod,incl,iazi)=
     &       factor(ide)*effsar(ide,imod,incl,iazi)/flux(ide)
         enddo

C                       FOR INCLINATIONS OF ZERO, SET ALL AZIMUTHS EQUAL
         if (incl.eq.1) then
            do iaz=2,nazim
               do ide=1,nbrenr
                  effsar(ide,imod,incl,iaz)=effsar(ide,imod,incl,1)
               enddo
            enddo
c---> I (cvm) don't know how this could ever work! 
c           iazi=nazim
         endif

C     ------------------------------------------------------------------

      enddo
      enddo
      enddo

      close(10)
      close(11)


c      i = index(calfil(1),'.fits') - 1
      tmpfnm = 'rm  -f ' // oname(1)
      call system(tmpfnm)

c      i = index(calfil(1),'.fits') - 1
      tmpfnm = 'rm  -f ' // oname(2)
      call system(tmpfnm)

      goto 9999
c-----------------------------------------------------------------------
c                                      error end
 8000 write(6,'(''  open ret code ios:'',i3,''  filename:'',a50)')
     &      ios,oname(fileindex)
      ierr=1
      goto 9999
 9000 write(6,'(//''read error on energy calibration file: iostat='',
     & i4//)') ios
      ierr=3
      goto 9999
 9001 write(6,'(//''read error on sens. area calibration file: iostat=''
     &,i4//)') ios
      ierr=3
c-----------------------------------------------------------------------
 9999 return
      end
