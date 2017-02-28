c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c       SUBROUTINE FITRIT(ARRAY,NLONG,NLAT,CDELT,CRVAL1,CRVAL2,COORD
c     & ,NENG,VENG,BUNIT,IUNIT,FNAME,DOCVEC,NDOC,MAPTYPE_new)
C
C
C  $Id: fitrit.f,v 1.3 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C	Effect: 
C++	Specification: FITSRIT writes a 16 bit floating point FITS format   
C++	map in the EGRET format; i.e. a 3D cube where the 1st D is
C++	longitude, the 2nd D is latitude, and the 3rd D corresponds to 
C++	(possible) multiple energy selections. The number of energy
C++	selections is described by NENG, and the upper and lower limits
C++	of the energy selections are in VENG. This subroutine uses the
C++	HEASARC FITSIO subroutines. BUNIT is a 20 byte string for
C++     the physical units. Written to file FNAME on I/O unit
C++	IUNIT. If COORD='G' or 'g' galatic coordinates are indicated
C++	in the fits file; If COORD='C' or 'c' celestial (J2000) coordinates  
C++     are indicated. DOCVEC is a vector of NDOC comments which
C++	are written with the HISTORY keyword.
c 	show does CALL FITRIT(MAP,CTLMSZ1,CTLMSZ2,CTLSCL,CTLORG(1)-CTLSCL/2.,
c     &   CTLORG(2)-CTLSCL/2.,'G',1,VENG,LU(15),FNAME,MAPDOC,4)
c
c--------------------------------------------------------------------------
c     Subroutine Argument Desriptions
c   	real 	ARRAY(NLONG,NLAT,NENG)  data cube   
c     	integer		NLONG		width in pixels
c      	integer		NLAT		height in pixels
c	integer		NENG 		number of possible energy selections
c      	real		CDELT 		pixel width
c     	real		CRVAL1 		ref X
c     	real 		CRVAL2 		ref Y
c     	character	COORD*1		key for galactic/celestial coords 
c	integer		NENG 		number of possible energy selections
c 	real 	VENG(10,2)  upper and lower limits of the energy selections
c	character	BUNIT*20	physical units
c	integer		IUNIT		I/O unit
c	character	FNAME*80	name of file to write
c	character(70) 	DOCVEC(NDOC) 	vector of NDOC comments which
c					are written with the HISTORY keyword
c	integer		NDOC		number of comments in DOCVEC
c	character(20)	MAPTYPE_new	type of map
c
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED:    by  JRM
c
C=======================================================================
C  $Log: fitrit.f,v $
C  Revision 1.3  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2006/04/17 20:54:28  irby
C  Updates for compliance with g95 and gfortran:
C
C  - Replace call to lnblnk with equivalent call to len_trim.
C    lnblnk (a routine which used to live in libg2c) is not currently
C    available with g95 or gfortran.
C
C  - Change calls to "perror" (also libg2c) to fcerr or c_fcerr.
C
C  - Change calls to IDATE (libg2c) to new libgro routine GIDATE.
C
C  - Fix non-integer loop variables.
C
C  Revision 1.1  2002/04/16 20:27:30  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:47:50  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:36:56  jae
c Subroutine Module for like V5.00
c
C
c
c--------------------------------------------------------------------------

      SUBROUTINE FITRIT(ARRAY,NLONG,NLAT,CDELT,CRVAL1,CRVAL2,COORD
     &     ,NENG,VENG,BUNIT,IUNIT,FNAME,DOCVEC,NDOC,MAPTYPE_new)

c     Common blocks used:
      INCLUDE '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE '../COMMON/fitrep.copy'
      INCLUDE '../COMMON/cnfrep.copy'

      save
c
c--------------------------------------------------------------------------
      character(80) id
      common /id/id
      integer NLONG,NLAT,NENG,IUNIT,NDOC
      integer status,bitpix,naxis,naxes(3),pcount,gcount
c      logical simple,extend,tryed_once
      logical simple,extend
      character(70) DOCVEC(NDOC)
      character FNAME*80,COORD*1,IEC*1,BUNIT*20,MAPTYPE_new*20
c     character ORIGIN*8
      character DATE*10
c      character DATE*10,input*90
      real CRPIX1,CRPIX2,CDELT
      real ARRAY(NLONG,NLAT,NENG) ! data cube
      real VENG(10,2)           ! upper and lower limits of the energy selections
      real*8 BSCALE,BZERO


c     do initializations
      id = '$Id: fitrit.f,v 1.3 2013/05/21 19:08:25 irby Exp $'
      status=0
      naxis=3
      naxes(1)=NLONG
      naxes(2)=NLAT
      naxes(3)=NENG
      simple=.true.
      bitpix=16
      BZERO=0.
      pcount=0
      gcount=1
      extend=.false.
      CRPIX1=1.
      CRPIX2=1.

C     open the new FITS file
c     tryed_once=.false.

      fname = fname(1:len_trim(fname)) // '.fits'

      call FileExists(fname, clobber, status)
      if (status .ne. 0)
     &     WRITE(*,'("Something must be wrong with the ",
     &     "Output FITS file specification.")')

 100  call ftinit(iunit,FNAME,2880,status)

c     IF(STATUS.NE.0) then
c     if(STATUS.eq.105) then
c     IF(.not.tryed_once) THEN
c     call ftclos(iunit,newstatus)
c     WRITE(*,'("Output FITS file "
c     &                "probably exists already."
c     &                " Do you want to remove it?")')
c     input='/bin/rm -i '//FNAME
c     call system(input)
c     tryed_once=.true.
c     STATUS=0
c     goto 100
c     ELSE
c     WRITE(*,'("Something must be wrong with the ",
c     &                "Output FITS file specification.")')
c     goto 113
c     ENDIF
c     else
c     goto 113
c     endif
c     endif

C     Define array structure
      call ftpdef(iunit,bitpix,naxis,naxes,pcount,gcount,status)
      IF(STATUS.NE.0) then
         WRITE(6,*)'ftpdef  error'
         goto 113
      endif

C     write the required primary array keywords
      call ftpprh(iunit,simple,bitpix,naxis,naxes,pcount,gcount,
     &     extend,status)
      IF(STATUS.NE.0) then
         WRITE(6,*)'ftpprh  error'
         goto 113
      endif

c     find BSCALE
      xmin = 1.e30                                                  
      xmax = -1.e30                                                  
      DO  IE = 1,NENG
         DO  JB = 1,NLAT
	    DO  JL = 1,NLONG
               IF (ARRAY(JL,JB,IE).lt.xmin)xmin=ARRAY(JL,JB,IE) ! minimum datum
               IF (ARRAY(JL,JB,IE).gt.xmax)xmax=ARRAY(JL,JB,IE) ! maximum datum
	    ENDDO
         ENDDO
      ENDDO
      if(xmax.lt.-xmin) xmax = -xmin
      BSCALE=xmax/32000.        ! best dynamic range for 16 bits 
      call ftpscl(iunit,BSCALE,BZERO,status) ! apply BSCALE
      IF(STATUS.NE.0) then
         WRITE(6,*)' ftpscl error'
         goto 113
      endif
      
C     write the other primary array keywords
      call ftpkyf(iunit,'CRVAL1',CRVAL1,4,
     &     'Refers to the center of the pixel.',status)
      call ftpkyf(iunit,'CRPIX1',CRPIX1,2,
     &     'The column of the CRVAL1 pixel.',status)
      call ftpkyf(iunit,'CDELT1',CDELT,4,
     &     'Column interval in degrees.',status)
      call ftpkyl(iunit,'PIXCENT',.TRUE.,
     &     'CRVAL1&2 do refer to the pixel center',status)
      if(COORD.eq.'G'.or.COORD.eq.'g') then
         call ftpkys(iunit,'CTYPE1','GLON','Galactic II',status)
         call ftpkys(iunit,'CTYPE2','GLAT','Galactic II',status)
      elseif(COORD.eq.'C'.or.COORD.eq.'c') then
         call ftpkys(iunit,'CTYPE1','RA','Celestial J2000',status)
         call ftpkys(iunit,'CTYPE2','DEC','Celestial J2000',status)
      else
         call ftpkys(iunit,'CTYPE1','LON','Map longitude',status)
         call ftpkys(iunit,'CTYPE2','LAT','Map latitude',status)
      endif
      call ftpkyf(iunit,'CRVAL2',CRVAL2,4,
     &     'Refers to the center of the pixel.',status)
      call ftpkyf(iunit,'CRPIX2',CRPIX2,2,
     &     'The column of the CRVAL2 pixel.',status)
      call ftpkyf(iunit,'CDELT2',CDELT,4,
     &     'Row interval in degrees.',status)
      call ftpkys(iunit,'BUNIT',BUNIT,
     &     'Physical units.',status)
      call ftpkyd(iunit,'BSCALE',BSCALE,7,' ',status)
      call ftpkyd(iunit,'BZERO',BZERO,1,' ',status)
      if(MAPTYPE_new.eq.'Model flux') then 
         if(gmap_conv) then
            call ftpkyl(iunit,'CONVLVD',.TRUE.,
     &           'Model was convolved with the PSF',status)
         else
            call ftpkyl(iunit,'CONVLVD',.FALSE.,
     &           'Model was not convolved with PSF',status)
         endif
      endif
      if(MAPTYPE.EQ.'SINGLE_POINTING')THEN
         if(STRT_DAY.ne.0.d0)
     &        call ftpkyg(iunit,'STRT-DAY',STRT_DAY,3,' ',status)
         if(STRT_TIM.ne.0.d0)
     &        call ftpkyg(iunit,'STRT-TIM',STRT_TIM,3,' ',status)
         if(END_DAY.ne.0.d0)
     &        call ftpkyg(iunit,'END-DAY',END_DAY,3,' ',status)
         if(END_TIM.ne.0.d0)
     &        call ftpkyg(iunit,'END-TIM',END_TIM,3,' ',status)
         if(coord_sys.eq.'G') then
            if(SC_LII.ne.0.)
     &           call ftpkyf(iunit,'SC-Z-LII',SC_LII,3,' ',status)
            if(SC_BII.ne.0.)
     &           call ftpkyf(iunit,'SC-Z-BII',SC_BII,3,' ',status)
         else
            if(SC_RA.ne.0.)
     &           call ftpkyf(iunit,'SC-Z-RA',SC_RA,3,' ',status)
            if(SC_DEC.ne.0.)
     &           call ftpkyf(iunit,'SC-Z-DEC',SC_DEC,3,' ',status)
         endif
      endif
      call ftpkys(iunit,'MAPTYPE',MAPTYPE_new,' ',status)
      call ftpkys(iunit,'GRIDTYPE','RECT',' ',status)
      call ftpkys(iunit,'TELESCOP','GRO',' ',status)
      call ftpkys(iunit,'INSTRUME','EGRET',' ',status)
c     call ftpkys(iunit,'DATATYPE',
c     &    'unknown','See history.',status)
      DO IE=1,NENG
         WRITE(IEC,'(I1)')IE
         call ftpkyf(iunit,'MINENG'//IEC,VENG(IE,1),1,
     &        'MIN ENERGY OF IMAGE '//IEC,status)
         call ftpkyf(iunit,'MAXENG'//IEC,VENG(IE,2),1,
     &        'MAX ENERGY OF IMAGE '//IEC,status)
      ENDDO
c     ORIGIN='Unknown'
c     call ftpkys(iunit,'ORIGIN',ORIGIN,
c     &             '',status)
      WRITE(DATE,'(I2,"/",I2,"/",I4)')DAY,MONTH,YEAR
      call ftpkys(iunit,'DATE',DATE,
     &     'Date program began (dd/mm/yy)',status)
      call ftpkys(iunit,'FILENAME',FNAME,
     &     'Original FITS file name',status)
      IF(STATUS.NE.0) then
         WRITE(6,*)'ftpky* error'
         goto 113
      endif
      
c     write history 
      do idoc=1,ndoc
         call ftphis(iunit,docvec(idoc),status)
         IF(STATUS.NE.0) then
	    in_dex=index(docvec(idoc),'   ')-1
	    status=0
	    call ftphis(iunit,docvec(idoc)(1:in_dex),status)
	    IF(STATUS.NE.0) then
               print *,
     &              'Oops, the following HISTORY statement did not ',
     &              'make it',
     &              ' to fits:'
               print *,docvec(idoc)
               status=0
	    endif
         endif
      enddo
C     Write the data
      call ftp3de(iunit,0,NLONG,NLAT,NLONG,NLAT,NENG,ARRAY,status)
      IF(STATUS.NE.0) then
         WRITE(6,*)'ftp3de error'
         goto 113
      endif
      
C     Close the file
      call ftclos(iunit,status)
      IF(STATUS.NE.0) goto 113
      RETURN
      
 113  WRITE(6,'("FITSIO ERROR; STATUS CODE:",i4)') STATUS
      signal='F'
      sigmsg='FITSIO ERROR'
      RETURN
      END                                                               
c
