
*+WTEXM1
c     ------------------------------------------------------------
      subroutine wtexm1(ounit,telescop,instrum,detnam,filter,
     &                  deadapp,vignapp,radecsys,equinox,ctype1,
     &                  ctype2,crpix1,crpix2,cunit1,cunit2,bunit,
     &                  crval1,crval2,cdelt1,cdelt2,
     &                  map_max,exparr,nk_hist,hist,
     &                  nk_comm,comments,chatter,errflg)
c     ------------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------------
c
c This routine writes Exposure Map data in the form of a Primary Array
c It assumes the file is already open ...
c use FTCLOS to close the file OR write another extension
c Format ...
c
c HDUCLASS OGIP
c HDUCLAS1 IMAGE
c HDUCLAS2 EXPOSURE
c HDUVERS1 1.0.0
c HDUVERS2 1.0.0
c
c Additional (non-mandantory) Keywords written ...
c
c TELESCOP : Mission/Telescope name
c INSTRUME : Instrument name
c DETNAM   : Specific detector name in use
c FILTER   : Filter in use
c DEADAPP  : Deadtime correction applied
c VIGNAPP  : Vignetting correction applied
c RADECSYS : WCS for this file
c EQUINOX  : Equinox (Epoch) for this file
c CTYPE1   : Axis type for dim 1
c CTYPE2   : Axis type for dim 2
c CRPIX1   : x pixel of tangent plane direction
c CRPIX2   : y pixel of tangent plane direction
c CRVAL1   : sky coordinate of 1st axis (deg)
c CRVAL2   : sky coordinate of 2nd axis (deg)
c CUNIT1   : units of dim 1
c CUNIT2   : units of dim 2
c CDELT1   : x degrees per pixel
c CDELT2   : y degrees per pixel
c BUNIT    : Units of exposure map data
c DATAMIN  : Minimum value in image
c DATAMAX  : Maximum value in image
c
c
c --- VARIABLES --------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) telescop,instrum,detnam,filter,radecsys
      character*(*) cunit1,cunit2,ctype1,ctype2,bunit
      real equinox,crval1,crval2,cdelt1,cdelt2
      real crpix1,crpix2      
      integer ounit,nk_hist,nk_comm
      character*(*) hist(*),comments(*)
      integer map_max,chatter,errflg
      real*4 exparr(map_max,map_max)
      logical deadapp,vignapp
c
c --- CALLED ROUTINES --------------------------------------------------
c
c FTPHPR  : (FITSIO) Writes Primary array keywords
c FTPDEF  : (FITSIO) Defines primary array
c FTP2DE  : (FITSIO) Writes primary array
c FTPKYx  : (FITSIO) Writes keyword
c 
c --- AUTHORS/MODIFICATION HISTORY -------------------------------------
c
c Rehana Yusaf (Oct 5 1993) 1.0.0;
c Rehana Yusaf (Nov 11 1993) 1.0.1; Additional keywords section added
c Rehana Yusaf (Jan 22 1996) 1.0.2; add wtinfo and friends
      character(5) version
      parameter (version = '1.0.2')
      character(6) subname
      parameter (subname = 'wtexm1')
*-
c ----------------------------------------------------------------------
c
c INTERNALS ...
c
      character(70) subinfo,errinfo,exhist
      integer bitpix,naxes(2),naxis,gcount,pcount,status,i,j
      logical simple,extend
      real datamin,datamax

      
c --- USER INFO ---

      subinfo = ' using '//subname//' '//version
      call wtinfo(chatter,15,1,subinfo)

c --- DEFINE PRIMARY HEADER ---

      simple   = .true.
      bitpix   = -32
      naxis    = 2
      naxes(1) = map_max
      naxes(2) = map_max
      pcount   = 0
      gcount   = 1
      extend   = .TRUE.

c -- WRITE MANDATORY PRIMARY ARRAY KEYWORDS ---

      status = 0
      call ftphpr(ounit,simple,bitpix,naxis,naxes,pcount,gcount,
     &          extend,status) 
      errinfo = ' writing mandatory keywords'
      call wtferr(subname,version,status,errinfo)
      IF (status.NE.0) THEN
        errflg = 1
        return
      ENDIF

c TELESCOP/INSTRUM ...

      status = 0
      call ftpkys(ounit,'TELESCOP',telescop,
     &'Mission/Telescope name',status)
      errinfo = ' problem writing TELESCOP keyword'
      call wtferr(subname,version,status,errinfo)

      status = 0
      call ftpkys(ounit,'INSTRUME',instrum,
     &'Instrument name',status)
      errinfo = ' problem writing INSTRUM keyword'
      call wtferr(subname,version,status,errinfo)

      IF (filter.EQ.'   ') THEN
        filter = 'NONE'
      ENDIF
      status = 0
      call ftpkys(ounit,'FILTER',filter,
     &'Filter in use',status)
      errinfo = ' problem writing FILTER keyword'
      call wtferr(subname,version,status,errinfo) 

      IF (detnam.NE.'  ') THEN
        status = 0
        call ftpkys(ounit,'DETNAM',detnam,
     &'Specific detector name in use',status)
        errinfo = ' probelm writing DETNAM keyword'
        call wtferr(subname,version,status,errinfo)
      ENDIF

c HDUCLASS and HDUVERS ...

      status = 0
      call ftpkys(ounit,'HDUCLASS','OGIP',
     &'format conforms to OGIP standard',status)
      errinfo = ' Problem writing HDUCLASS keyword'
      call wtferr(subname,version,status,errinfo)

      status = 0
      call ftpkys(ounit,'HDUCLAS1','IMAGE',
     &'dataset is an image',status)
      errinfo = ' problem writing HDUCLAS1 keyword'
      call wtferr(subname,version,status,errinfo)

      status = 0
      call ftpkys(ounit,'HDUCLAS2','EXPOSURE',
     &'dataset is an exposure map',status)
      errinfo = ' problem writing HDUCLAS2 keyword'
      call wtferr(subname,version,status,errinfo)

      status = 0
      call ftpkys(ounit,'HDUVERS1','1.0.0',
     &'Version of family of formats',status)
      errinfo = ' writing HDUVERS1 keyword'
      call wtferr(subname,version,status,errinfo)

      status = 0
      call ftpkys(ounit,'HDUVERS2','1.0.0',
     &'Version of format',status)
      errinfo = ' writing HDUVERS2 keyword'
      call wtferr(subname,version,status,errinfo)

      subinfo = ' mandatory keywords have been defined'
      call wtinfo(chatter,20,2,subinfo)

c --- WRITE ADDITIONAL KEYWORDS ---

c deadapp ...

      status = 0
      call ftpkyl(ounit,'DEADAPP',deadapp,
     &'Deadtime correction applied',status)
      errinfo = ' writing DEADAPP keyword'
      call wtferr(subname,version,status,errinfo)

c vignapp ...

      status = 0
      call ftpkyl(ounit,'VIGNAPP',vignapp,
     &'Vignetting correction applied',status)
      errinfo = ' writing VIGNAPP keyword'
      call wtferr(subname,version,status,errinfo)

c radecsys ...

      status = 0
      call ftpkys(ounit,'RADECSYS',radecsys,
     &'WCS for this file',status)
      errinfo = ' problem writing RADECSYS keyword'
      call wtferr(subname,version,status,errinfo)

c equinox ...

      status = 0
      call ftpkye(ounit,'EQUINOX',equinox,6,
     &'Equinox (Epoch) for this WCS',status)
      errinfo = ' problem writing EQUINOX keyword'
      call wtferr(subname,version,status,errinfo)

c ctype1 ...

      status = 0
      call ftpkys(ounit,'CTYPE1',ctype1,
     &'Axis type for dim 1',status)
      errinfo = ' writing CTYPE1 keyword'
      call wtferr(subname,version,status,errinfo) 

c crpix1 ...

      status = 0
      call ftpkye(ounit,'CRPIX1',crpix1,6,
     &'x pixel of tangent plane direction ',status)
      errinfo = ' writing CRPIX1 keyword'
      call wtferr(subname,version,status,errinfo)    

c crval1 ...

      status = 0
      call ftpkye(ounit,'CRVAL1',crval1,4,
     &'sky coordinate of 1st axis (deg) ',status)
      errinfo = ' writing CRVAL1 keyword'
      call wtferr(subname,version,status,errinfo)   

c cdelt1 ...

      status = 0
      call ftpkye(ounit,'CDELT1',cdelt1,6,
     &'x degrees per pixel ',status)
      errinfo = ' writing CDELT1 keyword'
      call wtferr(subname,version,status,errinfo)       

c cunit1 ...

      status = 0
      call ftpkys(ounit,'CUNIT1',cunit1,
     &'units of dim 1 ',status)
      errinfo = ' writing CUNIT1 keyword'
      call wtferr(subname,version,status,errinfo) 

c ctype2 ...

      status = 0
      call ftpkys(ounit,'CTYPE2',ctype2,
     &'Axis type for dim 2',status)
      errinfo = ' writing CTYPE2 keyword'
      call wtferr(subname,version,status,errinfo) 

c crpix2 ...

      status = 0
      call ftpkye(ounit,'CRPIX2',crpix2,4,
     &'y pixel of tangent plane direction ',status)
      errinfo = ' writing CRPIX2 keyword'
      call wtferr(subname,version,status,errinfo) 

c crval2 ...

      status = 0
      call ftpkye(ounit,'CRVAL2',crval2,6,
     &'sky coordinate of 2nd axis (deg) ',status)
      errinfo = ' writing CRVAL2 keyword'
      call wtferr(subname,version,status,errinfo) 

c cdelt2 ...

      status = 0
      call ftpkye(ounit,'CDELT2',cdelt2,6,
     &'y degrees per pixel ',status)
      errinfo = ' writing CDELT2 keyword'
      call wtferr(subname,version,status,errinfo)     

c cunit2 ...

      status = 0
      call ftpkys(ounit,'CUNIT2',cunit2,
     &'units of dim 2',status)
      errinfo = ' writing CUNIT2 keyword'
      call wtferr(subname,version,status,errinfo)     

c bscale ...

      status = 0
      call ftpkye(ounit,'BSCALE',1.000,6,
     &'Scaling factor for image',status)
      errinfo = ' writing BSCALE keyword'
      call wtferr(subname,version,status,errinfo)   

c bzero ...

      status = 0
      call ftpkye(ounit,'BZERO',0.000,6,
     &'Offset for image',status)
      errinfo = ' writing BZERO keyword'
      call wtferr(subname,version,status,errinfo)

c bunit ...

      status = 0
      call ftpkys(ounit,'BUNIT',bunit,
     &'Units of exposure map data',status)
      errinfo = ' writing BUNIT keyword'
      call wtferr(subname,version,status,errinfo)

c Determine and write min and max data values ...

      datamin = exparr(1,1)
      datamax = exparr(1,1)
      do i=1,map_max
        do j=1,map_max
          IF (exparr(i,j).LT.datamin) THEN
            datamin = exparr(i,j)
          ENDIF
          IF (exparr(i,j).GT.datamax) THEN
            datamax = exparr(i,j)
          ENDIF
        enddo
      enddo

      status = 0
      call ftpkye(ounit,'DATAMIN',datamin,6,
     &'Minimum value in image',status)
      errinfo = ' problem writing DATAMIN keyword'
      call wtferr(subname,version,status,errinfo)

      status = 0
      call ftpkye(ounit,'DATAMAX',datamax,6,
     &'Maximum value in image',status)
      errinfo = ' problem writing DATAMAX keyword'
      call wtferr(subname,version,status,errinfo)

c --- HISTORY/COMMENTS ---

        IF (nk_hist.GT.0) THEN
           do i=1,nk_hist
              status = 0
              call ftphis(ounit,hist(i),status)
              errinfo = ' writing history'
              IF (chatter.GE.15) THEN
                call wtferr(subname,version,status,errinfo)
              ENDIF
              IF (status.NE.0) THEN
                status = 0
                call ftphis(ounit,' missing record, illegal char ?',
     &          status)
              ENDIF
           enddo
        ENDIF
        exhist = 'EXTENSION WRITTEN BY WTEXM1 Ver '
     &//version
        status = 0
        call ftphis(ounit,exhist,status)
        errinfo = ' writing history'
        call wtferr(subname,version,status,errinfo)

        IF (nk_comm.GT.0) THEN
           do i=1,nk_comm
              status = 0
              call ftpcom(ounit,comments(i),status)
              errinfo = ' writing comments'
              IF (chatter.GE.15) THEN
                call wtferr(subname,version,status,errinfo)
              ENDIF
              IF (status.NE.0) THEN 
                status = 0
                call ftpcom(ounit,'missing record,illegal char ?',
     &          status)
              ENDIF
           enddo
        ENDIF


c --- DEFINE DATA STRUCTURE ---

      call ftpdef(ounit,bitpix,naxis,naxes,pcount,gcount,status)
      errinfo = ' defining primary data structure'
      call wtferr(subname,version,status,errinfo)
      IF (status.NE.0) THEN
        errflg = 2
        return
      ENDIF
      subinfo = ' data strucure has been defined'
      call wtinfo(chatter,20,2,subinfo)

c --- WRITE DATA ---

      status = 0
      call ftp2de(ounit,0,map_max,naxes(1),naxes(2),exparr,status)
      errinfo = ' writing primary data (exp map)'
      call wtferr(subname,version,status,errinfo)
      IF (status.NE.0) THEN
        errflg = 3
        return
      ENDIF
      subinfo = ' data has been written'
      call wtinfo(chatter,20,2,subinfo)
      return
      end
c -------------------------------------------------------------------
c     END OF WTEXM1
c -------------------------------------------------------------------
