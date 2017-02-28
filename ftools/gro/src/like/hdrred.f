c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c      SUBROUTINE HDRRED(BITPIX,NAXIS,NAXES,BSCALE,BZERO,PIXCENT,
C     &  CDELTA,CRVAL,CRPIX,NFILE,MAPTYP,flag)
C
C
C  $Id: hdrred.f,v 1.3 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C	Effect: 
C++       Called by SUBROUTINE FITRED.
C++       Reads FITS file header.
C++       Uses William Pence's (HEASARC, GSFC) FITSIO
C++       Library of Fortran Subroutines to Read and Write FITS Files.
c
c
c-----------------------------------------------------------------------
c     Subroutine Argument Desriptions
c     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c      integer		BITPIX		number of bits per pixel
c      integer		NAXIS		number of axes
c      integer(10)	NAXES		width, height in pixels, # of images
c      real 		BSCALE		scale factor
c      real 		BZERO		offset
c      logical 		PIXCENT		true > CRVAL describes the pixel center
c      real 		CDELTA(2)	pixel width
c      real 		CRVAL(2)	pixel reference location
c      real		CRPIX(2)	pixel reference number
c      integer*4	NFILE		image to read
c      character(4) 	MAPTYP		type of map
c      logical		flag		true > echo header
c
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED:   by  JRM
c
C=======================================================================
C   Changes:                                                           
c
C JRM25 Nov 91 Install version 3 FITSIO
C JRM Aug 1992 read some standard keywords
c 11/12/92, JRM: Read pixel centered CRVALs
c 23/6/93 , JRM: CRVALs assumed to  pixel centered if PIXCENT missing
c
C  $Log: hdrred.f,v $
C  Revision 1.3  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2007/01/31 21:04:11  irby
C  Replace use of FITSERROR routine (fitserro.f) with calls to fcerr/fcerrm.
C  The reliance of FITSERROR on the Fortran 'access' function prevents it
C  from compiling with gfortran (in which 'access' is not [yet?] implemented).
C  Also, fix some lines that were too long (>72 chars).
C
C  Revision 1.1  2002/04/16 20:27:32  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  2001/03/15  16:27:28  dlb
c Modified the test for celestial coordinates from testing for
c 'RA  ' to just 'RA' since newer FITS files use RA--CAR.
c
c Revision 5.1  1996/02/29  20:48:05  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:37:24  jae
c Subroutine Module for like V5.00
c
c
c-----------------------------------------------------------------------

      SUBROUTINE HDRRED(BITPIX,NAXIS,NAXES,BSCALE,BZERO,PIXCENT,
     &     CDELTA,CRVAL,CRPIX,NFILE,MAPTYP,flag)

c     Common blocks used:
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE '../COMMON/cnfrep.copy'
      INCLUDE '../COMMON/fitrep.copy'

      save
c
c-----------------------------------------------------------------------
      character(80) id, context
      common /id/id
      INTEGER BITPIX,NAXIS,NAXES(10),PCOUNT,GCOUNT,STATUS
      CHARACTER KYWD*8,COMMENT*45,value*30,BUFFER*80,coord_strng*4
      CHARACTER MAPTYP*4,input*80
      REAL BSCALE,BZERO
      REAL CDELTA(2),CRVAL(2),CRPIX(2)
      logical simple,extend,flag,PIXCENT


      id = '$Id: hdrred.f,v 1.3 2013/05/21 19:08:25 irby Exp $'
      SIGMSG='ERROR DEVELOPED IN SUBROUTINE CALLED BY HDRRED'

      if (verbose.and.nfile.eq.0) then 
c     print header
         key_no=0
         DO  while (KYWD(1:8).ne.'END     ')
            key_no=key_no+1
            CALL ftgkyn(LU(14),key_no,KYWD,value,comment,status)
            if (status .ne. 0)then
               context='Error reading header keywords'
               call fcerr(context)
               go to 998
            end if
C           CALL FITSERROR(1,STATUS,'ftgkyn')
            CALL ftgrec(LU(14),key_no,BUFFER,status)
            if (status .ne. 0)then
               if (STATUS.eq.203) then
                  write(6,*)'HDRRED:found status 203, 2'
                  STATUS=0
               else
                  context='Error reading header record'
                  call fcerr(context)
                  go to 998
               endif
            endif
C           CALL FITSERROR(1,STATUS,'ftgrec')
C     PRINT EACH HEADER RECORD AS AN 80 CHARACTER ASCII STRING
            if (.not.flag) WRITE(LU(1),*) BUFFER ! if flag false
         enddo
      endif
C     READ THE KEYWORDS WHICH DEFINE THE SIZE OF THE DATA RECORDS
c
c     Change this call to the new cftio library call.  8/21/01. DLB
c     CALL ftgprh(LU(14),simple,BITPIX,NAXIS,NAXES,pcount,gcount,
c    &       extend,status)
      CALL ftghpr(LU(14),10,simple,BITPIX,NAXIS,NAXES,pcount,gcount,
     &     extend,status)
      
      if (status .ne. 0)then
         if (STATUS.eq.203) then
            write(6,*)'HDRRED:found status 203, 2'
            STATUS=0
         else
            context='Error reading header keywords'
            call fcerr(context)
            go to 998
         endif
      endif
C     CALL FITSERROR(1,STATUS,'ftghpr')
      IF (.not.simple) then
         SIGNAL='E'
         SIGMSG ='HDRRED: FITS file is not simple'
         RETURN
      ENDIF
C     READ REQUIRED INFO FROM HEADER:
      KYWD='BSCALE  '
      CALL ftgkye(LU(14),KYWD,BSCALE,COMMENT,STATUS)
      if (STATUS.ne.0) then
	 print *,'STATUS=',STATUS,
     &        ' BSCALE apparently not specified in header, set to 1'
	 BSCALE=1.
	 STATUS=0
      endif
      KYWD='BZERO   '

      CALL ftgkye(LU(14),KYWD,BZERO,COMMENT,STATUS)
      if (STATUS.ne.0) then
	 print *,'STATUS=',STATUS,
     &        ' BZERO apparently not specified in header, set to 0'
	 BZERO=0.
	 STATUS=0
      endif
      
      KYWD='CDELT1  '
      CALL ftgkye(LU(14),KYWD,CDELTA(1),COMMENT,STATUS)
      KYWD='CDELT2  '
      CALL ftgkye(LU(14),KYWD,CDELTA(2),COMMENT,STATUS)
      
      KYWD='CTYPE1  '
      CALL ftgkys(LU(14),KYWD,coord_strng,COMMENT,STATUS)
      if (coord_strng.eq.'GLON') coord_sys='G'

CDLB  if (coord_strng.eq.'RA  ') coord_sys='C'
CDLB      Changed to accommodate the newer FITS format, namely
CDLB      RA--CAR and DEC-CAR as well as the old format.
CDLB      03/15/01.  D.L.Bertsch
      if (coord_strng(1:2).eq.'RA') coord_sys='C'

      KYWD='CRVAL1  '
      CALL ftgkye(LU(14),KYWD,CRVAL(1),COMMENT,STATUS)
      KYWD='CRVAL2  '
      CALL ftgkye(LU(14),KYWD,CRVAL(2),COMMENT,STATUS)
      KYWD='PIXCENT '
      CALL ftgkyl(LU(14),KYWD,PIXCENT,COMMENT,STATUS)

      if (STATUS.eq.202) then
         write(lu(1),'("HDRRED:PIXCENT not specified, ",
     &        "enter value (T of F) ",$)')
         read(LU(12),'(A)')input
         if (input(1:1).eq.'T'.or.input(1:1).eq.'t') then
	    PIXCENT=.true.
         else
	    PIXCENT=.false.
         endif
         STATUS=0
      endif

      KYWD='CONVLVD '
      CALL ftgkyl(LU(14),KYWD,gmap_conv,COMMENT,STATUS)

      if (STATUS.eq.202) then
c     write(lu(1),*)'HDRRED:CONVLVD not specified, assumed to be F'
         gmap_conv=.false.
         STATUS=0
      endif

      KYWD='CRPIX1  '
      CALL ftgkye(LU(14),KYWD,CRPIX(1),COMMENT,STATUS)

      if (STATUS.eq.202) then
         write(lu(1),*)'HDRRED:CRPIX1 not specified, assumed to be 1'
         CRPIX(1)=1.
         STATUS=0
      endif

      KYWD='CRPIX2  '
      CALL ftgkye(LU(14),KYWD,CRPIX(2),COMMENT,STATUS)

      if (STATUS.eq.202) then
         write(lu(1),*)'HDRRED:CRPIX2 not specified, assumed to be 1'
         CRPIX(2)=1.
         STATUS=0
      endif

      do N=1,NAXES(3)
         if (N.lt.10) then
            write(KYWD,'("MINENG",I1," ")')N
            CALL ftgkyj(LU(14),KYWD,ENERGY(1,N),COMMENT,STATUS)
            write(KYWD,'("MAXENG",I1," ")')N
            CALL ftgkyj(LU(14),KYWD,ENERGY(2,N),COMMENT,STATUS)
         else
            write(KYWD,'("MINENG",I2)')N
            CALL ftgkyj(LU(14),KYWD,ENERGY(1,N),COMMENT,STATUS)
            write(KYWD,'("MAXENG",I2)')N
            CALL ftgkyj(LU(14),KYWD,ENERGY(2,N),COMMENT,STATUS)
         endif
      enddo

      if (STATUS.eq.202) then
         write(lu(1),*)'HDRRED:Energy keywords not found.'
         STATUS=0
      endif
      
      if (maptyp.ne.'GMAP') then
         CALL ftgkyd(LU(14),'STRT-DAY',STRT_DAY,COMMENT,STATUS)
         CALL ftgkyd(LU(14),'STRT-TIM',STRT_TIM,COMMENT,STATUS)
         CALL ftgkyd(LU(14),'END-DAY',END_DAY,COMMENT,STATUS)
         CALL ftgkyd(LU(14),'END-TIM',END_TIM,COMMENT,STATUS)
         CALL ftgkye(LU(14),'SC-Z-RA',SC_RA,COMMENT,STATUS)
         CALL ftgkye(LU(14),'SC-Z-DEC',SC_DEC,COMMENT,STATUS)
         CALL ftgkye(LU(14),'SC-Z-LII',SC_LII,COMMENT,STATUS)
         CALL ftgkye(LU(14),'SC-Z-BII',SC_BII,COMMENT,STATUS)
         if (STATUS.eq.202) then
c     write(lu(1),*)'HDRRED: spacecraft pointing keywords not found.'
	    STATUS=0
         endif
         CALL ftgkys(LU(14),'MAPTYPE',MAPTYPE,COMMENT,STATUS)
         do N=1,NAXES(3)
	    if (N.lt.10) then
               write(KYWD,'("ZENMAX",I1," ")')N
            else
               write(KYWD,'("ZENMAX",I2)')N
	    endif
	    CALL ftgkye(LU(14),KYWD,zenith(N),COMMENT,STATUS)
         enddo
         if (STATUS.eq.202) then
c     write(lu(1),*)'HDRRED: zenith keywords not found.'
	    STATUS=0
         endif
      endif
      
      GOTO 999
C     Print out the FITSIO error number and text string
 998  CALL FCERRM(STATUS)

 999  CONTINUE
C     CALL FITSERROR(1,STATUS,'endcal')
      RETURN                                                            
      END                                                              
c     
