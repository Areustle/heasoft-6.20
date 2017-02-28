C***************************************************************************** 
C SELECTOR TASK:
C      fim2lst
C
C FILE:
C      fim2lst.f 
C
C DESCRIPTION: 
C      Creates a binary table from an input FITS file containing
C        a 1D, 2D or 3D image array
C
C AUTHOR:  
C      Vidya Sagar. Aug '92
C
C MODIFICATION HISTORY:
C       6/4/93 - allow for infinitely sized array
C        (EAG)   default to primary array for image
C
C       7/7/95 - extended to allow for 1D and 3D images
C        (MJT)   and added copyall parameter to allow for
C                copying of zero-valued pixels to list
C     12/03/97 PDW 3.0b - Introduce clobber parameter... sort of...
C                         call ffinit--an ftool utility--instead of ftinit
C     04/23/99 AM  -  modified to add column(s) that give the world 
C                          coordinate value for each pixel
C     12/20/99 NG  3.1  - Updated for reading compressed images.
C     
C
C NOTES:
C      fim2lst supported in IRAF and HOST environments
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infile    - input FITS file and extension number
C      outfil    - output histogram file
C
C CALLED ROUTINES:
C      subroutine gim2lt - gets parameters from environment
C      subroutine gkywds - gets necessary keywords from input FITS file
C      subroutine f2dpix - reads image and writes pixel values to list
C
C****************************************************************************** 
      subroutine fim2lt
C
      character(160)   infile 
      character(160)   outfil
      character * 16   x
      character * 16   y
      character * 16   z
      character * 16   XCOORD
      character * 16   YCOORD
      character * 16   value
      character * 40   extnam
      logical          copyall,C_KEY_WORDS,WCS_KEY_WORDS

      DOUBLE PRECISION CRPIX1,CRVAL1,CDELT1
      DOUBLE PRECISION XREFVAL,YREFVAL,XREFPIX
      DOUBLE PRECISION YREFPIX,XINC,YINC,ROT

      CHARACTER * 80  COORD_TYPE

      integer         bitpix
      integer         naxis
      integer         naxis1
      integer         naxis2
      integer         naxis3
      integer         status

      character(40) taskname
      common /task/ taskname

      taskname = 'fim2lst3.1'
      x   = ' '
      y   = ' '
      z   = ' '
      XCOORD = ' '
      YCOORD = ' '
      COORD_TYPE = ' '
      
      infile = ' '
      outfil = ' '
      value =  ' '
      extnam = ' '
      bitpix = 0
      naxis  = 0
      naxis1 = 0
      naxis2 = 0
      naxis3 = 0
      status = 0
      copyall = .false.

      XREFVAL = 0.
      YREFVAL = 0.
      XREFPIX = 0.
      YREFPIX = 0.
      XINC    = 0.
      YINC    = 0.
      ROT     = 0.

      C_KEY_WORDS   = .TRUE.
      WCS_KEY_WORDS = .TRUE.

C  get the parameters from the par file
      call gim2lt(infile,outfil,x,XCOORD,y,YCOORD,z,
     &  value,extnam,copyall,status)

C  get the keywords from the input FITS file
      call gkywds(infile,bitpix,naxis,naxis1,naxis2,naxis3,
     &  C_KEY_WORDS,CRPIX1,CRVAL1,CDELT1,WCS_KEY_WORDS,XREFVAL,
     &  YREFVAL,XREFPIX,YREFPIX,XINC,YINC,ROT,COORD_TYPE,status)

C  read in the FITS image and write out the FITS binary table
      call f2dpix(infile,outfil,x,XCOORD,y,YCOORD,z,value,
     & naxis,naxis1,naxis2,naxis3,C_KEY_WORDS,CRPIX1,CRVAL1,
     & CDELT1,WCS_KEY_WORDS,XREFVAL,YREFVAL,XREFPIX,YREFPIX,
     & XINC,YINC,ROT,COORD_TYPE,bitpix,extnam,copyall,status)


      return
      end

C******************************************************************************
C SUBROUTINE:
C      gim2lt
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C AUTHOR:  
C
C MODIFICATION HISTORY:
C      7/7/95 - added copyall parameter (MJT)
C      04/23/99 - added xcoord and ycoord parameters
C
C NOTES:
C      gim2lt uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gim2lt(infile,outfil,x,y,z,value,extnam,copyall,status)
C
C ARGUMENTS:
C      infile   - input FITS file and extension number
C      outfil   - output histogram file
C
C PRIMARY LOCAL VARIABLES:
C      contxt - error message 
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C      subroutine uclgst - get string parameter
C      subroutine uclgsb - get logical parameter
C
C******************************************************************************
      subroutine gim2lt(infile,outfil,x,XCOORD,y,YCOORD,z,
     & value,extnam,copyall,status)
C
      character*(*) infile, outfil
      character(16)   x, y, z
      character(16)   XCOORD, YCOORD, value
      character(16)   extnam
      character(80)  contxt

      logical       copyall

      integer status

C  initialize variables
      status = 0

C  get the name of the input FITS file
      call uclgst('infile',infile,status)
      if (status .ne. 0) then
         contxt = 'could not get INFILE parameter'
         call fcerr(contxt)
         goto 999
      endif

C  get the name of the output FITS file
      call uclgst('outfil',outfil,status)
      if (status .ne. 0) then
         contxt = 'could not get OUTFILE parameter'
         call fcerr(contxt)
         goto 999
      endif

C  get the x variable name
      call uclgst('x',x,status)
      if (status .ne. 0) then
         contxt = 'could not get X parameter'
         call fcerr(contxt)
         goto 999
      endif

C  get the x co-ordinate variable name
      call uclgst('xcoord',XCOORD,status)
      if (status .ne. 0) then
         contxt = 'could not get XCOORD parameter'
         call fcerr(contxt)
         goto 999
      endif


C  get the y variable name

      call uclgst('y',y,status)
      if (status .ne. 0) then
         contxt = 'could not get Y parameter'
         call fcerr(contxt)
         goto 999
      endif

C  get the y co-ordinate variable name

      call uclgst('ycoord',YCOORD,status)
      if (status .ne. 0) then
         contxt = 'could not get YCOORD parameter'
         call fcerr(contxt)
         goto 999
      endif

C  get the z variable name

      call uclgst('z',z,status)
      if (status .ne. 0) then
         contxt = 'could not get Z parameter'
         call fcerr(contxt)
         goto 999
      endif

C  get the value name
      call uclgst('value',value,status)
      if (status .ne. 0) then
         contxt = 'could not get VALUE parameter'
         call fcerr(contxt)
         goto 999
      endif

C  get the extension name 
      call uclgst('extnam',extnam,status)
      if (status .ne. 0) then
         contxt = 'could not get EXTNAM parameter'
         call fcerr(contxt)
         goto 999
      endif

C  get the extension name 
      call uclgsb('copyall',copyall,status)
      if (status .ne. 0) then
         contxt = 'could not get COPYALL parameter'
         call fcerr(contxt)
         goto 999
      endif

 999  continue

      return
      end

C******************************************************************************
C SUBROUTINE:
C      f2dpix
C
C DESCRIPTION: 
C      Reads 2-d image from input file and writes pixel values to output.
C
C AUTHOR:  
C      Vidya Sagar Aug '92 
C
C MODIFICATION HISTORY:
C       10/1/92 (EAG):  add check that requested extension is an image
C
C NOTES:
C      f2dpix uses FITSIO calls to read image file
C
C USAGE:
C       call f2dpix(infile,outfil,x,XCOORD,y,YCOORD,z,value,naxis,naxis1,
C     +   naxis2,naxis3,C_KEY_WORDS,CRPIX1,CRVAL1,CDELT1,WCS_KEY_WORDS,XREFVAL,
C     +   YREFVAL,XREFPIX,YREFPIX,XINC,YINC,ROT,COORD_TYPE,bitpix,extnam,
C     +   copyall,status)
C 
C
C ARGUMENTS:
C      infile   - input FITS file and extension number
C      outfil   - output FITS filename
C      x        - x co-ordinate label
C      y        - y co-ordinate label 
C      z        - z co-ordinate label
C      XCOORD   - x world co-ordinate label
C      YCOORD   - y world co-ordinate label
C      value    - pixel label 
C      naxis    - array dimensionality
C      naxis1   - x dimension
C      naxis2   - y dimension
C      naxis3   - z dimension
C           See Definition of the FITS, Section 5, Headers
C      CRPIX1   - 
C      CRVAL1   - 
C      CDELT1   -
C      XREFVAL  -
C      YREFVAL  - 
C      XREFPIX  -
C      YREFPIX  -
C      XINC     -
C      YINC     -
C      ROT      -
C      COORD_TYPE - type of coordinate projection (-SIN,-TAN,-ARC,-NCP,
C                              -GLS,-MER, or -AIT)
C      bitpix   - value which determines input format
C      extnam   - value to be written to EXTNAME keyword in table
C                    extension of output FITS file
C      copyall  - unless set, fim2lst only copies non-zero valued pixels
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine fcpars - parse off filename and extension number
C      subroutine ftclos - close a FITS file
C      subroutine ftgbnh - get BINARY table header
C      subroutine ftgtbh - get ASCII table header
C      subroutine ftmahd - absolute move to FITS header
C      subroutine ftopen - open a FITS file
C
C****************************************************************************** 
      subroutine f2dpix(infile,outfil,x,XCOORD,y,YCOORD,z,value,
     & naxis,naxis1,naxis2,naxis3,C_KEY_WORDS,CRPIX1,CRVAL1,CDELT1,
     & WCS_KEY_WORDS,XREFVAL,YREFVAL,XREFPIX,YREFPIX,XINC,YINC,ROT,
     & COORD_TYPE,bitpix,extnam,copyall,status)

      character*(*) infile,outfil
      character*(*) x,y,z,xcoord,ycoord,value
      character(160) filnam
      character(80)  contxt,COORD_TYPE
      character(16)  tform(10)
      character(16)  ttype(10) 
      character*(*) extnam
      character(40)  tunit(10)
      
      double precision CRPIX1,CRVAL1,CDELT1
      double precision XREFVAL,YREFVAL,XREFPIX
      double precision YREFPIX,XINC,YINC,ROT
      logical C_KEY_WORDS,WCS_KEY_WORDS

      integer block
      integer bitpix
      integer naxis1,naxis2,naxis3
      integer status, iunit,ounit, extnum
      integer hdtype
      integer pcount,gcount,naxis,naxes(3)
      integer parray
      
      logical copyall
      logical simple
      logical extend

C  the following MEM common block definition is in the system iraf77.inc file
C
      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD

C note:
C       datatype        value
C       logical         1
C       integer*2       3
C       Integer         4
C       Long Integer    5
C       Real            6
C       Double          7
C       Complex         8

C  initialize variables

      iunit = 15
      ounit = 16
      block = 2880
      status = 0
      

      tunit(1) = ' '
      tunit(2) = ' '
      tunit(3) = ' '
      tunit(4) = ' '
  
      parray = 0

C  get the filename and extension
      call fcpars(infile,filnam,extnum,status)

C EAG 8/25/93 default to primary array
      if (extnum .eq. -99) extnum = 0

C  open the input FITS file

      call ftopen(iunit,filnam,0,block,status)
      call ftmahd(iunit,extnum+1,hdtype,status)
      call ftghdt(iunit,hdtype,status)
      if (status .ne. 0) then
         contxt = 'unable to open infile'
         call fcerr(contxt)
         call fcerrm (status)
         return
      endif
      
      
C check that requested extension is an image
      if (hdtype .ne. 0) then
         contxt = ' requested extension is not an image'
         call fcerr(contxt)
         status = 1
         return
      endif
      
      call ffinit(ounit,outfil, status)
      if (status .ne. 0) then
         contxt = ' could not open; outfile may already exist?'
         call fcerr(contxt)
         goto 999
      endif

C construct primary array
      pcount = 0
      gcount = 1
      simple = .true.
      naxes(1) = naxis1
      naxes(2) = naxis2 
      naxes(3) = naxis3 
      extend = .true.
      call ftphpr(ounit,simple,bitpix,0,naxes,pcount,
     &     gcount,extend,status)
      call ftpdef(ounit,bitpix,0,naxes,pcount,gcount,status)

 88   continue

C append new HDU
      call ftcrhd(ounit,status)

C construct binary table header
      tform(1) = '1I'

C assign tform,ttype based on # of dimensions (MJT)

      if (naxis .eq. 1) then 
          
        if (C_KEY_WORDS) then
          ttype(1) = x
          ttype(2) = XCOORD
          ttype(3) = value
          tform(2) = '1D'
          
C select tform based on data type (EAG 10/1/92)

        if (bitpix .gt. 0) then
            tform(3) = '1J'
        else
             tform(3) = '1D'
        endif

            call ftphbn(ounit,0,3,ttype,tform,tunit,
     &              extnam,0,status)
            call ftbdef(ounit,3,tform,0,0,status)

        endif

        if ( .NOT. C_KEY_WORDS) then
          ttype(1) = x
          ttype(2) = value
          
C select tform based on data type (EAG 10/1/92)

        if (bitpix .gt. 0) then
            tform(2) = '1J'
        else
             tform(2) = '1D'
        endif

            call ftphbn(ounit,0,2,ttype,tform,tunit,
     &              extnam,0,status)
            call ftbdef(ounit,2,tform,0,0,status)

        endif


      endif
C-----------------------------------------------------------------

      if (naxis .eq. 2) then
        
       if (WCS_KEY_WORDS) then 
        ttype(1) = x
        ttype(2) = XCOORD
        ttype(3) = y
        ttype(4) = YCOORD
        ttype(5) = value
        tform(2) = '1D'
        tform(4) = '1D'
        tform(3) = '1I'

        if (bitpix .gt. 0) then
          tform(5) = '1J'
        else
          tform(5) = '1D'
        endif

        call ftphbn(ounit,0,5,ttype,tform,tunit,
     &               extnam,0,status)
      endif

       if ( .NOT. WCS_KEY_WORDS) then 
        ttype(1) = x
        ttype(2) = y
        ttype(3) = value
        tform(2) = '1I'

        if (bitpix .gt. 0) then
          tform(3) = '1J'
        else
          tform(3) = '1D'
        endif

        call ftphbn(ounit,0,3,ttype,tform,tunit,
     &               extnam,0,status)
        call ftbdef(ounit,3,tform,0,0,status)
      endif

      endif

C -----------------------------------------------------------------------

      if (naxis .eq. 3) then
        ttype(1) = x
        ttype(2) = y
        ttype(3) = z
        ttype(4) = value
        tform(2) = '1I'
        tform(3) = '1I'

        if (bitpix .gt. 0) then
          tform(4) = '1J'
        else
          tform(4) = '1D'
        endif

        call ftphbn(ounit,0,4,ttype,tform,tunit,extnam,0,status)
        call ftbdef(ounit,4,tform,0,0,status)
      endif

C allocate the dynamic memory -- To avoid "Satan's Numbers" for 
C small arrays, set minimum of 100 (MJT)
      if (naxis1*naxis2*naxis3 .lt. 100) then
        call udmget (100, 7, parray, status)
      else
        call udmget (naxis1*naxis2*naxis3, 7, parray, status)
      endif 
      if (status .ne. 0) then
         contxt = ' Error allocating dynamic memory '
         call fcerr (contxt)
         goto 999
      endif

C get the array and write the values to the file
      if (naxis .eq. 1) then
        call im1lgd (naxis1,C_KEY_WORDS,CRPIX1,CRVAL1,CDELT1,
     &   memd(parray), iunit,ounit, copyall, status)
      else if (naxis .eq. 2) then
        call im2lgd (naxis1,naxis2,WCS_KEY_WORDS,
     & XREFVAL,YREFVAL,XREFPIX,YREFPIX,XINC,YINC,ROT,
     & COORD_TYPE,memd(parray),iunit,ounit,copyall,status)
      else 
        call im3lgd (naxis1, naxis2, naxis3, memd(parray), 
     &     iunit, ounit, copyall, status)
      endif

C free the dynamic memory
      call udmfre (parray, 7, status)

 999  if (status .ne. 0) then
         call fcerrm(status)
         status = 0
      endif
      call ftclos(iunit,status)
      status = 0
      call ftclos(ounit,status)

      return
      end

C******************************************************************************
C SUBROUTINE:
C      gkywds
C
C DESCRIPTION: 
C      Get necessary keywords from input FITS file.
C
C AUTHOR:  
C        S.R.K. Vidya Sagar (HSTX)
C
C MODIFICATION HISTORY:
C
C        04/23/99 (AM) - added logical parameters C_KEY_WORDS and 
C                        WCS_KEY_WORDS. Added keywords CRPIX1,CRVAL1,CDELT1
C                        and the values of all standard FITS celestial 
C                        coordinate system keywords from the header of the 
C                        input image, XREFVAL,YREFVAL,XREFPIX,YREFPIX,XINC,
C                        YINC,ROT, and COORD_TYPE.
C       12/20/99 (NG) -  Updated for reading compressed images.
C
C USAGE:
C      call gkywds(infile,bitpix,naxis,naxis1,naxis2,naxis3,
C   +  C_KEY_WORDS,CRPIX1,CRVAL1,CDELT1,WCS_KEY_WORDS,XREFVAL,
C   +  YREFVAL,XREFPIX,YREFPIX,XINC,YINC,ROT,COORD_TYPE,status)
C
C ARGUMENTS:
C      infile   - input FITS file and extension number
C      bitpix   - denotes format of input values
C      naxis    - array dimensions
C      naxis1   - 1st dimension of input image array,
C      naxis2   - 2nd dimension of input image array,
C      naxis2   - 3nd dimension of input image array
C
C PRIMARY LOCAL VARIABLES:
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine fcerr  - echo error to terminal
C      subroutine ftopen - open FITS file
C      subroutine ftclos - close FITS file
C      subroutine ftmahd - advance to proper HDU
C      subroutine ftgkyj - get integer valued keyword
C      subroutine ftgics - get the values of all the standard FITS 
C                          celestial coordinate system keywords from the 
C                          header of a FITS image
C
C******************************************************************************

      subroutine gkywds(infile,bitpix,naxis,naxis1,naxis2,
     & naxis3,C_KEY_WORDS,CRPIX1,CRVAL1,CDELT1,WCS_KEY_WORDS,
     & XREFVAL,YREFVAL,XREFPIX,YREFPIX,XINC,YINC,ROT,
     & COORD_TYPE, status)

      integer iunit
      integer bitpix
      integer naxis,naxis1,naxis2,naxis3
      integer naxes(5)
      integer extnum
      integer status
      integer hdtype
      integer block

      double precision CRPIX1,CRVAL1,CDELT1
      double precision XREFVAL,YREFVAL,XREFPIX,YREFPIX
      double precision XINC,YINC,ROT
      double precision xpos, ypos

      logical C_KEY_WORDS,WCS_KEY_WORDS

      character*(*) infile
      character(160) filnam
      character(80) contxt

      character(80) COORD_TYPE

C  open the input FITS file

      iunit = 15
      status = 0
      naxis1 = 1
      naxis2 = 1
      naxis3 = 1

      do i = 1, 5
         naxes(i) = 0
      enddo

C initialize parameters

      CRPIX1 = 0.
      CRVAL1 = 0.
      CDELT1 = 0. 

      XREFVAL = 0.
      YREFVAL = 0.
      XREFPIX = 0.
      YREFPIX = 0.
      XINC    = 0.
      YINC    = 0.
      ROT     = 0.

      COORD_TYPE = ' '

      C_KEY_WORDS   = .TRUE.
      WCS_KEY_WORDS = .TRUE.


      call fcpars(infile,filnam,extnum,status)

C EAG 8/25/93 default to primary array
      if (extnum .eq. -99) extnum = 0

      call ftopen(iunit,filnam,0,block,status)
      call ftmahd(iunit,extnum+1,hdtype,status)

      if (status .ne. 0) then
         contxt = 'unable to open infile'
         call fcerr(contxt)
         call fcerrm(status)
         return
      endif
      
      call ftgipr(iunit, 5, bitpix, naxis, naxes, status)
      if (status .ne. 0) then 
         contxt = 'unable to get image information'
         call fcerr(contxt)
         call fcerrm(status)
         return
      endif
      
      if (naxis .ge. 4 .or. naxis .le. 0) then
         contxt = 'array dimension out of bounds'
         call fcerr(contxt)
         call fcerrm(status)
         return
      endif

      if (naxes(1) .ne. 0) naxis1 = naxes(1)
      if (naxes(2) .ne. 0) naxis2 = naxes(2)
      if (naxes(3) .ne. 0) naxis3 = naxes(3)

      call ftgkyd(iunit,'CRPIX1',CRPIX1,contxt,status)
        if (status.ne.0) then
           C_KEY_WORDS = .FALSE.
           status = 0
        endif

      call ftgkyd(iunit,'CRVAL1',CRVAL1,contxt,status)
        if (status.ne.0) then
           C_KEY_WORDS = .FALSE.
           status = 0
        endif

      call ftgkyd(iunit,'CDELT1',CDELT1,contxt,status)
        if (status.ne.0) then
           C_KEY_WORDS = .FALSE.
           status = 0
        endif

c Check whether valid WCS keywords have been set.
      
      call ftgics(iunit,XREFVAL,YREFVAL,XREFPIX,YREFPIX,
     &            XINC,YINC,ROT,COORD_TYPE,status)
      if (status.ne.0) then
         WCS_KEY_WORDS = .FALSE.
         status = 0
      endif
      call ftwldp(1.d0,1.d0,XREFVAL,YREFVAL,XREFPIX,YREFPIX,
     &            XINC,YINC,ROT,COORD_TYPE,xpos,ypos,status)
      if ( status .ne. 0 ) then
         CALL fcecho('WCS keywords present but CTYPE not recognised')
         CALL fcecho('No WCS columns will be written')
         WCS_KEY_WORDS = .FALSE.
         status = 0
      endif
         
      if (status .ne. 0)  call fcerrm(status)
      call ftclos(iunit,status)

      return
      end

C******************************************************************************
C SUBROUTINE:
C      im2lgd
C
C DESCRIPTION: 
C      Read in the image, and write out array as a list
C
C AUTHOR:  
C        Emily A. Greene
C        HSTX/GSFC
C        March 2, 1994
C
C MODIFICATION HISTORY:
C      7/95           - added copyall test (MJT)
C      04/23/999 (AM) - added two more columns, XCOORD and YCOORD, to 
C                       write in when the image contains the WCS keywords.
C
C USAGE:
C      call im2lgd (naxis1, naxis2, array, iunit,
C     &     ounit, copyall status)
C
C ARGUMENTS:
C      naxis1 - the X range of the array
C      naxis2 - the Y range of the array
C      array  - the array to be written as a list
C      iunit  - input FITS file unit
C      ounit  - output FITS file unit
C      copyall- copy even zero-valued pixels?
C      status - status of the operation
C
C PRIMARY LOCAL VARIABLES:
C      status  - error number
C
C CALLED ROUTINES:
C
C      ftwldp( XPIX, YPIX, XREFVAL, YREFVAL, XREFPIX, YREFPIX, XINC, 
C                 YINC, ROT, COORD_TYPE, > XPOS, > YPOS, status )
C
C           XPIX, YPIX - input X and Y pixel locations in the image
C           XPOS, YPOS - output X and Y axis celestial coordinates
C
C***************************************************************************** 
      subroutine im2lgd (naxis1,naxis2,WCS_KEY_WORDS,XREFVAL,
     & YREFVAL,XREFPIX,YREFPIX,XINC,YINC,ROT,COORD_TYPE,
     & array,iunit,ounit,copyall,status)

      integer naxis1, naxis2
      double precision array(naxis1, naxis2)
      integer iunit, ounit, status
      logical copyall

      integer ix, iy, frow
      double precision nullval
      logical anyf
      character(80) contxt

      character(80) COORD_TYPE

      logical WCS_KEY_WORDS
      
      double precision XREFVAL,YREFVAL,XREFPIX,YREFPIX
      double precision XINC,YINC,ROT

      double precision XPIX,YPIX,XPOS,YPOS

C initialize variables

      status = 0
     
      frow = 1
      nullval = 0.D0

      XPIX = 0.
      YPIX = 0.

C read in the array
      call ftg2dd (iunit, 1, nullval, naxis1, naxis1, naxis2, array,
     &     anyf, status)
      contxt = 'Failed to read image array'
      if (status .ne. 0)  goto 999

      if ( .NOT. WCS_KEY_WORDS ) THEN

      do 10 ix = 1, naxis1

         do 20 iy = 1, naxis2

            if (copyall .or. array(ix, iy) .ne. 0.D0) then
               call ftpclj (ounit, 1, frow, 1, 1, ix, status)
               call ftpclj (ounit, 2, frow, 1, 1, iy, status)
               call ftpcld (ounit, 3, frow, 1, 1, array(ix, iy), status)
               WRITE(contxt,'(a,i5,a,i5)') 'Failed to write ', 
     &                                     ix, ',', iy
               IF ( status .NE. 0 ) GOTO 999
               frow = frow + 1
            endif

 20      continue

 10   continue

      endif


      if ( WCS_KEY_WORDS ) THEN

      do 30 ix = 1, naxis1

         do 40 iy = 1, naxis2
     
            XPIX = ix
            YPIX = iy

            call ftwldp(XPIX,YPIX,XREFVAL,YREFVAL,XREFPIX,YREFPIX,
     &                  XINC,YINC,ROT,COORD_TYPE,XPOS,YPOS,status)

            if (copyall .or. array(ix, iy) .ne. 0.D0) then
               call ftpclj (ounit,1,frow,1,1,ix,status)
               call ftpcld (ounit,2,frow,1,1,XPOS,status)
               call ftpclj (ounit,3,frow,1,1,iy,status)
               call ftpcld (ounit,4,frow,1,1,YPOS,status)
               call ftpcld (ounit,5,frow,1,1,array(ix,iy),status)
               WRITE(contxt,'(a,i5,a,i5)') 'Failed to write ', 
     &                                     ix, ',', iy
               IF ( status .NE. 0 ) GOTO 999
               frow = frow + 1
            endif

 40      continue

 30   continue

      endif

 

C define the correct size of the extension

      frow = frow - 1
      call ftmkyj (ounit, 'NAXIS2', frow, '&', status)
      contxt = 'Failed to reset NAXIS2'
      IF ( status .NE. 0 ) GOTO 999

 999  CONTINUE
      if ( status .NE. 0 ) THEN
         CALL fcecho(contxt)
         CALL fcerrm(status)
      ENDIF

      RETURN
      end

C******************************************************************************
C SUBROUTINE:
C      im1lgd
C
C DESCRIPTION: 
C      Read in 1D array, and write out pixel list as binary table
C
C AUTHOR:  
C        M. Tripicco
C        HSTX/GSFC
C        July 7, 1995
C
C MODIFICATION HISTORY:
C        based on im2lgd
C
C      04/23/999 (AM) - added one more column, XCOORD, to write in when 
C                        the image contains the CRPIX1,CRVAL1, and CDELT1
C                        keywords.
C
C
C USAGE:
C      call im1lgd (naxis1,C_KEY_WORDS,CRPIX1,CRVAL1,CDELT1,array, 
C    +    iunit, ounit, copyall, status)
C
C ARGUMENTS:
C      naxis1 - the X range of the 1D array
C      array  - the array to be written as a list
C      iunit  - input FITS file unit
C      ounit  - output FITS file unit
C      copyall- copy even zero-valued pixels?
C      status - status of the operation
C
C PRIMARY LOCAL VARIABLES:
C      status  - error number
C
C CALLED ROUTINES:
C      FITSIO library routines
C
C***************************************************************************** 
      subroutine im1lgd (naxis1,C_KEY_WORDS,CRPIX1,CRVAL1,CDELT1,
     & array, iunit, ounit, copyall, status)

      integer naxis1
      double precision array(naxis1)
      integer iunit, ounit, status
      logical copyall

      integer ix, frow
      double precision nullval
      logical anyf
      character(80) contxt

      logical C_KEY_WORDS
      
      double precision CRPIX1,CRVAL1,CDELT1,X_COORD

C initialize variables

      status = 0

      frow = 1
      nullval = 0.D0
      X_COORD = 0.D0

C read in the array
      call ftgpvd (iunit, 1, 1, naxis1, nullval, array,
     &     anyf, status)

      if (status .ne. 0)  goto 999

      if (.NOT. C_KEY_WORDS) then 
          do 10 ix = 1, naxis1
            if (copyall .or. array(ix) .ne. 0.D0) then
               call ftpclj (ounit,1,frow,1,1,ix,status)
               call ftpcld (ounit,2,frow,1,1,array(ix),status)
               frow = frow + 1
            endif
 10       continue

      endif


      if (C_KEY_WORDS) then 
      do 20 ix = 1, naxis1
          X_COORD = (ix - CRPIX1) * CDELT1 + CRVAL1
            if (copyall .or. array(ix) .ne. 0.D0) then
               call ftpclj (ounit,1,frow,1,1,ix,status)
               call ftpcld (ounit,2,frow,1,1,X_COORD,status)
               call ftpcld (ounit,3,frow,1,1,array(ix),status)
               frow = frow + 1
            endif

 20   continue

      endif

C define the correct size of the extension
      frow = frow - 1
      call ftgkyj (ounit,'NAXIS1',ix,contxt,status)
      call ftddef (ounit, frow*ix, status)
      call ftmkyj (ounit, 'NAXIS2', frow, '&', status)

 999  return
      end

C******************************************************************************
C SUBROUTINE:
C      im3lgd
C
C DESCRIPTION: 
C      Read in the image, and write out array as a list
C
C AUTHOR:  
C        M. Tripicco
C        HSTX/GSFC
C        July 7, 1995
C
C MODIFICATION HISTORY:
C        based on im2lgd
C
C USAGE:
C      call im3lgd (naxis1, naxis2, naxis3, array, iunit, ounit,
C     &     copyall, status)
C
C ARGUMENTS:
C      naxis1 - the X range of the array
C      naxis2 - the Y range of the array
C      naxis3 - the Z range of the array
C      array  - the array to be written as a list
C      iunit  - input FITS file unit
C      ounit  - output FITS file unit
C      copyall- copy even zero-valued pixels?
C      status - status of the operation
C
C PRIMARY LOCAL VARIABLES:
C      status  - error number
C
C CALLED ROUTINES:
C      FITSIO library 
C
C***************************************************************************** 
      subroutine im3lgd (naxis1, naxis2, naxis3, array, 
     &      iunit, ounit, copyall, status)

      integer naxis1, naxis2, naxis3
      double precision array(naxis1, naxis2, naxis3)
      integer iunit, ounit, status
      logical copyall

      integer ix, iy, iz, frow
      double precision nullval
      logical anyf
      character(80) contxt

C initialize variables
      frow = 1
      nullval = 0.D0

C read in the array
      call ftg3dd (iunit, 1, nullval, naxis1, naxis2, naxis1, 
     &     naxis2, naxis3, array, anyf, status)
      if (status .ne. 0)  goto 999

      do 10 ix = 1, naxis1

         do 20 iy = 1, naxis2

            do 30 iz = 1, naxis3

               if (copyall .or. array(ix, iy, iz) .ne. 0.D0) then
                  call ftpclj (ounit, 1, frow, 1, 1, ix, status)
                  call ftpclj (ounit, 2, frow, 1, 1, iy, status)
                  call ftpclj (ounit, 3, frow, 1, 1, iz, status)
                  call ftpcld (ounit, 4, frow, 1, 1, 
     &                    array(ix, iy, iz), status)
                  frow = frow + 1
               endif

 30         continue

 20      continue

 10   continue

C define the correct size of the extension
      frow = frow - 1
      call ftgkyj (ounit,'NAXIS1',ix,contxt,status)
      call ftddef (ounit, frow*ix, status)
      call ftmkyj (ounit, 'NAXIS2', frow, '&', status)

 999  return
      end
