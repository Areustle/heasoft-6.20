**************************************************************************** 
C FTOOLS FIMAGE TASK:
C     fimconcat 
C     
C FILE:
C     fimconcat.f 
C
C DESCRIPTION: 
C     Takes two input FITS files containining images and places the
C     images side by side (either left-to-right or bottom-to-top in 
C     the primary image array of the output  
C     file.
C
C AUTHOR:  
C      Lawrence E. Brown, June 1994
C
C MODIFICATION HISTORY:
C     10/17/97 PDW 1.1a - Replace old get header routines
C     01/10/00 NG  1.1b - Fixed bug calling ftghpr. 
C
C NOTES:
C      fimconcat is supported in IRAF and HOST environments
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infil1        - Ist input FITS file and extension number
C      infil2        - 2nd input FITS file and extension number
C      outfil   - output FITS file
C      orient   - Left to right or bottom to top orientation
C      obitpix - the bitpix keyword for the output file
C      copyprime - whether to copy all other keywords
C
C CALLED ROUTINES:
C     subroutine gimconcat - gets the parameters
C     subroutine ginfilp - opens input files and gets dimensions
C     subroutine udmget  - dynamic memory allocation
C     subroutine fcerr - writes error to terminal
C     subroutine imconcat - concatenates the two images and writes them out
C     subroutine udmfre - deallocate dynamic memory
C
C****************************************************************************** 
      subroutine fimcot
      implicit none
C
      character * 160 infil1 
      character * 160 infil2 
      character * 160 outfil 
      character * 80 context
      character * 2   orient
      integer       obitpix,datatype

      integer         iunit1
      integer         iunit2
      integer         naxis
      integer         naxes1(10),naxes2(10),naxes3(10)
      integer         naxes21,naxes22,naxes31,naxes32
      integer         size1,size2
      integer         im1pt,im2pt,ftstat
      integer         status
      
      logical            copyprime
      logical allocok
      

C   COMMON variables
C******************************************************************************
C   the following MEM common block definition is in the system iraf77.inc file
C   and is used for dynamic memory allocation 
      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD
      
C     note:
C      datatype      value
C      logical        1
C      integer*2      3
C      Integer        4
C      Long Integer   5
C      Real           6
C      Double         7
C      Complex        8
C******************************************************************************      
      character(40) taskname
      common /task/ taskname
      
      taskname = 'fimconcat1.1b'
      infil1 = ' '
      infil2 = ' '
      orient = ' '
      obitpix = 0
      naxis = 2
      status = 0
      datatype=7
      allocok=.false.


C  get the parameters from the par file

      call gimconcat(infil1,infil2,outfil,orient,copyprime,obitpix,
     &     status)
C     do the concatenation with double precision variables
      datatype=7
      
C     get parameters from input fits files and allocate dynamic memory
      
      call ginfilp(infil1,infil2,iunit1,iunit2,naxes1,naxes2,
     +    status)
      if (status .ne. 0) return
      
C     create dynamic arrays
      size2=naxes2(1)*naxes2(2)
      if(orient.eq.'LR') then
         naxes3(1)=naxes1(1)+naxes2(1)
         naxes3(2)=max(naxes1(2),naxes2(2))
      elseif(orient.eq.'BT') then
         naxes3(1)=max(naxes1(1),naxes2(1))
         naxes3(2)=naxes1(2)+naxes2(2)
      else
         context='Orientation must be BT or LR'
         call fcerr (context)
         goto 999
      endif
C     note the image1 array is also the output array for efficiency
      im1pt = 0
      im2pt = 0
      size1=naxes3(1)*naxes3(2)
      call udmget(size1,datatype,im1pt,ftstat)
      call udmget(size2,datatype,im2pt,ftstat)
      if (ftstat .ne. 0) then
         context = ' Error dynamically allocating arrays'
         call fcerr (context)
         goto 999
      endif
      allocok=.true.
      

C  read in the FITS files stick them together
C  and write the results to the output file.
C  naxes2 and naxes3 must be passed as integer scalars
C  because you can't dimension an adjustable array with
C  array elements (blame ANSI, not me)
      naxes21=naxes2(1)
      naxes22=naxes2(2)
      naxes31=naxes3(1)
      naxes32=naxes3(2)


      call imconcat(iunit1,iunit2,outfil,naxes1,
     $     naxes21,naxes22,naxes31,naxes32,
     $     memd(im1pt),memd(im2pt),
     $     orient,copyprime,obitpix,status)

 999  continue
      if(allocok) then
         call udmfre (im1pt, datatype, ftstat)
         call udmfre (im2pt, datatype, ftstat)
         if(ftstat.ne.0) then
            context = 'error deallocating dynamic memory'
            call fcerr(context)
         endif
      endif
      return
      end

C******************************************************************************
C SUBROUTINE:
C     gimconcat 
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C AUTHOR:  
C     Lawrence Brown  June 1994
C     based on garith by Vidya Sagar. Sep '92
C       
C
C NOTES:
C      gimconcat uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C    call gimconcat(infil1,infil2,outfil,orient, copyprime,obitpix,
C    status)
C
C ARGUMENTS:
C      infil1        - Ist input FITS file and extension number
C      infil2        - 2nd input FITS file and extension number
C      outfil    - output FITS file
C      orient   - Left to right or bottom to top orientation
C      copyprime - whether to copy keywords
C      obitpix - the bitpix keyword for the output file
C      status  - error number
C
C PRIMARY LOCAL VARIABLES:
C      contxt - error message 
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C      subroutine uclgs_ - get parameter
C
C******************************************************************************

      subroutine gimconcat(infil1,infil2,outfil,orient,copyprime,
     $     obitpix,status)
      implicit none
C
      character*(*) infil1,infil2
        character*(*) outfil
        character*(*)  orient
      logical copyprime
        character(80)  contxt
        integer obitpix
        integer status
        character(1) datatype

C  initialize variables
      status = 0

C  get the name of the input FITS file
      call uclgst('infil1',infil1,status)
      if (status .ne. 0) then
          contxt = 'could not get INFIL1 parameter'
          call fcerr(contxt)
          goto 999
      endif


C  get the name of the input FITS file
      call uclgst('infil2',infil2,status)
      if (status .ne. 0) then
          contxt = 'could not get INFIL2 parameter'
          call fcerr(contxt)
          goto 999
      endif

C  get the name of the output FITS file
      call uclgst('outfil',outfil,status)
      if (status .ne. 0) then
          contxt = 'could not get OUTFIL parameter'
          call fcerr(contxt)
          goto 999
      endif

C  get the orient variable name
      call uclgst('orient',orient,status)
      if (status .ne. 0) then
          contxt = 'could not get ORIENT parameter'
          call fcerr(contxt)
          goto 999
        else
            call ftupch(orient)
      endif

C  get the datatype variable 
      call uclgst('datatype',datatype,status)
      if (status .ne. 0) then
          contxt = 'could not get DATATYPE parameter'
          call fcerr(contxt)
          goto 999
        else
            call ftupch(datatype)
      endif
C  get the obitpix value
      if (datatype .eq. 'B') then
        obitpix = 8
      else if (datatype .eq. 'I') then
        obitpix = 16
      else if (datatype .eq. 'J') then
        obitpix = 32
      else if (datatype .eq. 'E') then
        obitpix = -32
      else if (datatype .eq. 'D') then
        obitpix = -64
      else
        contxt = ' Unsupported datatype requested for output'
        call fcerr(contxt)
        goto 999
      end if


C  get whether to copy other keywords from primary array of file1
      call uclgsb('copyprime', copyprime, status)
      if (status .ne. 0) then
          contxt = 'could not get COPYPRIME parameter'
          call fcerr(contxt)
          goto 999
      endif

999      continue

      return
      end
C******************************************************************************
C SUBROUTINE:
C      imconcat
C
C DESCRIPTION: 
C     concatenates the two 2d fits images and writes them out
C
C AUTHOR:  
C      Lawrence Brown based on arwrit by
C      Vidya Sagar Aug '92 
C
C MODIFICATION HISTORY:
C
C NOTES:
C      imconcat uses FITSIO calls to read image file
C
C USAGE:
C      call imconcat(iunit1,iunit2,outfil,naxes1,
C     $    naxes21,naxes22,naxes31,naxes32,
C     $    image1,image2,orient,copyprime,obitpix,status)
C
C ARGUMENTS:
C      infil1        - Ist input FITS file and extension number
C      infil2        - 2nd input FITS file and extension number
C      outfil    - output FITS file
C      naxes1     - array containing size of each dimension.
C     naxes21,naxes22,naxes31,naxes32 - these have to be passed as
C                 simple integer variable for FORTRAN standard reasons
C      image1,image2 - the working arrays
C      orient   - Left to right or Top to bottom orientation
C      copyprime - whether to copy other keywords from primary array
C      obitpix - the bitpix keyword for the output file
C      status - error number
C CALLED ROUTINES:
C      subroutine ffinit - initialize a new FITS file
C      subroutine ftclos - close a FITS file
C      subroutine ftphpr - write required  primary header keywords
C      subroutine fcerr - write error message to terminal
C      subroutine xcopyscale - copy extra header keywords if asked
C      subroutine ftpdef - define primary HDU
C      subroutine ftg2dd - get a 2d array from primary HDU
C      subroutine ftp2dd - put a 2d array into primary HDU
C     
C     
C
C****************************************************************************** 
      subroutine imconcat(iunit1,iunit2,outfil,naxes1,
     $    naxes21,naxes22,naxes31,naxes32,
     $     image1,image2,orient,copyprime,obitpix,status)
      implicit none
      character*(*) outfil
      character*(*)   orient
      
      integer iunit1,iunit2
      integer obitpix
      integer fpixel
      integer naxes1(2),naxes2(2),naxes3(2)
      integer naxes21,naxes22,naxes31,naxes32 
      integer status,ounit, fstatus
      integer pcount,gcount
      integer naxis
      integer i,j
      
      
      double precision nullval
      double precision image1(naxes31,naxes32)
      double precision image2(naxes21,naxes22)
      
      
      logical anyf1
      logical anyf2
      logical simple
      logical extend
      logical copyprime
      
      naxes2(1)=naxes21
      naxes2(2)=naxes22
      naxes3(1)=naxes31
      naxes3(2)=naxes32
          
      
      ounit = 16
      
      pcount = 0
      gcount = 1
      simple = .true.
      extend = .true.
      naxis=2
      call ffinit(ounit,outfil,status)
      if (status .ne. 0) then
         call fcerr('error opening output file')
         goto 1001
      end if
      
      call ftphpr(ounit,simple,obitpix,naxis,naxes3,pcount,
     +     gcount,extend,status)
      if (status .ne. 0) goto 999
      
C     copy other keywords, if asked
      if (copyprime) call xcopyscale (iunit1, ounit, status)
      
      call ftpdef(ounit,obitpix,naxis,naxes3,pcount,gcount,status)
      if (status .ne. 0) goto 999
      
      fpixel = 1
      
      status = 0
      anyf1 = .false.
      anyf2 = .false.
      do 10 i = 1 , naxes1(1)
         do 20 j=1 , naxes1(2)
            image1(i,j) = 0.
 20      continue
 10   continue
      do 30 i = 1 , naxes2(1)
         do 40 j=1 , naxes2(2)
            image2(i,j) = 0.
 40      continue
 30   continue
      
      call ftg2dd(iunit1,0,nullval,naxes3(1),naxes1(1),naxes1(2),
     $     image1,anyf1,status)
      if (status .ne. 0) goto 999
      
      call ftg2dd(iunit2,0,nullval,naxes2(1),naxes2(1),naxes2(2),
     $     image2,anyf1,status)
      if (status .ne. 0) goto 999
      
C     assign  second image into the blank space next to first image
      if(orient.eq.'BT') then
         do 50 i=1,naxes2(1)
            do 60 j=1+naxes1(2),naxes1(2)+naxes2(2)
               image1(i,j)=image2(i,j-naxes1(2))
 60         continue
 50      continue
      else if (orient.eq.'LR') then
         do 70 i=naxes1(1)+1,naxes1(1)+naxes2(1)
            do 80 j=1,naxes2(2)
               image1(i,j)=image2(i-naxes1(1),j)
 80         continue
 70      continue
      endif
      
      
      call ftp2dd(ounit,0,naxes3(1),naxes3(1),naxes3(2),image1,status)
      if (status.ne.0) goto 999
      
      status = 0
 999  fstatus = 0
      call ftclos(ounit,fstatus)
      
 1001 continue
      fstatus = 0
      call ftclos(iunit1,fstatus)
      fstatus = 0
      call ftclos(iunit2,fstatus)
      return
      end


C****************************************************************************** 
C SUBROUTINE:
C      imcatgkwds
C
C DESCRIPTION: 
C      Get necessary keywords from input FITS file.
C
C AUTHOR:  
C        S.R.K. Vidya Sagar (HSTX)
C
C MODIFICATION HISTORY:
C      Lawrence Brown- June, 1994, changed name to avoid iraf conflicts
C
C USAGE:
C      call imcatgkwds(iunit,infile,bitpx,naxis,naxes,status)
C
C ARGUMENTS:
C      infile   - input FITS file and extension number
C      bitpx    - denotes format of input values
C      naxis    - Ist dimesnion of input image array,
C      naxes    - 2nd dimesnion of input image array,
C
C PRIMARY LOCAL VARIABLES:
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C
C****************************************************************************** 

        subroutine imcatgkwds(iunit,infile,bitpx,naxis,naxes,status)
        implicit none

        integer iunit
        integer extnum
        integer block
        integer naxis
        integer naxes(100)
        integer bitpx
        integer hdtype
        integer status, fstatus
        integer pcount
        integer gcount

        character*(*) infile
        character(160) filnm
        character(80)  contxt

        logical extend 
        logical simple

C  open the input FITS file

        status = 0
      fstatus = 0

      call fcpars(infile,filnm,extnum,status)
C default to primary array
C EAG 8/25/93
      if (extnum .eq. -99) extnum = 0
      call ftopen(iunit,filnm,0,block,status)

      if (status .ne. 0) then
          contxt = 'unable to open infile' // infile
          call fcerr(contxt)
          return
      endif
     
        if (extnum .gt. 0) call ftmahd(iunit,extnum+1,hdtype,status)
      if (status .ne. 0)  then
            call fcerrm(status)
            return
        endif

        pcount = 0
        gcount = 1
        simple = .true.
        extend = .true.
        call ftghpr(iunit,10,simple,bitpx,naxis,naxes,pcount,gcount,
     +              extend,status)

      if (status .ne. 0)  then
            call fcerrm(status)
            call ftclos(iunit,fstatus)
        endif

        return
        end

C***************************************************************************** 
C      ginfilp
C
C DESCRIPTION: 
C      gets array sizes 
C
C AUTHOR:  
C     Lawrence Brown based on gckfil by:
C      Vidya Sagar. Sep '92
C
C
C ARGUMENTS:
C
C PRIMARY LOCAL VARIABLES:
C      infil1        - Ist input FITS file and extension number
C      infil2        - 2nd input FITS file and extension number
C
***************************************************************************** 


        subroutine ginfilp(infil1,infil2,iunit1,iunit2,naxes1,naxes2,
     $     status)

        implicit none

        character*(*) infil1
        character*(*) infil2
        
        
        integer iunit1
        integer iunit2
        integer bitpx1
        integer bitpx2
        integer naxis1
        integer naxis2
        integer naxes1(2)
        integer naxes2(2)
        integer status
        character(80) context
        logical ok

        ok = .false.

        iunit1 = 14
        iunit2 = 15 

        call imcatgkwds(iunit1,infil1,bitpx1,naxis1,naxes1,status)
        if (status .ne. 0) return
           
        call imcatgkwds(iunit2,infil2,bitpx2,naxis2,naxes2,status)
        
        if(naxis1.ne.2.or.naxis2.ne.2) then
           context = 'Only 2d arrays supported'
           call fcerr (context)
           return
        endif

        return
        end

