C**************************************************************************** 
C SELECTOR TASK:
C      fcarith 
C
C FILE:
C      fcarith.f 
C
C DESCRIPTION: 
C      Performs arithmetic operations viz ADD, SUB, MUL, and DIV on
C      two input FITS files containining images.
C
C AUTHOR:  
C      Vidya Sagar. Sep '92
C
C MODIFICATION HISTORY:
C      Added copyprime feature. JKB 5/24/94
C      1/20/95 EAG 3.3a Don't copy scaling keywords, neaten up, clobber
C      1/23/95 EAG 3.3b Copy WCS keywords, datatype parameter
C     10/17/97 PDW 3.3c - Replace old get header routines
C     03/30/00 NG  3.4  - Added overflow parameter.
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infile    - Ist input FITS file and extension number
C      const     - Constant used for arithmetic operations
C      outfil    - output FITS file
C      ops       - Arithmetic operation to be performed 
C                  Only ADD,SUB,MUL and DIV are allowed.
C      datatype  - datatype of output image
C      copyprime - whether to copy all other keywords
C
C CALLED ROUTINES:
C      subroutine gcarih - gets parameters from environment
C      subroutine acwrit - write result to output file.
C
C*****************************************************************************
      subroutine fcarih
C
      character(160) infile 
      character(160) outfil 
      character(20) datatype
      character * 3 ops

      integer iunit
      integer bitpx
      integer naxis
      integer naxes(100)
      integer status

      logical overflow 
      logical copyprime

      double precision const
      character(40) taskname
      common /task/ taskname

      taskname = 'fcarith3.4'
      infile =  ' '
      ops    =  ' '
      bitpx  =   0
      naxis  =   0
      status =   0

      call ftcmsg

C  get the parameters from the par file

      call gcarih(infile,const,outfil,ops,datatype,overflow,
     *            copyprime,status)

C  Get the keywords from the FITS file.

      call gckwds(iunit,infile,bitpx,naxis,naxes,datatype,status)

      if (status .ne. 0) return

C  read in the FITS files perform the requested arithmetic
C  operations and write the results to the output file.

      call acwrit(iunit,const,outfil,bitpx,naxis,naxes,
     +     ops,overflow,copyprime,status)

      return
      end

C******************************************************************************
C SUBROUTINE:
C     garith 
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C AUTHOR:  
C
C MODIFICATION HISTORY:
C
C NOTES:
C      garith uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gcarih(infile,const,outfil,ops,status)
C
C ARGUMENTS:
C      infile    - Ist input FITS file and extension number
C      const     - Constant used for arithmetic operations
C      outfil    - output FITS file
C      ops       - arithmetic operations to be performed.
C                  Operations allowed are ADD,SUB,MUL,DIV
C      copyprime - whether to copy all other keywords
C
C PRIMARY LOCAL VARIABLES:
C      contxt - error message 
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C      subroutine uclgsd - get double precision parameter
C      subroutine uclgst - get string parameter
C
C*****************************************************************************

      subroutine gcarih(infile,const,outfil,ops,datatype,overflow,
     &          copyprime, status)
C
      character*(*) infile
      character*(*) outfil
      character*(*) datatype
      character(3)   ops
      character(80)  contxt

      integer status

      logical overflow
      logical copyprime

      double precision  const

C  initialize variables
      status = 0

C  get the name of the input FITS file
      call uclgst('infile',infile,status)
      if (status .ne. 0) then
         contxt = 'could not get infile parameter'
         call fcerr(contxt)
         goto 999
      endif

C  get the constant
      call uclgsd('const',const,status)
      if (status .ne. 0) then
         contxt = 'could not get CONSTANT parameter'
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

C  get the ops variable name
      call uclgst('ops',ops,status)
      if (status .ne. 0) then
         contxt = 'could not get OPS parameter'
         call fcerr(contxt)
         goto 999
      else
         call ftupch(ops)
      endif


C  get the datatype for the output file
      call uclgst('datatype',datatype,status)
      if (status .ne. 0) then
         contxt = 'could not get DATATYPE parameter'
         call fcerr(contxt)
         goto 999
      else
         call ftupch(datatype)
      endif

C  Ignore the overflow? 
      call uclgsb('overflow', overflow, status)
      if (status .ne. 0) then
         contxt = 'could not get OVERFLOW parameter'
         call fcerr(contxt)
         goto 999
      endif

C  get whether to copy other keywords from primary array of file1
      call uclgsb('copyprime', copyprime, status)
      if (status .ne. 0) then
         contxt = 'could not get COPYPRIME parameter'
         call fcerr(contxt)
         goto 999
      endif

 999  continue

      return
      end

C*****************************************************************************
C SUBROUTINE:
C      acwrit
C
C DESCRIPTION: 
C      Reads n-d images from input files and the resultant obtained by
C      performing the arithmetic operation requested by the user.
C
C AUTHOR:  
C      Vidya Sagar Sep '92 
C
C MODIFICATION HISTORY:
C
C NOTES:
C      acwrit uses FITSIO calls to read image file
C
C USAGE:
C       call acwrit(iunit,const,outfil,bitpx,
C                 naxis,naxes,ops,status)
C
C ARGUMENTS:
C      infile    - Ist input FITS file and extension number
C      const     - Constant used for arithmetic operations
C      outfil    - output FITS file
C      bitpx     - format of data
C      naxis     - # of dimensions
C      naxes     - array containing size of each dimension.
C      ops       - arithmetic operation to be performed
C                  Operations allowed are ADD,SUB,MUL,DIV
C      copyprime - whether to copy all other keywords
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine fcpars - parse off filename and extension number
C      subroutine ftclos - close a FITS file
C      subroutine ftmahd - absolute move to FITS header
C      subroutine ftopen - open a FITS file
C
C****************************************************************************** 
      subroutine acwrit(iunit,const,outfil,bitpx,naxis,naxes,
     +     ops,overflow, copyprime,status)

      character*(*) outfil
      character(3)   ops
      character(80)  card

      integer i
      integer iunit
      integer block
      integer bitpx
      integer fpixel
      integer lpixel 
      integer naxis,naxes(naxis)
      integer status,ounit, tstatus
      integer pcount,gcount
      integer nvals
      integer nelem, fstat

      double precision  const
      double precision  image(1000)
      
      logical anyf
      logical flgval(1000)
      logical index(1000)
      logical simple
      logical extend
      logical overflow 
      logical copyprime

      external GOODWCS, BADDUMMY


C  initialize variables

C  get the input FITS filename and extension number

      
C  Now check for the operation to be peformed. Determine the null values
C  if any and flag the corresponding index number in the array.

      if ((ops .eq. '/' .or. ops .eq. 'DIV') .and. 
     +     const .eq. 0.0) then
         call fcerr('Invalid constant')
         return
      end if

      if (ops .ne. 'ADD' .and. ops .ne. 'SUB' .and.
     +     ops .ne. 'DIV' .and. ops .ne. 'MUL' .and.
     +     ops .ne. '+'   .and. ops .ne. '-'   .and. 
     +     ops .ne. '*'   .and. ops .ne. '/') then
         call fcerr('Wrong arithmetic operator')
         return
      end if

      ounit = 16
      nvals = naxes(1)

      block = 1
      pcount = 0
      gcount = 1
      simple = .true.
      extend = .true.

C  Open the output file

      call ffinit(ounit,outfil,status)
      if (status .ne. 0) then
         call fcerr(' Output file already exists')
         goto 999
      end if

C  Insert the keywords

      call ftphpr(ounit,simple,bitpx,naxis,naxes,pcount,
     +     gcount,extend,status)
      if (status .ne. 0) goto 999

C copy other keywords, if asked
      if (copyprime) then

C check if the FITS definition comment keyword exist in the input file,
C and if so, delete them from the output file, to not duplicate them
         call ftgrec(iunit, 7, card, tstatus)
	 if (card(1:7) .eq. 'COMMENT') then
                call ftdkey(ounit, 'COMMENT', tstatus)
                call ftdkey(ounit, 'COMMENT', tstatus)
         end if

         call copyhead (iunit, ounit, .true., .true.,
     &     BADDUMMY, GOODWCS, status)
      end if

      call ftpdef(ounit,bitpx,naxis,naxes,pcount,gcount,status)
      if (status .ne. 0) goto 999

C  Compute the total number of image elements in the FITS file

      do 100 i = 2, naxis
         nvals = nvals * naxes(i)
 100  continue
      status = 0
      
      fpixel = 1

 1000 continue

      if (nvals .gt. 1000) then
         nelem = 1000
         nvals = nvals - nelem 
         lpixel = fpixel + nelem
      else
         nelem = nvals
         nvals = 0
      end if

      status = 0

C  Initialize the input array

      do 208 i = 1, nelem
         image(i) = 0
         flgval(i) = .false.
	 index(i) = .false.
 208  continue

C  Get the image values from the FITS file

      call ftgpfd(iunit,0,fpixel,nelem,image,flgval,anyf,status)

      if (status .ne. 0) goto 999

C  Now start performing the operations

      if (ops .eq. 'ADD' .or. ops .eq. '+') then

         if (anyf) then

            do 1200 i = 1, nelem
               if (flgval(i)) then
                  index(i) = .true.
               else
                  image(i) = image(i) + const
               end if
 1200       continue

         else

            do 1201 i = 1, nelem
               image(i) = image(i) + const
 1201       continue

         end if

      else if (ops .eq. 'SUB' .or. ops .eq. '-') then

         if (anyf) then

            do 2200 i = 1, nelem
               if (flgval(i)) then
                  index(i) = .true.
               else
                  image(i) = image(i) - const
               end if
 2200       continue

         else

            do 2201 i = 1, nelem
               image(i) = image(i) - const
 2201       continue

         end if

      else if (ops .eq. 'MUL' .or. ops .eq. '*') then

         if (anyf) then

            do 3200 i = 1, nelem
               if (flgval(i)) then
                  index(i) = .true.
               else
                  image(i) = image(i) * const
               end if
 3200       continue

         else

            do 3201 i = 1, nelem
               image(i) = image(i) * const
 3201       continue

         end if

      else if (ops .eq. 'DIV' .or. ops .eq. '/') then

         if (anyf) then

            do 4200 i = 1, nelem
               if (flgval(i)) then
                  index(i) = .true.
               else
                  image(i) = image(i) / const
               end if
 4200       continue

         else

            do 4201 i = 1, nelem
               image(i) = image(i) / const
 4201       continue

         end if
         
      else
      end if

C     check the overflow/underflow
      if(overflow) call ouflow(image,index,nelem,bitpx) 
      call ftpprd(ounit,1,fpixel,nelem,image,status)
      if (anyf) then

         do 5000 i = 1, nelem
            if (index(i)) then
               call ftppru(ounit,1,i + fpixel - 1,1,status)
            end if
 5000    continue
         
      endif              
      if(status.ne.0) then
          call fcerr('Error writing data to file')
          goto 999
      endif

C  Repeat the process until all the elements are covered

      if (nvals .ne. 0) then
         fpixel = fpixel + nelem
         goto 1000
      end if

      status = 0

 999  continue
      call fcerrm(status)
      fstat = 0
      call ftclos(iunit,fstat)
      if (status .ne. 0) then
         call ftdelt (ounit, fstat)
      else
         call ftclos(ounit,fstat)
      endif

      return
      end

C*****************************************************************************
C SUBROUTINE:
C      gckwds
C
C DESCRIPTION: 
C      Get necessary keywords from input FITS file.
C
C AUTHOR:  
C        S.R.K. Vidya Sagar (HSTX)
C
C MODIFICATION HISTORY:
C
C USAGE:
C      call gckwds(iunit,infile,bitpx,naxis,naxes,status)
C
C ARGUMENTS:
C      infile   - input FITS file and extension number
C      bitpx    - denotes format of input values
C      naxis    - Ist dimesnion of input image array,
C      naxes    - 2nd dimesnion of input image array,
C      datatype - datatype to use for output image
C
C PRIMARY LOCAL VARIABLES:
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine fcpars - parse off filename and extension number
C      subroutine ftclos - close a FITS file
C      subroutine ftmahd - absolute move to FITS header
C      subroutine ftopen - open a FITS file
C
C*****************************************************************************

      subroutine gckwds(iunit,infile,bitpx,naxis,naxes,datatype,status)

      integer iunit
      integer extnum
      integer block
      integer naxis
      integer naxes(100)
      integer bitpx
      integer hdtype
      integer status
      integer pcount
      integer gcount

      character*(*) infile, datatype
      character(160) filnm
      character(80)  contxt

      logical extend 
      logical simple

C  open the input FITS file

      status = 0
      iunit  = 15

      call fcpars(infile,filnm,extnum,status)

C EAG 8/25/93 default to primary array
      if (extnum .eq. -99) extnum = 0

      call ftopen(iunit,filnm,0,block,status)

      if (status .ne. 0) then
         contxt = 'unable to open infile'
         call fcerr(contxt)
         return
      endif
      
      call ftmahd(iunit,extnum+1,hdtype,status)
      if (status .ne. 0)  then
         call fcerrm(status)
         return
      endif

      pcount = 0
      gcount = 1
      simple = .true.
      extend = .true.
      call ftghpr(iunit,100,simple,bitpx,naxis,naxes,pcount,gcount,
     +     extend,status)

      if (status .ne. 0)  then
         call fcerrm(status)
         status = 0
         call ftclos(iunit,status)
      endif

C figure the datatype
      call dataty (datatype, bitpx, status)

      return
      end

