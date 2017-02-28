C**************************************************************************** 
C SELECTOR TASK:
C      farith 
C
C FILE:
C      farith.f 
C
C DESCRIPTION: 
C      Performs arithmetic operations viz ADD, SUB, MUL, DIV, MIN,
C      and MAX on two input FITS files containining images.
C
C AUTHOR:  
C      Vidya Sagar. Sep '92
C
C MODIFICATION HISTORY:
C       6/29/93 EAG
C       7/14/93 EAG added output datatype selection
C       5/24/94 JKB changed xcopynoscale to xcopyscale
C       1/13/95 EAG 3.3a removed dataty to misc.for, ftcmsg, etc.
C       1/20/95          changed xcopy scale to copyhead no scaling
C       1/23/95 EAG 3.3b copy WCS keywords from 1st infile
C   
C       7/23/97 Banashree M Seifert 4.0
C                 . new option for operator added MIN & MAX 
C                   MIN --- replaces with minimum of the two values
C                   MAX --- replaces with maximum of the two values
C      10/17/97 PDW 4.0a - Replace old get header routines
C      11/19/97 PDW 4.1  - Allow arithmetic between images of arbitrary
C                          dimensions/sizes
C      12/11/97 PDW 4.1a - Added status checks after writing data, and
C                          improved error handling
C      03/31/00 NG  4.2  - Added the overflow parameter.
C      06/01/05 WDP 4.3  - don't duplicate the 2 FITS COMMENT keywords
C                          at the beginning of the header if they already
C                          exist in the input file.
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infil1    - 1st input FITS file and extension number
C      infil2    - 2nd input FITS file and extension number
C      outfil    - output FITS file
C      ops       - Arithmetic operation to be performed 
C                  Only ADD,SUB,MUL,DIV,MIN,MAX are allowed.
C      blank     - value to use for NULL
C      null      - whether to use NaN for real/double NULLs
C      copyprime - whether to copy all other keywords
C      datatype  - the datatype of the output array
C      dim1      - New dimensions for image 1
C      dim2      - New dimensions for image 2
C      evenvec   - whether to require the dimensions of images to
C                  be even multiples of each other
C
C CALLED ROUTINES:
C      subroutine garith - gets parameters from environment
C      subroutine arith  - do arithmetic and write result to output file
C
C*****************************************************************************
      subroutine farith
C
      character(160) infil1,infil2,outfil
      character(3)   ops
      character(20)  datatype,dim1,dim2

      integer       status

      double precision blank
      logical       null
      logical       copyprime,evenvec
      logical       overflow

      character(40) taskname
      common /task/ taskname

      taskname = 'farith4.3'
      status = 0

      call ftcmsg

C  get the parameters from the par file

      call garith(infil1, infil2, outfil, ops, blank, null, copyprime,
     &     datatype, overflow, dim1, dim2, evenvec, status)

C  read in the FITS files, perform the requested arithmetic
C  operations, and write the results to the output file.

      call arith(infil1, infil2, outfil, ops, blank, null, copyprime,
     &     datatype, overflow, dim1, dim2, evenvec, status)

      if(status.ne.0) then
         call fcerrm(status)
      endif

      return
      end

C****************************************************************************
C SUBROUTINE:
C     garith 
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C AUTHOR:  
C
C MODIFICATION HISTORY:
C       6/29/93 EAG Add BLANK and NULL parameters
C       7/14/93 EAG Added datatype parameter
C      11/19/97 PDW Added DIM1, DIM2, and EVENVEC
C
C NOTES:
C      garith uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call garith(infil1,infil2,outfil,ops, blank, null, copyprime, datatype,
C                  overflow, dim1,dim2,evenvec,status)
C
C ARGUMENTS:
C      infil1    - 1st input FITS file and extension number
C      infil2    - 2nd input FITS file and extension number
C      outfil    - output FITS file
C      ops       - arithmetic operations to be performed.
C                  Operations allowed are ADD,SUB,MUL,DIV,MIN,MAX
C      blank     - value of null
C      null      - whether to use NaN for real/double nulls
C      copyprime - whether to copy keywords
C      datatype  - the output file datatype
C      dim1      - New dimensions of first image
C      dim2      - New dimensions of second image
C      evenvec   - Whether to require dimensions of images to be
C                  even multiples of each other
C
C PRIMARY LOCAL VARIABLES:
C      contxt  - error message 
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine fcerr  - echo error message to terminal
C      subroutine uclgst - get string parameter
C
C*****************************************************************************

      subroutine garith(infil1,infil2,outfil,ops, blank, null, 
     &     copyprime, datatype, overflow, dim1, dim2, evenvec, status)
C
      character*(*) infil1,infil2
      character*(*) outfil
      character*(*)   ops, datatype, dim1, dim2
      character(80)  contxt
      double precision blank
      logical null, copyprime, evenvec,overflow
      integer status

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

C  get the ops variable name
      call uclgst('ops',ops,status)
      if (status .ne. 0) then
         contxt = 'could not get OPS parameter'
         call fcerr(contxt)
         goto 999
      else
         call ftupch(ops)
      endif
      if ( ops .ne. 'ADD' .and. ops .ne. '+' .and.
     +     ops .ne. 'SUB' .and. ops .ne. '-' .and. 
     +     ops .ne. 'MUL' .and. ops .ne. '*' .and.
     +     ops .ne. 'DIV' .and. ops .ne. '/' .and.
     +     ops .ne. 'MIN' .and. ops .ne. 'MAX' ) then
         contxt = ' Unknown arithmetic operator:' // ops
         call fcerr(contxt)
         goto 999
      end if

C  get the datatype variable name
      call uclgst('datatype', datatype, status)
      if (status .ne. 0) then
         contxt = 'could not get DATATYPE parameter'
         call fcerr(contxt)
         goto 999
      else
         call ftupch(datatype)
      endif

C  Whether ignore the overflow.
      call uclgsb('overflow', overflow, status)
      if (status .ne. 0) then
         contxt = 'could not get overflow parameter'
         call fcerr(contxt)
         goto 999
      endif

C  get the value of the output NULL 
      call uclgsd('blank',blank,status)
      if (status .ne. 0) then
         contxt = 'could not get BLANK parameter'
         call fcerr(contxt)
         goto 999
      endif

C  get whether to use NaN or BLANK for real/double NULLS
      call uclgsb('null', null, status)
      if (status .ne. 0) then
         contxt = 'could not get NULL parameter'
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

C  get replacement dimensions for image 1
      call uclgst('dim1',dim1,status)
      if (status .ne. 0) then
         contxt = 'could not get DIM1 parameter'
         call fcerr(contxt)
         goto 999
      endif

C  get replacement dimensions for image 2
      call uclgst('dim2',dim2,status)
      if (status .ne. 0) then
         contxt = 'could not get DIM2 parameter'
         call fcerr(contxt)
         goto 999
      endif

C  get whether dimensions of images should be even multiples of each other
      call uclgsb('evenvec', evenvec, status)
      if (status .ne. 0) then
         contxt = 'could not get EVENVEC parameter'
         call fcerr(contxt)
         goto 999
      endif

 999  continue

      return
      end

C***************************************************************************** 
C SUBROUTINE:
C      arith
C
C DESCRIPTION: 
C      Reads n-d images from input files and the resultant obtained by
C      performing the arithmetic operation requested by the user.
C
C AUTHOR:  
C      Vidya Sagar Aug '92 
C
C MODIFICATION HISTORY:
C       6/29/93 EAG Add BLANK and NULL parameters
C
C       7/23/1997 Banashree M Seifert 
C             . operators MIN & MAX are added
C      11/19/97 PDW - Generalized operation to arbitrary and different
C                     dimensions of each input image
C      12/11/97 PDW - Added status checks after writing data
C
C NOTES:
C       arith uses FITSIO calls to read image file
C
C USAGE:
C       call arith(infil1, infil2, outfil, ops, blank, null, copyprime,
C                  datatype, overflow, dim1, dim2, evenvec, status)
C
C ARGUMENTS:
C      infil1    - 1st input FITS file and extension number
C      infil2    - 2nd input FITS file and extension number
C      outfil    - output FITS file
C      ops       - arithmetic operation to be performed
C                  Operations allowed are ADD,SUB,MUL,DIV,MIN,MAX
C      blank     - value to use for nulls
C      null      - whether to use NaN for real/double nulls
C      copyprime - whether to copy other keywords from primary array
C      datatype  - the datatype of the output array
C      dim1      - New dimensions for image 1
C      dim2      - New dimensions for image 2
C      evenvec   - whether to require the dimensions of images to
C                  be even multiples of each other
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine fcpars - parse off filename and extension number
C      subroutine ftclos - close a FITS file
C      subroutine ftmahd - absolute move to FITS header
C      subroutine ftopen - open a FITS file
C
C*****************************************************************************
      subroutine arith(infil1, infil2, outfil, ops, blank, null,
     +     copyprime, datatype, overflow, dim1, dim2, evenvec, status)

      character*(*) infil1,infil2,outfil
      character*(*) ops,datatype,dim1,dim2
      character(80) card
      double precision blank
      logical null,copyprime,evenvec,overflow
      integer status, tstatus

      integer bufsiz
      parameter (bufsiz=2000)
      integer iunit1,iunit2,ounit
      integer block
      integer bitpx
      integer naxis,naxes1(100),naxes2(100),naxes3(100),idx(100)
      integer fstatus
      integer pcount,gcount
      integer fpixel1,fpixel2,fpixel3
      integer nvals1,nvals2,nvals3
      integer nelem1,nelem2,nelem3
      integer inpos1,inpos2,outpos
      integer i
      integer inull

      double precision  imag1(bufsiz),imag2(bufsiz),imag3(bufsiz)
      
      logical anyf1
      logical anyf2
      logical flgvl1(bufsiz),flgvl2(bufsiz),flgvl3(bufsiz)
      logical simple
      logical extend

      external BADDUMMY, GOODWCS

C  check if files are in order.

      iunit1 = 14
      iunit2 = 15

      call gtdim(infil1,infil2,iunit1,iunit2,naxis,
     +     naxes1,naxes2,dim1,dim2,evenvec,bitpx,datatype,status)
      if (status .ne. 0) return

C for special case of dividing integers and no datatype specified:
      if ((datatype .eq. ' ') .or. (datatype .eq. '-')) then
         if ((ops .eq. 'DIV') .or. (ops .eq. '/')) then
            if (bitpx .gt. 0) bitpx = -32
         endif
      endif

      ounit = 16
      block = 1
      pcount = 0
      gcount = 1
      simple = .true.
      extend = .true.
      call ffinit(ounit,outfil,status)
      if (status .ne. 0) then
         call fcerr('output file already exists')
         goto 1001
      end if

      do 50 i=1,naxis
         naxes3(i)=max( naxes1(i), naxes2(i) )
         idx(i)=0
 50   continue

      call ftphpr(ounit,simple,bitpx,naxis,naxes3,pcount,
     +     gcount,extend,status)
      if (status .ne. 0) goto 999

C copy other keywords, if asked and one of the "files" is real
      if (copyprime) then
         if(iunit1.gt.0 ) then

C check if the FITS definition comment keyword exist in the input file,
C and if so, delete them from the output file, to not duplicate them
            call ftgrec(iunit1, 7, card, tstatus)
	    if (card(1:7) .eq. 'COMMENT') then
                call ftdkey(ounit, 'COMMENT', tstatus)
                call ftdkey(ounit, 'COMMENT', tstatus)
            end if

            call copyhead (iunit1, ounit, .true., .true.,
     &           BADDUMMY, GOODWCS, status)
         else if (iunit2.gt.0) then

C check if the FITS definition comment keyword exist in the input file,
C and if so, delete them from the output file, to not duplicate them
            call ftgrec(iunit2, 7, card, tstatus)
	    if (card(1:7) .eq. 'COMMENT') then
                call ftdkey(ounit, 'COMMENT', tstatus)
                call ftdkey(ounit, 'COMMENT', tstatus)
            end if

            call copyhead (iunit2, ounit, .true., .true.,
     &           BADDUMMY, GOODWCS, status)
         endif
         call ftrdef(ounit,status)
         if (status .ne. 0) goto 999
      endif

C for integers, define NULLs, if we're supposed to.
      inull = blank
      if ((null) .and. ((bitpx .eq. 8) .or. (bitpx .eq. 16)
     &     .or. (bitpx .eq. 32))) then
         call ftpnul (ounit, inull, status)
         call ftpkyj (ounit, 'BLANK', inull, 'Null value', status)
      endif
      if (status .ne. 0) goto 999

      nvals1 = naxes1(1)
      nvals2 = naxes2(1)
      nvals3 = naxes3(1)
      do 100 i = 2, naxis
         nvals1 = nvals1 * naxes1(i)
         nvals2 = nvals2 * naxes2(i)
         nvals3 = nvals3 * naxes3(i)
 100  continue
      
      fpixel1 = 1
      fpixel2 = 1
      fpixel3 = 1

      if( iunit1.eq.0 ) then
         call parsvec( infil1, nelem1, imag1, .true., status )
         do 110 i=1,nelem1
            flgvl1(i)=.false.
 110     continue
      else
         nelem1 = 0
      endif

      if( iunit2.eq.0 ) then
         call parsvec( infil2, nelem2, imag2, .true., status )
         do 120 i=1,nelem2
            flgvl2(i)=.false.
 120     continue
      else
         nelem2 = 0
      endif

      nelem3 = min( nvals3, bufsiz )
      do 130 i=1,nelem3
         flgvl3(i)=.false.
 130  continue
      outpos = 1

 1000 continue

      inpos1 = 0
      inpos2 = 0
      do 1010 i=naxis,1,-1
         inpos1 = mod( idx(i),naxes1(i) ) + naxes1(i)*inpos1
         inpos2 = mod( idx(i),naxes2(i) ) + naxes2(i)*inpos2
 1010 continue
      inpos1 = inpos1+1
      inpos2 = inpos2+1

C        Check if the datum from image 1 is in the buffer
      if( inpos1.lt.fpixel1 .or. inpos1.ge.fpixel1+nelem1 ) then
         fpixel1=inpos1
         nelem1 = min(nvals1-fpixel1+1,bufsiz)
         do 1020 i=1,nelem1
            flgvl1(i)=.false.
 1020    continue
         call ftgpfd(iunit1,0,fpixel1,nelem1,imag1,flgvl1,anyf1,status)
         if (status .ne. 0) goto 999
      end if
      inpos1 = inpos1-fpixel1+1

C        Check if the datum from image 2 is in the buffer
      if( inpos2.lt.fpixel2 .or. inpos2.ge.fpixel2+nelem2 ) then
         fpixel2=inpos2
         nelem2 = min(nvals2-fpixel2+1,bufsiz)
         do 1030 i=1,nelem2
            flgvl2(i)=.false.
 1030    continue
         call ftgpfd(iunit2,0,fpixel2,nelem2,imag2,flgvl2,anyf2,status)
         if (status .ne. 0) goto 999
      end if
      inpos2 = inpos2-fpixel2+1
         
C        Perform the arithmetic

      if (ops .eq. 'ADD' .or. ops .eq. '+') then

         if (flgvl1(inpos1) .or. flgvl2(inpos2)) then
            if (null) then
               flgvl3(outpos) = .true.
            else
               imag3(outpos) = blank
            endif
         else
            imag3(outpos) = imag1(inpos1) + imag2(inpos2) 
         end if
         
      else if (ops .eq. 'SUB' .or. ops .eq. '-') then
         
         if (flgvl1(inpos1) .or. flgvl2(inpos2)) then
            if (null) then
               flgvl3(outpos) = .true.
            else
               imag3(outpos) = blank
            endif
         else
            imag3(outpos) = imag1(inpos1) - imag2(inpos2) 
         end if
         
      else if (ops .eq. 'MUL' .or. ops .eq. '*') then
         
         if (flgvl1(inpos1) .or. flgvl2(inpos2)) then
            if (null) then
               flgvl3(outpos) = .true.
            else
               imag3(outpos) = blank
            endif
         else
            imag3(outpos) = imag1(inpos1) * imag2(inpos2) 
         end if
         
      else if (ops .eq. 'DIV' .or. ops .eq. '/') then
         
         if (flgvl1(inpos1) .or. flgvl2(inpos2)) then
            if (null) then
               flgvl3(outpos) = .true.
            else
               imag3(outpos) = blank
            endif
         else
            if (imag2(inpos2) .ne. 0) then
               imag3(outpos) = imag1(inpos1) / imag2(inpos2) 
            else 
               if (null) then
                  flgvl3(outpos) = .true.
                  anyf1 = .true.
               else
                  imag3(outpos) = blank
               endif
            end if
         end if
         
      else if (ops .eq. 'MIN' ) then
         
         if (flgvl1(inpos1) .or. flgvl2(inpos2)) then
            if (null) then
               flgvl3(outpos) = .true.
            else
               imag3(outpos) = blank
            endif
         else
            imag3(outpos) = min(imag1(inpos1), imag2(inpos2)) 
         end if
         
      else if (ops .eq. 'MAX' ) then
         
         if (flgvl1(inpos1) .or. flgvl2(inpos2)) then
            if (null) then
               flgvl3(outpos) = .true.
            else
               imag3(outpos) = blank
            endif
         else
            imag3(outpos) = max(imag1(inpos1), imag2(inpos2)) 
         end if
         
      end if
      
C        Check whether the buffer is full and needs to be outputted
      outpos = outpos+1
      if( outpos.gt.nelem3 ) then
         outpos = 1
c        process any overflows or underflows. 
         if(overflow) 
     *     call ouflow(imag3, flgvl3, nelem3, bitpx)
         call ftpprd(ounit,1,fpixel3,nelem3,imag3,status)
         if (null) then
            do 5000 i = 1, nelem3
               if (flgvl3(i)) then
                  call ftppru(ounit,1,fpixel3+i-1,1,status)
                  flgvl3(i)=.false.
               end if
 5000       continue
         endif
         if(status.ne.0) then
            call fcerr('Error writing data to file')
            goto 999
         endif 
         fpixel3 = fpixel3+bufsiz
         nelem3 = min( nvals3-fpixel3+1, bufsiz )
      endif

C        Increment indices and do another datum
      i=1
 5010 idx(i) = idx(i)+1
      if( idx(i).ge.naxes3(i) ) then
         idx(i)=0
         i=i+1
         if( i.le.naxis ) goto 5010
      endif
      if( i.le.naxis ) goto 1000

 999  fstatus = 0
      if (status .eq. 0) then
         call ftclos(ounit,fstatus)
      else
         call ftdelt(ounit, fstatus)
      endif

 1001 continue
      fstatus = 0
      call ftclos(iunit1,fstatus)
      fstatus = 0
      call ftclos(iunit2,fstatus)
      return
      end

C*****************************************************************************
C SUBROUTINE:
C      gkwds
C
C DESCRIPTION: 
C      Get necessary keywords from input FITS file, or deduce them
C           from the "infile" parameter.
C
C AUTHOR:  
C        S.R.K. Vidya Sagar (HSTX)
C
C MODIFICATION HISTORY:
C       11/19/97 PDW - infile can contain a string of numbers serving
C                      as the input vector instead of a file.
C
C USAGE:
C      call gkwds(iunit,infile,bitpx,naxis,naxes,status)
C
C ARGUMENTS:
C      infile   - input FITS file and extension number
C      bitpx    - denotes format of input values
C      naxis    - number of dimensions of input image array,
C      naxes    - dimensions of input image array,
C
C PRIMARY LOCAL VARIABLES:
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C
C*****************************************************************************

      subroutine gkwds(iunit,infile,bitpx,naxis,naxes,status)

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

      double precision parsdummy(1)

      if(status.ne.0) return
      fstatus = 0

C  open the input FITS file

      call fcpars(infile,filnm,extnum,status)
C default to primary array
C EAG 8/25/93
      if (extnum .eq. -99) extnum = 0
      call ftopen(iunit,filnm,0,block,status)

      if (status .ne. 0) then
         call parsvec(infile,naxes(1),parsdummy,.false.,fstatus)
         if (fstatus.ne.0) then
            contxt = 'unable to open infile ' // infile
            goto 999
         endif
         naxis=1
         bitpx=-32
         iunit=0
         status=0
         return
      endif
      
      if (extnum .gt. 0) call ftmahd(iunit,extnum+1,hdtype,status)
      if (status .ne. 0)  then
         write(contxt,'(A,I12)') 'Unable to move to extension ',extnum
         goto 999
      endif

      pcount = 0
      gcount = 1
      simple = .true.
      extend = .true.
      call ftghpr(iunit,100,simple,bitpx,naxis,naxes,pcount,gcount,
     +     extend,status)

      if (status .ne. 0)  then
         contxt = 'Unable to read extension header'
         goto 999
      endif
      return

 999  call fcerr(contxt)
      fstatus=0
      call ftclos(iunit,fstatus)
      return
      end

C***************************************************************************** 
C      gtdim
C
C DESCRIPTION: 
C      Reads in and checks to see keywords naxis,naxes and bitpix of
C      the 2 files are compatible
C
C AUTHOR:  
C      Vidya Sagar. Sep '92
C
C MODIFICATION HISTORY:
C       7/14/93 EAG - determine output bitpix sensibly
C      11/19/97 PDW - Allow dim1/2 parameter to override image dimensions
C
C ARGUMENTS:
C
C PRIMARY LOCAL VARIABLES:
C      infil1    - 1st input FITS file and extension number
C      infil2    - 2nd input FITS file and extension number
C
C***************************************************************************** 
      subroutine gtdim(infil1,infil2,iunit1,iunit2,naxis,
     +     naxes1,naxes2,dim1,dim2,evenvec,bitpx1,datatype,status)

      character*(*) infil1
      character*(*) infil2, datatype, dim1, dim2
      
      integer iunit1,iunit2
      integer bitpx1,bitpx2
      integer naxis,naxis1,naxis2
      integer naxes1(100),naxes2(100)
      integer nvals1,nvals2,status
      integer i, bitpx

      logical evenvec

      call gkwds(iunit1,infil1,bitpx1,naxis1,naxes1,status)
      call gkwds(iunit2,infil2,bitpx2,naxis2,naxes2,status)
      if (status .ne. 0) return
      
      if( dim1.ne.' ' ) then
         nvals1 = naxes1(1)
         do 2 i=2,naxis1
            nvals1 = nvals1 * naxes1(i)
 2       continue
         call fcgrgs(dim1,999999,naxis1,naxes1,naxes1)
         nvals2 = naxes1(1)
         do 3 i=2,naxis1
            nvals2 = nvals2 * naxes1(i)
 3       continue
         if( nvals1.ne.nvals2 ) then
            status = 1
            call fcerr('New dimensions for first image ' //
     &           'give wrong size')
            return
         endif
      endif

      if( dim2.ne.' ' ) then
         nvals1 = naxes2(1)
         do 4 i=2,naxis2
            nvals1 = nvals1 * naxes2(i)
 4       continue
         call fcgrgs(dim2,999999,naxis2,naxes2,naxes2)
         nvals2 = naxes2(1)
         do 5 i=2,naxis2
            nvals2 = nvals2 * naxes2(i)
 5       continue
         if( nvals1.ne.nvals2 ) then
            status = 1
            call fcerr('New dimensions for second image ' //
     &           'give wrong size')
            return
         endif
      endif

      naxis = max(naxis1,naxis2)
      if( naxis1.lt.naxis ) then
         do 6 i=naxis1+1,naxis
            naxes1(i)=1
 6       continue
      else if( naxis2.lt.naxis ) then
         do 7 i=naxis2+1,naxis
            naxes2(i)=1
 7       continue
      endif

      if( evenvec ) then
         do 10 i = 1, naxis
            if ( mod( naxes1(i), naxes2(i) ).ne.0
     +           .and. mod( naxes2(i), naxes1(i) ).ne.0 ) then
               status = 1
               call fcerr('dimensions of 2 input files are ' //
     +              'not even multiples')
               return
            end if
 10      continue
      end if

C determine a reasonable output datatype based on precision
      bitpx = max(abs(bitpx1),abs(bitpx2))
      if ((bitpx1 .lt. 0) .or. (bitpx2 .lt. 0)) bitpx = -bitpx

C determine output datatype based on the datatype keyword
      call dataty (datatype, bitpx, status)
      bitpx1 = bitpx

      return
      end

C*****************************************************************************
C SUBROUTINE:
C      parsvec
C
C DESCRIPTION: 
C      Parse a string of comma- or space-separated numbers into
C      an array of double precision numbers
C
C AUTHOR:  
C      Peter D. Wilson (HSTX)/Nov. 1997
C
C MODIFICATION HISTORY:
C
C USAGE:
C     call parsvec( line, terms, array, savedata, status )
C
C ARGUMENTS:
C     line     - String containing numbers
C     terms    - Number of terms located in string
C     array    - Double precision values
C     savedata - Copy values into array? (If false, just count terms)
C     status   - FITSIO status code
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C      function   fcstln - get length of string... minus trailing spaces
C      subroutine ftc2dd - Convert string to double precision value
C
C*****************************************************************************
      subroutine parsvec( line, terms, array, savedata, status )

      character*(*) line
      integer terms,status
      double precision array(*)
      logical savedata

      double precision data
      integer i,j,len,fcstln

      len=fcstln(line)
      terms=0

      j=1
 10   if( line(j:j).eq.' ' .and. j.lt.len ) then
         j=j+1
         goto 10
      endif

      i=j+1
 20   if( i.le.len .and. 
     +     line(i:i).ne.',' .and. line(i:i).ne.' ' ) then
         i=i+1
         goto 20
      endif

      call ftc2dd(line(j:i-1),data,status)
      if( status.eq.0 ) terms=terms+1
      if( savedata ) array(terms)=data
      j=i+1
      if(j.le.len) goto 10
      return
      end
