CCCCCCCCCCCCCCCCCCCCCCCCCCC readat.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  nreadat
CH1
CH1  $Id: nreadat.f,v 1.6 2001/11/23 19:42:12 irby Exp $
CH1
CH1  Conversion to an Ftool
CH1  Several changes were made to produce the Ftool they are listed at the 
CH1  end of the coment section.
CH1  
CH1  Jeff Silvis HSTX 8 Aug 1997
CH1
CH1
CH1  Programmer(s) and Completion Date:
CH1     Albert Etienne - Hughes-STX - 01/29/92
CH1
CH1  Function: Reads the bin data from the FITS file and stores it in
CH1            an array.
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  Sun 4/280 - Fortran 2.1
CH1
CH2  Calling Sequence:  
CH2
CH2 call nreadat(lunout,lun_cnt,lun_exp,intflg,icntbn,iexpbn,ptdir,detmax,mapt,status,*)
CH2
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2********************************************************************************
CH2      Variables dropped for Ftool
CH2 
CH2	cntbuf(2880)Char   I/O	buffer to read the counts file
CH2	expbuf(2880)Char   I/O	buffer to read the exposure file
CH2
CH2     Jeff Silvis  HSTX   8 Aug 1997
CH2********************************************************************************
CH2
CH2
CH2      lunout        I*4         gives a logical number for the ascii output file
CH2                                  (see below)
CH2      lun_cnt        I*4         logical number for the counts fits file
CH2      lun_exp        I*4         logical number for the exposure fits file
CH2	 intflg	        L*4	    I	Tests if the intensity map is to be generated
CH2      icntbn         R*4         real array to store data from count fits file
CH2      iexpbn         R*4         real array to store data from exposure fits file
CH2      ptdir          R*4         contains the coordinates of the spacecraft x-axis
CH2      detmax         R*4         Maximum angle from detector axis
CH2      mapt           CH*20       Mapt
CH2      status         I*4         gives the error status of the fitsio functions
CH2	  *			    Where to return in case of error
CH2
CH2  Called by:  nreadft
CH2
CH2  Calls:
CH2***********************************************************************************
CH2   Functions no longer called by the Ftool:
CH2
CH2   fread : System routine to read a record
CH2   kmvc  : System routine to move bytes
CH2
CH2***********************************************************************************
CH2
CH2   fcecho: Writes a character string to the screen
CH2   ftgpve: Get real elements from the fits data array
CH2   ftgpvj: Get integer elements from the fits data array
CH2
CH3  COMMON Use:
CH3  COMMON Block Name: infits (Information about the input FITS file)
CH3  Variable   Type                    Definition
CH3    Name
CH3 ---------  -------   -----------------------------------------------
CH3 icrvl1     Real*8    Position of the reference point on the first axis
CH3 icdel1     Real*8    Increment on the first axis
CH3 icrvl2     Real*8    Position of the reference point on the second axis
CH3 icdel2     Real*8    Increment on the second axis
CH3 ibitpx     Integer   Number of bits per pixel
CH3 inaxis     Integer   Number of axis in the FITS file
CH3 inaxs1     Integer   Number of bins on the first axis
CH3 inaxs2     Integer   Number of bins on the second axis
CH3 inaxs3     Integer   Number of bins on the third axis (energy levels)
CH3 ibscal(2)  Real*4    Scale to use to convert the FITS values (1 index for
CH3			 each of the 2 input maps)
CH3 ibzero(2)  Integer   Offset to use to convert the FITS values (1 index for
CH3			 each of the 2 input maps)
CH3 iftprm     Integer   FITS parameters for the Aitoff map (up to 90 groups)
CH3  (5,90)		 1: Number of elements on first axis
CH3  			 2: Bin position on first axis
CH3  			 3: Bin position on second axis
CH3  			 4: Bin incremant on first axis
CH3  			 5: Bin increment on second axis
CH3 igridt     Ch*4      Grid type ('RECT', or 'AITF')
CH3 ienrgy     Real      Energy level ranges
CH3  (2,10)
CH3 iprcnt     Integer   Number of group parameters in FITS file
CH3 igrcnt     Integer   Number of groups in FITS data
CH3 inax12(200)Integer   Number of bins on axis with variable # of bins
CH3 icrpx1     Real      Array index of reference point on axis 1
CH3 icrpx2     Real      Array index of reference point on axis 2
CH3 iznmax(10) Real      Maximum zenith angle for each energy level
CH3 iblank(2)  Real      FITS blank value (1 for each input map)
CH3 icoord     Ch*4      Coordinate system used ('GALA' or 'CELE')
CH3 ibunit(2)  Ch*8      Data type units ('COUNTS' or 'EXPOSR')
CH3 icntbn     Integer   Counts map bin values
CH3   (360,200,10)
CH3 itotcn     Integer   Total number of counts in input map
CH3 imaxvl(2)  Real      Maximum counts bin value for the input maps
CH3 iminvl(2)  Real      Minimum counts bin value for the input maps
CH3 ixmaxp(2)  Real      X position of the maximum bin value
CH3 iymaxp(2)  Real      Y position of the maximum bin value
CH3 izmaxp(2)  Integer   Z index for the maximum bin value
CH3 iexpbn     Real      Exposure map bin values
CH3   (360,200,10)
CH3
CH3  Significant Local Variables:
CH3  Variable   Type   Ini. Val.               Description
CH3  --------   ----   ---------  --------------------------------------
CH3  length     I*4        -      Length of record read
CH3  cntwrd     I*4        -      Number of bytes per word in counts file
CH3  expwrd     I*4        -      Number of bytes per word in exposure file
CH3  cx         I*4        -      Index into the input cntbuf
CH3  ex         I*4        -      Index into the input expbuf
CH3  idat       I*4        -      Raw data read in I*4 format
CH3  idat2      I*4        -      Raw data read in I*2 format
CH3  type       R*4        -      Data type
CH3
CH4  Logical Units Used:
CH4     Unit#  Variable                   Description
CH4     -----  --------  -----------------------------------------------
CH4       3              Program output listing
CH4       6              Terminal output
CH4
CH4************************************************************************************
CH4     Method for Ftool.
CH4
CH4 The method below is basically unchanged except now fitsio routines are use to
CH4 the data and the Aitoff map has been dropped (see notes below).
CH4
CH4  Jeff Silvis 8 Aug 1997
CH4*************************************************************************************
CH4  Method:
CH4     Initialize variables
CH4     Read the first FITS data record
CH4     If (the map is rectangular) then
CH4        For (k=1 to number of energy levels) do
CH4           For (j=1 to number of bins on axis 2) do
CH4              For (i=1 to the number of bins on axis 1) do
CH4                 Get the bin data at the current buffer index
CH4                 Scale and store the bin data
CH4		    Increment the total number of counts
CH4		    Find the maximum counts and its location
CH4		    Find the minimum counts
CH4                 Increment the input buffer index
CH4                 If (the index is > 2880) then
CH4                    Read the next input record from the FITS file
CH4                    Set the record index to 1
CH4                 End if
CH4		    If (the intensity map is requested) repeat for exposure file
CH4              End for
CH4           End for
CH4        End for
CH4     Else if (the map is Aitoff) then
CH4        For (k=1 to number of energy levels) do
CH4           For (j=1 to number of groups) do
CH4              For (p=1 to the number of group parameters) do
CH4		    If (intensity map) Get the next exposure group parameters
CH4                 Get the next counts group parameter
CH4                 Read a new input record if needed
CH4              End for
CH4              For (i=1 to the number of bins in the group)
CH4                 Get the next bin value and scale it
CH4                 Increment the total number of counts
CH4                 Find the maximum counts and its location
CH4                 Find the minimum counts
CH4                 increment the input buffer index
CH4                 if (the index is > 2880) then
CH4                    read the next input record from the FITS file
CH4                    set the record index to 1
CH4                 end if
CH4                If (the intensity map is requested) repeat for exposure file
CH4              end for
CH4           end for
CH4        end for
CH4     end if
CH4  End readat
CH4
CH5 $Log: nreadat.f,v $
CH5 Revision 1.6  2001/11/23 19:42:12  irby
CH5 Fix uninitialized variables to prevent warnings.
CH5
CH5 Revision 1.5  1999/03/25 21:19:27  toliver
CH5 removed dead code causing compiler warnings
CH5
c Revision 1.4  1997/11/03  22:55:51  silvis
c The files used to define the global variables were called: oufits.cmn.f
c and infits.cmn.f and the make file was trying to compile them.  Their
c names were changed to oufits.cmn.inc and infits.cmn.inc and
c were changed in the programs that used these files.
c
CH5 Revision 1.3  1997/09/18 19:38:17  silvis
CH5 A large number of changes were made to the code to make it compatible
CH5 with g77.  Most of these changes involved shorting certain lines.
CH5
CH5 Jeff Silvis
CH5 HSTX
CH5 18 Sept. 1997
CH5
CH5 Revision 1.2  1997/09/05 19:59:02  silvis
CH5 Several changes were made to the above code to make it run on Linux.  It
CH5 still does not run on Linux but I wanted to archive the code and test it on
CH5 solaris and sun to confirm that it still runs there.
CH5
CH5
CH5 Jeff Silvis
CH5
CH5 Revision 1.1  1997/09/03 20:16:01  silvis
CH5 This is the intial input to CVS of the ftool eaddmap.  This Ftool will add
CH5 two EGRET maps.
CH5
CH5 Jeff Silvis
CH5 3 Sept 1997
CH5 Hughes STX
CH5
C Revision 1.6  1994/12/14  14:08:36  albert
C Modified to only write a warning message if an I/O error occurs when the
C end of file is reached. The program then continues normally.
C
C *************************************************************************
C
C         Make readat compatible with Ftools
C
C     There were three major changes to readat to make it compatible with 
C   ftools.  First, a logical was added for output to an ascii error file.
C   Second, all the I/O was changed from the orginal egret software to 
C   standard fitsio.  Third the part of code that handles ther Aitoff maps
C   was comented out.  I will discuss each of these changes below.
C 
C         New logical for ascii file
C
C     The variable lunout added to the argument call. This variable 
C   provides a logicial number that is used for output to an ascii file.
C   In the original code, this was hardwired to be 3.  Since this is 
C   now an Ftool, the logical number 3 is used by XPI (Xanadu Parameter 
C   Interface) to link with the parameter file, so the hardwiring causes 
C   a conflict.  A value for lunout was assigned by the fitsio routine 
C   ftgiou in a higher level routine.  Then lunout is added as an argument 
C   to any routine, such as this one, that has  output to the ascii file.
C
C        Fitsio
C     Originally a character buffer was passed to readat by a higher level 
C   routine (e.g. readft).  This character buffer was then converted into
C   numbers and the correction BSCALE and BZERO were used to scale the data.
C     Now a logical unit number is passed to readat instead of a character
C   buffer.  The fitsio routines ftgpvj and ftgpve use this logical number
C   to get an array of data from the fits file.  The scaling correction is
C   done inside of ftgpvj or ftgpve.  Also the variable status is used to pass
C   fitsio error codes out of this subroutine.
C
C     Aitoff Maps  
C
C     Aitoff Maps are not used with Egret data.  It is likely that this feature
C   was included in readat to process data from other instruments.  I have 
C   commented out this code and have a warning printed to screen and the 
C   error file if the user selectes that option.
C
C    Subroutines no longer needed
C
C    fread - original purpose was to get blocks of character data
C    kmvc  - converts character data to numerical value based on number of 
C            bytes in the number.
C
C    The two subroutines above have been replaced with fitsio.
C
C     Variables no longer needed
C
C    cntbuf - a character variable that stored the counts data from the 
C             fits file, a logical number is now used for fitsio
C    expbuf - a character variable that stored the exposure data from the 
C             fits file, a logical number is now used for fitsio
C    idat - this was a numerical output from kmvc. the fitsio directly
C             produces a number so this is no longer needed.
C    idat2- output for two byte numbers from kmvc.
C    length - a variable that gave the number of bytes that readf pulled from
C             the character buffer
C    pscale - this variable was in the Aitoff code (now commented out).
C    p      -  this variable was in the Aitoff code (now commented out).
C
C
C    Changes made between 28 April and 3 June 1997
C
C    Jeff Silvis
C
C    HUGHES STX 
C  
C    END FTOOL CHANGES
C
C******************************************************************************
C
C Revision 1.5  1994/02/04  15:06:54  albert
C Ignored the cutoff angle if the input map is an sum of maps.
C
C Revision 1.4  1994/01/28  20:48:02  albert
C Modified to cutoff events from the input map beyond the cutoff angle
C
C Revision 1.3  1993/05/24  14:03:55  albert
C Changed the counts array from integer to real.
C
C Revision 1.2  1992/04/16  16:46:47  albert
C Modified to get the bin data arrays as parameters and dimension them variably
C
C Revision 1.1  1992/01/29  19:54:11  albert
C Initial revision
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC readat.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  Ftool change:
C
C  To use fitsio a logical number is passed, instead of a buffer.
C
C
C      Subroutine readat(lunout,cntbuf,expbuf,intflg,icntbn,iexpbn,ptdir,detmax,mapt,*)     
C
C
C J Silvis  13 May 1997
C
C******************************************************************************

       Subroutine nreadat(lunout,lun_cnt,lun_exp,intflg,icntbn,
     &     iexpbn,ptdir,detmax,mapt,status,*)
      implicit none 
C**************************************************************************
C  Ftool change:
C   
C   The variables pscale,p,length,idat,idat2 are no longer used.
C See above for explanation.
C
C      real	pscale,ptdir(2),rcutof,theta,as,ds,p1,p2
C      integer   length,cntwrd,expwrd,detmax,cx,ex,idat,idat2*2,i,j,k,p
C***************************************************************************
      real	ptdir(2),rcutof,theta,as,ds,p1,p2
      integer   cntwrd,expwrd,detmax,cx,ex,i,j,k
      logical   intflg
C**************************************************************************
C  Ftool change:
C
C In the original code the buffer was a character that was coverted to an 
C integer.  This conversion was excecuted within the routine, kmvc.  Now with 
C fitsio, the data are directly pulled as integers by ftgpvj.  So kmvc is 
C no longer needed and cntbuf must be converted to an integer.
C
C      character cntbuf(2880),expbuf(2880),id*80,type(3)*9,mapt*20
C
C**************************************************************************
      character type(3)*9,mapt*20
       character        id*80
      integer cntbuf(2880)
      real expbuf(2880)
      double precision twopi

      include 'infits.cmn.inc'
      real 	icntbn(inaxs1,inaxs2,inaxs3)
      real 	iexpbn(inaxs1,inaxs2,inaxs3)

C**************************************************************************
C  Ftool change:
C
C  To use fitsio the variables below were added.
C
C J. Silvis 13 May 1997 
C 
       integer lun_cnt,lun_exp,firstpix,nbuffer,nullval,status,npixels
       integer group,lunout,ex_firstpix,ex_group,ex_npixels,ex_nbuffer
       logical anynull
C
C   end of new variables for fitsio.
C
C***************************************************************************

      common /id/id
      data type /'COUNTS','EXPOSURE','INTENSITY'/
      id= '$Id: nreadat.f,v 1.6 2001/11/23 19:42:12 irby Exp $'
C
C***************************************************************************
C
C            Ftool Change
C
C  All fitsio routines use "inherited status" convention.  This means that 
C  if an error has already occurred (i.e. status has a value other than 0)
C  then a subroutine will exit immediately without changing the value of
C  status.   This convention will be maintained for the EGRET I/O.  (See 
C  section 4.6 of the FITSIO User's Guide Version 5.0 by William Pence)
C
       if (status .gt. 0 )  then 
           return
       endif
C
C    J. Silvis HSTX 
C     6 Aug 1997  
C
C***************************************************************************
C---> Initialize variables
      status = 0
      cx = 1
      ex = 1
      theta = 0.0
      ex_npixels = 0
      cntwrd = ibitpx(1) / 8
      expwrd = ibitpx(2) / 8
      twopi  = atan(1.0) * 8
      rcutof = cutoff*twopi/360.0
      if (cutoff .eq. detmax .or. mapt.eq.'NOCUTOFF') rcutof = 0
      if (ptdir(1) .gt. twopi/2) ptdir(1) = ptdir(1)-twopi
      if (ptdir(1) .lt. icrvl1) ptdir(1) = ptdir(1)+twopi
      p1 = icrvl1+(int((ptdir(1)-icrvl1)/
     &   icdel1+icrpx1)-icrpx1+0.5)*icdel1
      p2 = icrvl2+(int((ptdir(2)-icrvl2)/
     &   icdel2+icrpx2)-icrpx2+0.5)*icdel2

C************************************************************************
C Replace the counts input below with standard fitsio
C
C J. Silvis 13 May 1997
C
C
C---> Read the first FITS data record
C      call fread(cntbuf,1,length,*2000,*3000)
C************************************************************************
       group = 1
       firstpix = 1
       npixels=inaxs1*inaxs2*inaxs3
       nbuffer=min(2880,npixels)
       call ftgpvj(lun_cnt,group,firstpix,nbuffer,nullval,
     &            cntbuf,anynull,status)
C************************************************************************
C Replace the exposure input below with standard fitsio
C
C J. Silvis 27 May 1997
C
C
C---> Read the first FITS data record
C      if (intflg) call fread(expbuf,10,length,*2000,*3000)
C************************************************************************
      if (intflg) then 
       ex_group = 1
       ex_firstpix = 1
       ex_npixels = npixels
       ex_nbuffer=min(2880,ex_npixels)
       call ftgpve(lun_exp,ex_group,ex_firstpix,ex_nbuffer,nullval,
     &            expbuf,anynull,status)
      endif
C---> Read and store the rectangular maps FITS files
      if (igridt.eq.'RECT') then

C------> Get the bin data for all energies, all y bins, all x bins
         do k=1,inaxs3
	    ds = icrvl2 + (icrpx2-0.5)*icdel2
            do j=1,inaxs2
 	      as = icrvl1 + (icrpx1-0.5)*icdel1
              do i=1,inaxs1

C***************************************************************************
C
C Ftool Change
C
C kmvc is no longer needed.  the fitsio routines above handle its function.
C So I replaced the kmvc lines with idat = cntbuf(cx)
C
C J. Silvis  13 May 1997
C
C---------------> Get the next data item from the count data buffer into idat
C                  if (cntwrd.eq.4) then
C                     call kmvc(idat,1,cntbuf(cx),1,cntwrd)
C                  else if (cntwrd.eq.2) then
C                     call kmvc(idat2,1,cntbuf(cx),1,cntwrd)
C                     idat = idat2
C                  endif
C****************************************************************************
C
                   icntbn(i,j,k) = cntbuf(cx)

C*****************************************************************************
C     Ftool Change
C
C    In the lines below idat has been replaced with icntbn(i,j,k)
C
C
C
C---------------> Zero the bin if it is outside of the cutoff angle
		  if (rcutof .ne. 0.0) then
      		     theta = acos(sin(ds)*sin(p2)+cos(ds)*cos(p2)*cos(p1-as))
      		     if (icoord.eq.'ERTH'.or.icoord.eq.'INST')
     &		        theta = atan(sqrt(tan(as)**2 + tan(ds)**2))
      		     if (theta.gt.rcutof) icntbn(i,j,k) = 0
		  endif
C*****************************************************************************






C*****************************************************************************
C     Ftool Change
C
C   The routine "ftgpvj" automatically scales the data so the line
C  below is not needed.
C                     iexpbn(i,j,k) = idat*ibscal(2) + ibzero(2)
C
C                  icntbn(i,j,k) = idat*ibscal(1) + ibzero(1)
C*****************************************************************************




C---------------> Compute the total counts value
	          itotcn = itotcn + icntbn(i,j,k)

C---------------> Find the maximum counts value and its location
                  if (icntbn(i,j,k).gt.imaxvl(1)) then
	             imaxvl(1) = icntbn(i,j,k)
	             ixmaxp(1) = i
	             iymaxp(1) = j
	             izmaxp(1) = k
	          end if

C---------------> Find the minimum counts value
                  if (icntbn(i,j,k).lt.iminvl(1).and.icntbn(i,j,k).ne.0) 
     &	             iminvl(1) = icntbn(i,j,k)

C---------------> Increment the buffer index and get the next buffer if needed
C**************************************************************************
C   Ftool change
C
C   The orginal program converted characters to numbers, so we needed to move 
C  through the character array in steps that equaled the number of bytes ( i.e.
C  cntwrd).  Now the array is obtained through fitsio and we step through it
C  one element at a time.
C
C                  cx = cx + cntwrd
C*****************************************************************************
                  cx = cx + 1
                  if (cx.gt.2880) then

C************************************************************************
C
C  Ftool Change
C
C Replace the input below with standard fitsio
C
C J. Silvis 13 May 1997
C
C                     call fread(cntbuf,1,length,*100,*3000)
C                     cx = 1
C************************************************************************
                      npixels=npixels-nbuffer
                      firstpix=firstpix+nbuffer
                      nbuffer=min(2880,npixels)
                      call ftgpvj(lun_cnt,group,firstpix,nbuffer,
     &                        nullval,cntbuf,anynull,status)
                      cx = 1
                  endif

C---------------> Do same processing for the exposure data if intensity selected
		  if (intflg) then
C***************************************************************************
C
C Ftool Change
C
C kmvc is no longer needed.  the fitsio routines above handle its function.
C So I replaced the kmvc lines with idat = cntbuf(ex)
C                     if (expwrd.eq.4) then
C                        call kmvc(idat,1,expbuf(ex),1,expwrd)
C                     else if (cntwrd.eq.2) then
C                        call kmvc(idat2,1,expbuf(ex),1,expwrd)
C                        idat = idat2
C                     endif
C
C J. Silvis  27 May 1997
C
C****************************************************************************
C
                     iexpbn(i,j,k) = expbuf(ex)

C*****************************************************************************
C     Ftool Change
C   The intermediate variable idat is no longer needed so iexpbn(i,j,k)
C replaces idat below.
C                     if (rcutof.ne.0.0 .and. theta.gt.rcutof) idat = 0
C
C*****************************************************************************
                      if (rcutof.ne.0.0 .and. theta.gt.rcutof) 
     &                    iexpbn(i,j,k)=0
C*****************************************************************************
C     Ftool Change
C
C   The routine "ftgpve" automatically scales the data so the line 
C  below is not needed.
C                     iexpbn(i,j,k) = idat*ibscal(2) + ibzero(2)
C
C*****************************************************************************
                     if (iexpbn(i,j,k).gt.imaxvl(2)) then
	                imaxvl(2) = iexpbn(i,j,k)
	                ixmaxp(2) = i
	                iymaxp(2) = j
	                izmaxp(2) = k
	             end if
                     if (iexpbn(i,j,k).lt.iminvl(2).and.
     &                   iexpbn(i,j,k).ne.0) 
     &                      iminvl(2) = iexpbn(i,j,k)
C**************************************************************************
C   Ftool change
C
C   The orginal program converted characters to numbers, so we needed to move
C  through the character array in steps that equaled the number of bytes ( i.e.
C  cntwrd).  Now the array is obtained through fitsio and we step through it
C  one element at a time.
C
C                  ex = ex + cntwrd
C*****************************************************************************
                     ex = ex + 1
                     if (ex.gt.2880) then
C
C
C************************************************************************
C  Ftool Change
C
C Replace the input below with standard fitsio
C
C J. Silvis 13 May 1997
C
C                     call fread(expbuf,10,length,*100,*3000)
C                     ex = 1
C************************************************************************
                      ex_npixels=ex_npixels-ex_nbuffer
                      ex_firstpix=ex_firstpix+ex_nbuffer
                      ex_nbuffer=min(2880,ex_npixels)
                      call ftgpve(lun_exp,ex_group,ex_firstpix,
     &                    ex_nbuffer,nullval,expbuf,anynull,status)
                      ex = 1
                     endif
		  endif
                  as = as + icdel1
               end do
               ds = ds + icdel2
            end do
         end do

C---> Read and store the Aitoff FITS file
        else if (igridt.eq.'AITF') then
C***************************************************************************
C
C  Ftool change
C Egret does not use Aitoff FITS files, so there is no need to convert 
C all the code below to fitsio.  I will comment out all the code and 
C leave the user with a warning that these maps can no longer be 
C processed.
C
         call fcecho('*******************************************')
         call fcecho('This code cannot process Aitoff FITS files')
         call fcecho('The routine readat.f would have to be modified')
         call fcecho('if the user must work with Aitoff FITS files')
         call fcecho('*******************************************')
         write(lunout,*) '***************************************'  
         write(lunout,*) 
     &    'This code cannot process Aitoff FITS files' 
         write(lunout,*) 
     &    'The routine readat.f would have to be modified'
         write(lunout,*) 
     &    'If the user must work with Aitoff FITS files'
         write(lunout,*) '****************************************' 
         return 9000
C**
C*************************************************************************
C	 pscale = 100.0
C
CC------> Loop over all the energy levels and over all the groups
C         do k=1,inaxs3
C            do j=1,igrcnt
C
CC------------> Get all the parameters in the group
C               do p=1,iprcnt
C		  if (intflg) then
C     	             call kmvc(idat2,1,expbuf(ex),1,expwrd)
C         	     ex = ex + expwrd
C                     if (ex.gt.2880) then
C                        call fread(expbuf,10,length,*2000,*3000)
C                        ex = 1
C                     endif
C		  end if
C                  call kmvc(idat2,1,cntbuf(cx),1,cntwrd)
C                  iftprm(p,j) = idat2 / 100.0
C	          if (p.eq.1) then
C		     if (j.eq.1.and.iftprm(p,j).lt.1) pscale = 1.0
C	             iftprm(p,j) = idat2 / pscale
C		  end if
C                  cx = cx + cntwrd
C                  if (cx.gt.2880) then
C                     call fread(cntbuf,1,length,*2000,*3000)
C                     cx = 1
C                  endif
C               end do
C               inax12(j) = iftprm(1,j)
C
CC------------> Loop over all the longitude bins for the current j
C               do i=1,inax12(j)
C
CC---------------> Store the bin data (and initialize the exposure bins)
C                  call kmvc(idat2,1,cntbuf(cx),1,cntwrd)
C                  icntbn(i,j,k) = idat2*ibscal(1) + ibzero(1)
C
CC---------------> Compute the total counts value
C	          itotcn = itotcn + icntbn(i,j,k)
C
CC---------------> Find the maximum counts value and its location
C                  if (icntbn(i,j,k).gt.imaxvl(1)) then
C	             imaxvl(1) = icntbn(i,j,k)
C	             ixmaxp(1) = i
C	             iymaxp(1) = j
C	             izmaxp(1) = k
C	          end if
C
CC---------------> Find the minimum counts value
C                  if (icntbn(i,j,k).lt.iminvl(1).and.icntbn(i,j,k).ne.0) 
C     &	             iminvl(1) = icntbn(i,j,k)
C
CC---------------> Increment the cntbuf index & get next cntbuf if needed
C                  cx = cx + cntwrd
C                  if (cx.gt.2880) then
C                     call fread(cntbuf,1,length,*300,*3000)
C                     cx = 1
C                  endif
C
CC---------------> Redo same processing for exposure data if requested
C		  if (intflg) then
C                     call kmvc(idat2,1,expbuf(ex),1,expwrd)
C                     iexpbn(i,j,k) = idat2*ibscal(2) + ibzero(2)
C                     if (iexpbn(i,j,k).gt.imaxvl(2)) then
C	                imaxvl(2) = iexpbn(i,j,k)
C	                ixmaxp(2) = i
C	                iymaxp(2) = j
C	                izmaxp(2) = k
C	             end if
C                     if (iexpbn(i,j,k).lt.iminvl(2).and.iexpbn(i,j,k).ne.0) 
C     &	                iminvl(2) = iexpbn(i,j,k)
C                     ex = ex + expwrd
C                     if (ex.gt.2880) then
C                        call fread(expbuf,10,length,*300,*3000)
C                        ex = 1
C                     endif
C		  end if
C
C               end do
C            end do
C         end do
C****************************************************************************
C Ftool Change
C 
C This is the end of the line commented out to remove the Aitoff map 
C option.
C
C****************************************************************************
      endif

      return

C---> End of file reached
C100   if (i.lt.inaxs1.or.j.lt.inaxs2.or.k.lt.inaxs3) goto 2000
C      return
C300   if (i.lt.inax12(j) .or. j.lt.igrcnt) goto 2000
C      return
C2000  write(lunout,*) 'READAT: Premature end of the FITS file'
C      return 1

C---> Read I/O error
C3000  if (i.lt.inaxs1.or.j.lt.inaxs2.or.k.lt.inaxs3) goto 3001
C      write(lunout,*) 'READAT: Warning - I/O error instead of EOF 
C     & in the FITS file'
C      return
C3001  write(lunout,*) 'READAT: I/O error in reading the FITS file'
C      return 1

CCCCCCCCCCCCCCCCCCCCCCCCCC End readat.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      end
