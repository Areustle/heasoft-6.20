CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC intens.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  intens
CH1
CH1  $Id: intens.f,v 1.4 1997/11/03 22:55:50 silvis Exp $
CH1
CH1  Programmer(s) and Completion Date:
CH1     Albert Etienne - Hughes-STX - 01/29/92
CH1
CH1  Function: Computes the intensity data from the counts and exposure data
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  Sun 4/280 - Fortran 2.1
CH1
CH2  Calling Sequence:  call intens(intflg,*)
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2	intflg	    L*4	    I	Tests if the intensity map is to be produced
CH2	* 	    		Line to return to in case of error
CH2
CH2  Called by:  addmap
CH2
CH2  Calls: None
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
CH3  COMMON Block Name: oufits (Information about the output FITS file)
CH3  Variable   Type                    Definition
CH3    Name
CH3 ---------  -------   -----------------------------------------------
CH3 ocrvl1     Real*8    Position of the reference point on the first axis
CH3 ocdel1     Real*8    Increment on the first axis
CH3 ocrvl2     Real*8    Position of the reference point on the second axis
CH3 ocdel2     Real*8    Increment on the second axis
CH3 obitpx     Integer   Number of bits per pixel
CH3 onaxis     Integer   Number of axis in the FITS file
CH3 onaxs1     Integer   Number of bins on the first axis
CH3 onaxs2     Integer   Number of bins on the second axis
CH3 onaxs3     Integer   Number of bins on the third axis (energy levels)
CH3 obscal(3)  Real*4    Scale to use to convert the FITS values (1 index for
CH3			 each of the 3 output maps)
CH3 obzero(3)  Integer   Offset to use to convert the FITS values (1 index for
CH3			 each of the 3 output maps)
CH3 oindex(2)  Integer   Index for the oheader buffer (2 input files)
CH3 oftprm     Integer   FITS parameters for the Aitoff map (up to 90 groups)
CH3  (5,90)		 1: Number of elements on first axis
CH3  			 2: Bin position on first axis
CH3  			 3: Bin position on second axis
CH3  			 4: Bin incremant on first axis
CH3  			 5: Bin increment on second axis
CH3 ogridt     Ch*4      Grid type ('RECT', or 'AITF')
CH3 oenrgy     Real      Energy level ranges
CH3  (2,10)
CH3 oprcnt     Integer   Number of group parameters in FITS file
CH3 ogrcnt     Integer   Number of groups in FITS data
CH3 onax12(200)Integer   Number of bins on axis with variable # of bins
CH3 ocrpx1     Real      Array index of reference point on axis 1
CH3 ocrpx2     Real      Array index of reference point on axis 2
CH3 oznmax(10) Real      Maximum zenith angle for each energy level
CH3 oblank(3)  Real      FITS blank value (1 for each output map)
CH3 omapsz(3)  Integer   Map size (number of integers in each of the 3 maps)
CH3 omaxvl(3)  Real      Maximum counts bin value for the 3 output maps
CH3 ominvl(3)  Real      Minimum counts bin value for the 3 output maps
CH3 oxmaxp(3)  Real      X position of the maximum bin value
CH3 oymaxp(3)  Real      Y position of the maximum bin value
CH3 ozmaxp(3)  Integer   Z index of the maximum bin value
CH3 ocoord     Ch*4      Coordinate system used ('GALA' or 'CELE')
C**************************************************************************
C
C  FTOOL Change
C
C The fits file is no longer read into a buffer.  Rather fitsio is
C is used, so obuffr is no longer used by the code.
C
C  Jeff Silvis
C
C  Sept 1997
C**************************************************************************
CH3 obuffr(3)  Ch*2880   Output FITS buffer for the 3 output maps
CH3 oheadr     Ch*80     Buffer to save the header information of the 2 input
CH3  (2,300)		 maps
CH3 ocntbn     Integer   Counts map bin values
CH3   (360,200,10)
CH3 oexpbn     Real      Exposure map bin values
CH3   (360,200,10)
CH3 ointbn     Real      Intensity map bin values
CH3   (360,200,10)
CH3
CH3  Significant Local Variables:
CH3  Variable   Type   Ini. Val.               Description
CH3  --------   ----   ---------  --------------------------------------
CH3  maxint     I*4        -      Maximum integer value
CH3  maxi2      I*2        -      Maximum integer value in I*2
CH3  type(3)    CH*9       -      Data type
CH3  ototcn     R*8        -      Total number of counts in the file
CH3
CH4  Logical Units Used:
CH4     Unit#  Variable                   Description
CH4     -----  --------  -----------------------------------------------
CH4       3              Program output listing
CH4
CH4  Method:
CH4    For (k=1 to the number of energy levels) do
CH4       For (j=1 to the number of bins on the y axis) do
CH4	     If (the output map is Aitoff) get the number of bins for the x axis
CH4    	     For (i=1 to the number of bins on the x axis) do
CH4	        Increment the total number of count bins
CH4    		Find the maximum counts and its location
CH4    		Find the minimum count
CH4       	If (the intensity map is requested) then
CH4	  	   Find the maximum exposure bin and its location
CH4	  	   Find the minimum exposure bin
CH4	     	   Compute the current intensity bin from the counts and expos
CH4	  	   Find the maximum intensity bin and its location
CH4	     	   Find the minimum intensity bin
CH4	  	End if
CH4	     End for
CH4	  End for
CH4    End for
CH4    If (all the counts are 0) terminate in error
CH4    Compute the scale and offset for the intensity and exposure bins
CH4    Write information about the output file
CH4  End intens
CH4
CH5 $Log: intens.f,v $
CH5 Revision 1.4  1997/11/03 22:55:50  silvis
CH5 The files used to define the global variables were called: oufits.cmn.f
CH5 and infits.cmn.f and the make file was trying to compile them.  Their
CH5 names were changed to oufits.cmn.inc and infits.cmn.inc and
CH5 were changed in the programs that used these files.
CH5
CH5 Revision 1.3  1997/09/18 19:38:15  silvis
CH5 A large number of changes were made to the code to make it compatible
CH5 with g77.  Most of these changes involved shorting certain lines.
CH5
CH5 Jeff Silvis
CH5 HSTX
CH5 18 Sept. 1997
CH5
CH5 Revision 1.2  1997/09/05 19:59:01  silvis
CH5 Several changes were made to the above code to make it run on Linux.  It
CH5 still does not run on Linux but I wanted to archive the code and test it on
CH5 solaris and sun to confirm that it still runs there.
CH5
CH5
CH5 Jeff Silvis
CH5
CH5 Revision 1.1  1997/09/03 20:15:58  silvis
CH5 This is the intial input to CVS of the ftool eaddmap.  This Ftool will add
CH5 two EGRET maps.
CH5
CH5 Jeff Silvis
CH5 3 Sept 1997
CH5 Hughes STX
CH5
C
C   Revision 2.0    28 April 1997   Jeff Silvis
C   The variable lunout added to the argument call. This variable 
C   provides a logicial number that is used for output to an ascii file.
C   In the original code, this was hardwired to be 3.  Since this is 
C   now an Ftool, the logical number 3 is used by XPI to link with the 
C   parameter file, so the hardwiring causes a conflict.  A value for 
C   lunout was assigned by the fitsio routine ftgiou in the main 
C   routine, eaddamp.  Then lunout is added as an argument to any 
C   routine, such as this one, that has  output to the ascii file.
C
C
C Revision 1.3  1993/05/24  14:01:49  albert
C Computed the scaling factor for the output counts map since the counts are
C now stored as reals.
C
C Revision 1.2  1992/04/16  16:40:47  albert
C Modified to get the bin data arrays as parameters and dimension them variably
C
C Revision 1.1  1992/01/29  19:54:11  albert
C Initial revision
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC intens.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine intens(lunout,intflg,ocntbn,oexpbn,ointbn,*)
      implicit none 

       integer           lunout
      integer   	i,j,k,i1,n1,maxint,maxi2
      logical		intflg
      character 	type(3)*9
       character        id*80
      double precision  twopi,conv,ototcn

      include 'infits.cmn.inc'
      include 'oufits.cmn.inc'

      real ocntbn(onaxs1,onaxs2,onaxs3)
      real oexpbn(onaxs1,onaxs2,onaxs3)
      real ointbn(onaxs1,onaxs2,onaxs3)

      common /id/id
      data   type /'COUNTS', 'EXPOSURE','INTENSITY'/, 
     & maxi2/32767/, ototcn/0.0/

      id= '$Id: intens.f,v 1.4 1997/11/03 22:55:50 silvis Exp $'
      write(lunout,'(///,79(''-''))')
      twopi = atan(1.0) * 8
      conv = twopi / 360.0

C---> Loop over all the bins
      do k=1,onaxs3
  	 do j=1,onaxs2
            n1 = onaxs1
            if (ogridt.eq.'AITF') n1 = onax12(j)
	    do i=1,n1

C------------> Compute the total counts value
	       ototcn = ototcn + ocntbn(i,j,k)

C------------> Find the maximum counts value and its location
               if (ocntbn(i,j,k).gt.omaxvl(1)) then
	          omaxvl(1) = ocntbn(i,j,k)
	          oxmaxp(1) = i
	          oymaxp(1) = j
	          ozmaxp(1) = k
	       end if

C------------> Find the minimum counts value
               if (ocntbn(i,j,k).lt.ominvl(1).and.ocntbn(i,j,k).ne.0) 
     &	          ominvl(1) = ocntbn(i,j,k)

C------------> Test if the intensity map is to be generated
	       if (intflg) then

C---------------> Find the maximum exposure value and its location
                  if (oexpbn(i,j,k).gt.omaxvl(2)) then
	   	     omaxvl(2) = oexpbn(i,j,k)
		     oxmaxp(2) = i
		     oymaxp(2) = j
		     ozmaxp(2) = k
	          end if

C---------------> Find the minimum exposure value
                  if (oexpbn(i,j,k).lt.ominvl(2).and.oexpbn(i,j,k).ne.0) 
     &	             ominvl(2) = oexpbn(i,j,k)

C---------------> Compute the intensity bins
	          if (oexpbn(i,j,k).ne.0)
     &	             ointbn(i,j,k) = ocntbn(i,j,k) / oexpbn(i,j,k)

C---------------> Find the maximum intensity value and its location
                  if (ointbn(i,j,k).gt.omaxvl(3)) then
	   	     omaxvl(3) = ointbn(i,j,k)
		     oxmaxp(3) = i
		     oymaxp(3) = j
		     ozmaxp(3) = k
	          end if

C---------------> Find the minimum intensity value
                  if (ointbn(i,j,k).lt.ominvl(3).and.ointbn(i,j,k).ne.0) 
     &	             ominvl(3) = ointbn(i,j,k)

	       end if
	    end do
	 end do
      end do

C---> Test if any data has been collected
      if (ototcn .eq. 0) goto 100

C---> Compute the scale and offset for the intensity and exposure maps
      i1 = 1
      if (intflg) i1 = 3
      maxint = maxi2
      write(lunout,'(/,''Information about the output file:'',/)')
      write(lunout,*) 'Data Type      Max Bin ',
     &     'Location of Max (x,y,z)                Min Bin'
      do i=1,i1
         obscal(i) = omaxvl(i) / maxint
         obzero(i) = 0.0
	 oblank(i) = 0
         if (obscal(i).eq.0) then
            obscal(i) = 1.0
            write(lunout,*) 
     &        'Warning: all bins in the ',type,' map are 0'
         end if
	 if (ogridt.eq.'AITF') ocdel1 = twopi/onax12(int(oymaxp(i)))
	 oxmaxp(i) = (ocrvl1 + ocdel1*(oxmaxp(i)-ocrpx1+0.5)) / conv
	 oymaxp(i) = (ocrvl2 + ocdel2*(oymaxp(i)-ocrpx2+0.5)) / conv
	 write(lunout,'(a9,4x,e13.7,4x,2(f7.2,2x),i2,5x,e13.7)')
     &      type(i),omaxvl(i),oxmaxp(i),oymaxp(i),ozmaxp(i),ominvl(i)
      end do
      write(lunout,'(/,''Total number of counts ='',i9)') nint(ototcn)

      return

100   continue
      write(lunout,*) 'intens - ERROR: all counts bins are zero'
      return 1

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC End intens.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      end
