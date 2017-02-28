CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC tstmap.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  tstmap
CH1
CH1  $Id: tstmap.f,v 1.7 2013/05/21 19:08:23 irby Exp $
CH1
CH1  Programmer(s) and Completion Date:
CH1     Albert Etienne - Hughes-STX - 01/29/92
CH1
CH1  Function: Tests whether the current input map has the same parameters
CH1	       as the output map or not.
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  Sun 4/280 - Fortran 2.1
CH1
CH2  Calling Sequence:  case = tstmap(in)
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2	in	    I*4	    I	Index of the number of input maps (1 or 2)
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
CH3
CH4  Logical Units Used:
CH4     Unit#  Variable                   Description
CH4     -----  --------  -----------------------------------------------
CH4       3              Program output listing
CH4
CH4  Method:
CH4    Test if the maps have the same coordinate system
CH4    Test if the maps have the same basic FITS structure
CH4    Test if the maps have the same bin size and reference point
CH4    If (this is the 1st input map) then
CH4	  Assign the energy levels of the output map to those of the input map
CH4    Else
Ch4	  Test if the map energy levels are the same
CH4	  Test if the earth cutoff angles are the same
CH4    End if
CH4  End tstmap
CH4
CH5 $Log: tstmap.f,v $
CH5 Revision 1.7  2013/05/21 19:08:23  irby
CH5 Change character*n to character(n) to silence warnings: "Obsolescent
CH5 feature: Old-style character length".
CH5
CH5 Revision 1.6  2005/08/26 20:10:26  irby
CH5 - Fix broken write statement at line 333.
CH5 - Cosmetic changes to lines longer than 72 chars to allow compilation
CH5   with gfortran/g95 (which were otherwise being truncated).
CH5
CH5 Revision 1.5  1997/11/16 21:59:31  silvis
CH5 Changes were made to allow code to run under OSF.   This mostly involved
CH5 changing format statments for write commands and putting additional
CH5 parenthesises in several if statments.
CH5
CH5 Revision 1.4  1997/11/03 22:55:54  silvis
CH5 The files used to define the global variables were called: oufits.cmn.f
CH5 and infits.cmn.f and the make file was trying to compile them.  Their
CH5 names were changed to oufits.cmn.inc and infits.cmn.inc and
CH5 were changed in the programs that used these files.
CH5
CH5 Revision 1.3  1997/09/18 19:38:23  silvis
CH5 A large number of changes were made to the code to make it compatible
CH5 with g77.  Most of these changes involved shorting certain lines.
CH5
CH5 Jeff Silvis
CH5 HSTX
CH5 18 Sept. 1997
CH5
CH5 Revision 1.2  1997/09/05 19:59:07  silvis
CH5 Several changes were made to the above code to make it run on Linux.  It
CH5 still does not run on Linux but I wanted to archive the code and test it on
CH5 solaris and sun to confirm that it still runs there.
CH5
CH5
CH5 Jeff Silvis
CH5
CH5 Revision 1.1  1997/09/03 20:16:06  silvis
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
C Revision 1.4  1993/02/22  14:35:48  albert
C Allowed more coordinate systems to be used (earth, instrument, sun or moon
C centered).
C
C Revision 1.3  1992/06/15  18:10:41  albert
C Wrote warning messages about non-matching energy levels and zenith angles
C to the terminal (in addition to the report file).
C
C Revision 1.2  1992/04/16  16:55:13  albert
C Modified to test that the output map total size does not exceed the new
C maximum of 648000 and that the maximum number of bins on the X and Y axis
C is under 720 and 360 respectively.
C
C Revision 1.1  1992/01/29  19:54:11  albert
C Initial revision
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC tstmap.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      character(4) function tstmap(lunout,in,maxsiz)
      implicit none 

       integer lunout
      integer maxsiz,in,i
      character case*4
       character        id*80

      include 'infits.cmn.inc'
      include 'oufits.cmn.inc'
      common /id/id

      id= '$Id: tstmap.f,v 1.7 2013/05/21 19:08:23 irby Exp $'

      case = 'diff'
      if (in .eq. 1) then
	 onaxs3 = inaxs3
         if (onaxs1*onaxs2*onaxs3.gt.maxsiz) then
            write(lunout,*) 'Error: too much data in the output map:'
            write(lunout,*) '       naxis1*naxis2*naxis3 is >',maxsiz
            case = 'bad'
	    goto 100
         endif
         if (onaxs1.lt.3.or.onaxs1.gt.oxl) then
            write(lunout,*) 
     &       'Error: Number of bins on X axis < 3 or >',oxl
            case = 'bad'
	    goto 100
         endif
         if (onaxs2.lt.3.or.onaxs2.gt.oyl) then
            write(lunout,*) 
     &        'Error: Number of bins on Y axis < 3 or >',oyl
            case = 'bad'
	    goto 100
         endif
      end if

C---> Test if the maps have different coordinate system (not parallel)
      if (icoord .ne. ocoord) then
	 if ((icoord.ne.'GALA' .and. icoord.ne.'CELE') .or.
     &   (ocoord.ne.'GALA' .and. ocoord.ne.'CELE')) then
     	   write(lunout,*) 'tstmap - ERROR: input coordimate system'
 	   write(lunout,*) 
     &     'may differ from the output map system only if 1 is Galactic'
	   write(lunout,*)
     &      ' and the other is Celestial'
	    case = 'bad'
	 end if
	 goto 100
      end if

      case = 'csys'

C---> Test if the maps have the same basic FITS stucture
      if (igrcnt.ne.ogrcnt .or. iprcnt.ne.oprcnt .or. 
     &  igridt.ne.ogridt) goto 100

C---> Test for same bin size and starting point
      if (icdel1.ne.ocdel1 .and. ogridt.ne.'AITF') goto 100
      if (icdel2.ne.ocdel2 .or. icrvl1.ne.ocrvl1 .or. icrvl2.ne.ocrvl2 
     & .or. icrpx1.ne.ocrpx1 .or. icrpx2.ne.ocrpx2) goto 100
 
      case = 'same'

100   continue

C---> Test additional information
      do i=1,min(inaxs3,onaxs3)

C------> The energy ranges of the output file should match the first file's
	 if (in .eq. 1) then
	    oenrgy(1,i) = ienrgy(1,i)
	    oenrgy(2,i) = ienrgy(2,i)
	    oznmax(i) = iznmax(i)

C------> Test if the energy levels are the same
	 else
	    if ((ienrgy(1,i).ne.oenrgy(1,i)) .or. 
     &             (ienrgy(2,i).ne.oenrgy(2,i))) then
	       write(lunout,*) ' '
	       write(lunout,*) 'Warning: energy ranges',i,
     &		          ' of the 2 maps do not match'
C *******************************************************************
C                          Ftool Change
C
C     OSF does not support the / in a format statemant that is within 
C     the actual write statment.  So a separate format statement must be 
C     writen. 
C            write(lunout,'(9x,''The maps will still be added level by level'')')
C
C
C*********************************************************************
  22           format(9x,"The maps will still be added level by level")
               write(lunout,22)


               write(lunout,33)
C	       write(lunout,'(9x,''Energy range of input map'',i2,3x,'' ='',f7.1,
C    &	          '' -'',f7.1,'' Mev'')') in,ienrgy(1,i),ienrgy(2,i)
  33           format(9x,"Energy range of input map",i2,3x," =",f7.1,
     &	          " -",f7.1," Mev") 
               write(lunout,33) in,ienrgy(1,i),ienrgy(2,i)


C     	       write(lunout,'(9x,''Energy range of the output map ='',f7.1,
C    &	          '' -'',f7.1,'' Mev'')') oenrgy(1,i),oenrgy(2,i)
  44           format(9x,"Energy range of the output map =",f7.1,
     &	          " -",f7.1," Mev")
               write(lunout,44) oenrgy(1,i),oenrgy(2,i)         

     
	       write(6,*) 'Warning: energy ranges',i,
     &		          ' of the 2 maps do not match'
	       write(6,22)


C	       write(6,'(9x,''Energy range of input map'',i2,3x,'' ='',f7.1,
C    &	          '' -'',f7.1,'' Mev'')') in,ienrgy(1,i),ienrgy(2,i)
  55           format(9x,"Energy range of input map",i2,3x," =",f7.1,
     &	          " -",f7.1," Mev") 
                write(6,55)in,ienrgy(1,i),ienrgy(2,i)

 
C     	       write(6,'(9x,''Energy range of the output map ='',f7.1,
C    &	          '' -'',f7.1,'' Mev'')') oenrgy(1,i),oenrgy(2,i)
  66           format(9x,"Energy range of the output map =",f7.1,
     &	          " -",f7.1," Mev")   
               write(6,66) oenrgy(1,i),oenrgy(2,i)
	    end if

C---------> Test if the earth cutoff angles are the same
	    if (iznmax(i).ne.oznmax(i)) then 
	       write(lunout,*) 'Warning: earth cutoff angles for level'
     &                    ,i,' do not match:',iznmax(i),oznmax(i)
	       write(6,*) 'Warning: earth cutoff angles for level',i,
     &                    ' do not match:',iznmax(i),oznmax(i)
	    end if

	 end if

      end do

      tstmap = case

      return

CCCCCCCCCCCCCCCCCCCCCCCCCCC End tstmap.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      end
