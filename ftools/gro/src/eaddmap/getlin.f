CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC getlin.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  getlin
CH1
CH1  $Id: getlin.f,v 1.2 1997/09/18 19:38:13 silvis Exp $
CH1
CH1  Programmer(s) and Completion Date:
CH1     Albert Etienne - Hughes-STX - 01/29/92
CH1
CH1  Function: Get the line parameters between 2 given points
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  Sun 4/280 - Fortran 2.1
CH1
CH2  Calling Sequence:  call getlin(pt1,pt2,slope,intrp)
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2	pt1(2)	    R*8	    I	x and y coordinates of the 1st point of the line
CH2	pt2(2)	    R*8	    I	x and y coordinates of the 2nd point of the line
CH2	slope	    R*8	    O   Slope of the line found
CH2	intrp	    R*8     O   Intercept of the line found
CH2
CH2  Called by:  inbox, inters, polyar
CH2
CH2  Calls: None
CH2
CH3  COMMON Use: None
CH3
CH3  Significant Local Variables: None
CH3  Variable   Type   Ini. Val.               Description
CH3  --------   ----   ---------  --------------------------------------
CH3
CH4  Logical Units Used: None
CH4     Unit#  Variable                   Description
CH4     -----  --------  -----------------------------------------------
CH4
CH4  Method:
CH4    If (the line is not vertical) then
CH4	  Compute the slope of the line from the points
CH4	  Compute the line intercept from the slope and a point
CH4    Else
CH4	  Set the slope to a large number
CH4    End if
CH4  End getlin
CH4
CH5 $Log: getlin.f,v $
CH5 Revision 1.2  1997/09/18 19:38:13  silvis
CH5 A large number of changes were made to the code to make it compatible
CH5 with g77.  Most of these changes involved shorting certain lines.
CH5
CH5 Jeff Silvis
CH5 HSTX
CH5 18 Sept. 1997
CH5
CH5 Revision 1.1  1997/09/03 20:15:56  silvis
CH5 This is the intial input to CVS of the ftool eaddmap.  This Ftool will add
CH5 two EGRET maps.
CH5
CH5 Jeff Silvis
CH5 3 Sept 1997
CH5 Hughes STX
CH5
c Revision 1.2  1993/05/24  13:55:55  albert
c Set line intercept for lines with infinite slope.
c
c Revision 1.1  1992/01/29  19:54:11  albert
c Initial revision
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC getlin.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine getlin(pt1,pt2,slope,intrp) 
      implicit none 

        character        id*80
      double precision  pt1(2),pt2(2),slope,intrp

      common /id/id

      id= '$Id: getlin.f,v 1.2 1997/09/18 19:38:13 silvis Exp $'

C---> Compute the line parameters from point pt1 to point pt2 if not vertical
      if (pt1(1) .ne. pt2(1)) then
	 slope = (pt2(2)-pt1(2)) / (pt2(1)-pt1(1))
	 intrp = pt1(2) - slope*pt1(1)

C---> For vertical lines, set a very large slope
      else
	 slope = 1.0e10
	 intrp = pt1(1)
      end if

      return

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC End getlin.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      end
