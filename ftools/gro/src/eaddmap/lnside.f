CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC lnside.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  lnside
CH1
CH1  $Id: lnside.f,v 1.2 1997/09/18 19:38:17 silvis Exp $
CH1
CH1  Programmer(s) and Completion Date:
CH1     Albert Etienne - Hughes-STX - 01/29/92
CH1
CH1  Function: Finds which side of a line a given point is
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  Sun 4/280 - Fortran 2.1
CH1
CH2  Calling Sequence:  test = lnside(pt,slope,intrp)
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2	pt(2)	    R*8	    I	x and y coordinates of the point to test
CH2	slope	    R*8	    I	slope of the line
Ch2	intrp	    R*8	    I	intercept of the line
CH2
CH2  Called by:  inbox, polyar
CH2
CH2  Calls: None
CH2
CH3  COMMON Use: None
CH3
CH3  Significant Local Variables:
CH3  Variable   Type   Ini. Val.               Description
CH3  --------   ----   ---------  --------------------------------------
CH3  diff       R*8        -      Result of plugging the point into the line
CH3				  equation
CH3
CH4  Logical Units Used:
CH4     Unit#  Variable                   Description
CH4     -----  --------  -----------------------------------------------
CH4
CH4  Method:
CH4    Plug the point into the equation of the line
CH4    Return T or F depending on the sign of the result
CH4  End lnside
CH4
CH5 $Log: lnside.f,v $
CH5 Revision 1.2  1997/09/18 19:38:17  silvis
CH5 A large number of changes were made to the code to make it compatible
CH5 with g77.  Most of these changes involved shorting certain lines.
CH5
CH5 Jeff Silvis
CH5 HSTX
CH5 18 Sept. 1997
CH5
CH5 Revision 1.1  1997/09/03 20:16:00  silvis
CH5 This is the intial input to CVS of the ftool eaddmap.  This Ftool will add
CH5 two EGRET maps.
CH5
CH5 Jeff Silvis
CH5 3 Sept 1997
CH5 Hughes STX
CH5
c Revision 1.1  1992/01/29  19:54:11  albert
c Initial revision
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC lnside.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      logical function lnside(pt,slope,intrp) 
      implicit none 

        character        id*80
      double precision  pt(2),slope,intrp,diff

      common /id/id

      id= '$Id: lnside.f,v 1.2 1997/09/18 19:38:17 silvis Exp $'

C---> Plug the point into the equation of the line to find what side it is on
      if (slope .lt. 1.0e10) then
	 diff = pt(2)  - slope*pt(1) - intrp
      else
	 diff = pt(1) - intrp
      endif

C---> Return TRUE if the point is on the positive side of the line
      if (diff .le. 0) then
         lnside = .false.
      else
	 lnside = .true.
      endif

      return

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC End lnside.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      end
