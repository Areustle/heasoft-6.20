CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC inbox.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  inbox
CH1
CH1  $Id: inbox.f,v 1.3 2005/08/26 19:36:34 irby Exp $
CH1
CH1  Programmer(s) and Completion Date:
CH1     Albert Etienne - Hughes-STX - 01/29/92
CH1
CH1  Function: Logical function to determine if a given point is in a given box
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  Sun 4/280 - Fortran 2.1
CH1
CH2  Calling Sequence:  test = inbox(par,box,pt)
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2	par	    L*4	    I	Specifies if the box is parallel to the 
CH2				coordinate system
CH2	box(2,4)    R*8	    I	x and y coordinates of the 4 points of the box
Ch2	pt(2)	    R*8	    I	x and y coordinates of the point
CH2
CH2  Called by:  addiff
CH2
CH2  Calls:
CH2   getlin: gets the parameters of a line
CH2   lnside: determines on which side of a line a point is
CH2
CH3  COMMON Use: None
CH3
CH3  Significant Local Variables:
CH3  Variable   Type   Ini. Val.               Description
CH3  --------   ----   ---------  --------------------------------------
CH3  quit       L*4        F      Flag to test if processing should be cancelled
CH3  slope	R*8	   -	  Slope of the current line
CH3  intrp	R*8	   -	  Intercept of the current line
CH3
CH4  Logical Units Used: None
CH4     Unit#  Variable                   Description
CH4     -----  --------  -----------------------------------------------
CH4
CH4  Method:
CH4    If (the box is parallel to the coordinate system) then
CH4       Test if the point is between the x & y coordinates of the box
CH4    Else    
CH4       Loop over the 4 line of the box
CH4	     Call getlin to get the current line parameters
CH4	     If (the point is not on the same side of the line as the next point
CH4	     of the box) then
CH4	        Exit the loop
CH4	  End loop
CH4    End if
CH4  End inbox
CH4
CH5 $Log: inbox.f,v $
CH5 Revision 1.3  2005/08/26 19:36:34  irby
CH5 Purely cosmetic changes to allow compilation with gfortran/g95, mostly
CH5 involving fixes to lines longer than 72 chars that were being truncated,
CH5 but in the case of the CGRO code, also moving misplaced (tabbed) line
CH5 continuation characters to their appropriate position in column 6.
CH5
CH5 Revision 1.2  1997/09/18 19:38:14  silvis
CH5 A large number of changes were made to the code to make it compatible
CH5 with g77.  Most of these changes involved shorting certain lines.
CH5
CH5 Jeff Silvis
CH5 HSTX
CH5 18 Sept. 1997
CH5
CH5 Revision 1.1  1997/09/03 20:15:57  silvis
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
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC inbox.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      logical function inbox(par,box,pt) 
      implicit none 

       integer   	i,j,k
      logical   	par,quit,lnside
       character        id*80
      double precision  box(2,4),pt(2),slope,intrp

      common /id/id

      id= '$Id: inbox.f,v 1.3 2005/08/26 19:36:34 irby Exp $'

C---> If the box is parallel to the coordinate system, then do a simple test
      if (par) then
	 inbox = box(1,1).le.pt(1).and.pt(1).le.box(1,2) .and.
     &		 box(2,1).le.pt(2).and.pt(2).le.box(2,4)

C---> The box is not parallel to the coordinate system: do the complete test
      else
         i = 0
         quit = .false.

C------> Loop over the 4 lines of the box
	 do while (i.lt.4 .and. .not.quit)
	    i = i + 1
	    j = mod(i,4) + 1
	    k = mod(j,4) + 1

C---------> Get the parameters of the line from point i to point j of the box
	    call getlin(box(1,i),box(1,j),slope,intrp)

C---------> Find if point pt is on the same side of the line as point k of box
	    if (lnside(pt,slope,intrp) .neqv.
     &          lnside(box(1,k),slope,intrp))
     &	       quit=.true.
         end do

         inbox = .not.quit
      end if

      return

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC End inbox.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      end
