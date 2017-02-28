CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC inters.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  inters
CH1
CH1  $Id: inters.f,v 1.2 1997/09/18 19:38:16 silvis Exp $
CH1
CH1  Programmer(s) and Completion Date:
CH1     Albert Etienne - Hughes-STX - 01/29/92
CH1
CH1  Function: Determines if the intersection of 2 lines defined by 2 end 
CH1            points (each) is between the end points
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  Sun 4/280 - Fortran 2.1
CH1
CH2  Calling Sequence:  test = inters(pa1,pa2,pb1,pb2,vert,pt)
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2	pa1(2)	    R*8	    I	x and y coordinates of point 1 of line a
CH2	pa2(2)	    R*8	    I	x and y coordinates of point 2 of line a
CH2	pb1(2)	    R*8	    I	x and y coordinates of point 1 of line b
CH2	pb2(2)	    R*8	    I	x and y coordinates of point 2 of line b
CH2	vert	    L*4	    I	Test if line b is vertical
CH2	pt(2)	    R*8	    O	x and y coordinate of the intersection
CH2
CH2  Called by:  addiff
CH2
CH2  Calls:
CH2   getlin: Get the parameters of a line given 2 end points
CH2
CH3  COMMON Use: None
CH3
CH3  Significant Local Variables:
CH3  Variable   Type   Ini. Val.               Description
CH3  --------   ----   ---------  --------------------------------------
CH3  slopea     R*8        -      slope of line a
CH3  intrpa     R*8        -      intercept of line a
CH3
CH4  Logical Units Used:
CH4     Unit#  Variable                   Description
CH4     -----  --------  -----------------------------------------------
CH4
CH4  Method:
CH4    Get the parameters of line a from point a1 to point a2
CH4    If (line b is vertical) then
CH4       Compute the coordinates of the intersection
CH4       Test if the intersection is between the 2 end points of each line
CH4    Else
CH4       Compute the coordinates of the intersection
CH4       Test if the intersection is between the 2 end points of each line
CH4    End if
CH4  End inters
CH4
CH5 $Log: inters.f,v $
CH5 Revision 1.2  1997/09/18 19:38:16  silvis
CH5 A large number of changes were made to the code to make it compatible
CH5 with g77.  Most of these changes involved shorting certain lines.
CH5
CH5 Jeff Silvis
CH5 HSTX
CH5 18 Sept. 1997
CH5
CH5 Revision 1.1  1997/09/03 20:15:59  silvis
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
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC inters.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      logical function inters(pa1,pa2,pb1,pb2,vert,pt) 
      implicit none 

       logical		vert
       character        id*80
      double precision  pa1(2),pa2(2),pb1(2),pb2(2),pt(2)
      double precision  slopea,intrpa

      common /id/id

      id= '$Id: inters.f,v 1.2 1997/09/18 19:38:16 silvis Exp $'

C---> Get the parameters of line a from pa1 to pa2
      call getlin(pa1,pa2,slopea,intrpa)

C---> Test if line b (from the input bin) is vertical
      if (vert) then

C------> Compute the coordinates of the intersection of the 2 lines
	 pt(1) = pb1(1)
	 pt(2) = slopea*pt(1) + intrpa

C------> Find if is the intersection is between the 2 end points of each line
         inters = pt(1).ge.min(pa1(1),pa2(1)) .and. 
     &            pt(1).le.max(pa1(1),pa2(1)) .and. 
     &            pt(2).ge.min(pb1(2),pb2(2)) .and. 
     &            pt(2).le.max(pb1(2),pb2(2))

C---> Otherwise line b is horizontal (the input bin is parallel to coord system)
      else

C------> Compute the coordinates of the intersection of the 2 lines
	 pt(2) = pb1(2)
	 pt(1) = 1.0e10
	 if (slopea .ne. 0) pt(1) = (pt(2)-intrpa) / slopea 

C------> Find if is the intersection is between the 2 end points of each line
         inters = pt(1).ge.min(pb1(1),pb2(1)) .and. 
     &            pt(1).le.max(pb1(1),pb2(1)) .and. 
     &            pt(2).ge.min(pa1(2),pa2(2)) .and. 
     &            pt(2).le.max(pa1(2),pa2(2))

      end if
	 
      return

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC End inters.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      end
