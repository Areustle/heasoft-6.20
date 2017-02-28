CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC polyar.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  polyar
CH1
CH1  $Id: polyar.f,v 1.3 2005/08/26 19:36:34 irby Exp $
CH1
CH1  Programmer(s) and Completion Date:
CH1     Albert Etienne - Hughes-STX - 01/29/92
CH1
CH1  Function: Returns the area of the polygon passed to it
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  Sun 4/280 - Fortran 2.1
CH1
CH2  Calling Sequence:  area = polyar(n,polygn)
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2	n	    I*4	    I	Number of vertices in the polygon
Ch2	polygn(2,10)R*8	    I	x and y coordinate of each vertex of the polygon
CH2
CH2  Called by:  addiff
CH2
CH2  Calls:
CH2   getlin: Get the parameters of a line from its 2 end points
CH2   lnside: Determines what side of a line a given point is
CH2
CH3  COMMON Use: None
CH3
CH3  Significant Local Variables:
CH3  Variable   Type   Ini. Val.               Description
CH3  --------   ----   ---------  --------------------------------------
CH3  newpol(2,10) R*8      -      Polygon obtained
CH3  slope      R*8        -      Slope of the current line
CH3  intrp      R*8        -      Intercept of the current line
CH3  area       R*8        -      Area of the polygon
CH3
CH4  Logical Units Used:
CH4     Unit#  Variable                   Description
CH4     -----  --------  -----------------------------------------------
CH4
CH4  Method:
CH4    Sort the polygon in increasing values of x
CH4    Get the parameters of the line from the minx to the max x
CH4    For (i = 2 to Number of vertices) do
CH4       If (point i is above the line) then
CH4	     Store the point at the next position in the beginning of the list
CH4	  Else
CH4	     Store the point at the next position at the end of the list
CH4	  End if
CH4	  For (i=1 to the number of points in the polygon) do
CH4	     Increment the area of the polygon with the current area
CH4	  End for
CH4    End for
CH4  End polyar
CH4
CH5 $Log: polyar.f,v $
CH5 Revision 1.3  2005/08/26 19:36:34  irby
CH5 Purely cosmetic changes to allow compilation with gfortran/g95, mostly
CH5 involving fixes to lines longer than 72 chars that were being truncated,
CH5 but in the case of the CGRO code, also moving misplaced (tabbed) line
CH5 continuation characters to their appropriate position in column 6.
CH5
CH5 Revision 1.2  1997/09/18 19:38:21  silvis
CH5 A large number of changes were made to the code to make it compatible
CH5 with g77.  Most of these changes involved shorting certain lines.
CH5
CH5 Jeff Silvis
CH5 HSTX
CH5 18 Sept. 1997
CH5
CH5 Revision 1.1  1997/09/03 20:16:04  silvis
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
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC polyar.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      double precision function polyar(n,polygn) 
      implicit none 

       integer   	n,i,j,k,l
      logical   	lnside
       character        id*80
      double precision  polygn(2,10),newpol(2,10),slope,intrp,area,x,y

      common /id/id

      id= '$Id: polyar.f,v 1.3 2005/08/26 19:36:34 irby Exp $'

C---> Sort the polygon in x values
      j = 0
      do i = 1,n
	 k = i
	 do j = i,n
            if (polygn(1,j) .lt. polygn(1,k)) k = j
         end do
	 if (k .ne. i) then
	    x = polygn(1,i)
	    y = polygn(2,i)
            polygn(1,i) = polygn(1,k)
            polygn(2,i) = polygn(2,k)
            polygn(1,k) = x
            polygn(2,k) = y
	 end if
	 newpol(1,n) = 99999
      end do

C---> Get the parameters of the line between the leftmost & the rightmost points
      call getlin(polygn(1,1),polygn(1,n),slope,intrp)

C---> Loop from the leftmost to the rightmost point and sort the points
      j = 1
      k = n + 1
      newpol(1,1) = polygn(1,1)
      newpol(2,1) = polygn(2,1)
      do i = 2,n
	 if (lnside(polygn(1,i),slope,intrp)) then
	    j = j + 1
	    l = j
	 else
	    k = k - 1
	    l = k
	 end if
	 newpol(1,l) = polygn(1,i)
	 newpol(2,l) = polygn(2,i)
      end do

C---> Compute the area of the sorted polygon
      area = 0.0
      do i=1,n
	 j = mod(i,n) + 1
	 area = area + (newpol(1,j)-newpol(1,i)) *
     &                 (newpol(2,j)+newpol(2,i))/2.0
      end do

      polyar = area

      return

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC End polyar.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      end
