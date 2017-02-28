*+DEWINDOW
	subroutine dewindow(pha,pi,dx,dy,dx_inter,dy_inter,
     &              yprime,ga,chan,iymap,gy,
     &                 errflg,chatter)
      IMPLICIT NONE 
c --- DESCRIPTION -------------------------------------------
c Subroutine to undo golden disk correction
c -----------------------------------------------------------
c --- AUTHORS/MODIFICATION HISTORY ---
c Dr Jane Turner (August 1995) 1.0.0; Original program
c Rehana Yusaf (Sept 1995) 1.0.1;     Changed it to a subroutine.
c                                     The calculations are now done
c                                     row by row, therefore arrays
c                                     are no longer used.
c                                     Minor cosmetic changes.
c Rehana yusaf (Oct 1995) 1.0.2;      change iymap from int*2 to int
c Rehana Yusaf (Oct 23 1995) 1.0.3; change RAN to ft_ran2
      character(5) version
      parameter (version='1.0.3')
c ---------------------------------------------------------------
*-
c VARIABLES
c
        real dx_inter, yprime,ga(*), GAY,gy,ft_ran2
        integer pha,dx,dy
	real DYM,DYM1,dy_inter
        integer chan(*)
        integer element_x1,element_y1, pi, c1b, PI2, ISEED
	integer element_y2,IYS, rem_x,rem_y
	integer iymap(512,512)
        integer chatter,errflg
        character(70) desc
	ISEED=31456789

c USER INFO

        IF (chatter.GE.40) THEN
           desc = ' ... using dewindow Ver '//version
           call fcecho(desc)
        ENDIF
	   element_x1=INT(dx_inter/16.0)
	   element_y1=INT(dy_inter/16.0)
	   rem_x=INT(dx_inter) - 16*element_x1
	   rem_y=INT(dy_inter) - 16*element_y1
 	   IF (rem_y.le.7) then 
	        IYS= -1
	   ELSE
		IYS=+1
	   ENDIF
		element_y2=element_y1 + IYS

C Get window correction from interpolation between these grid points)

	   IF (element_x1.gt.1.and.element_x1.le.511.and.
     +		element_y1.gt.1.and.element_y1.le.511) THEN
	     DYM=iymap(element_y1,element_x1)
	     DYM1=iymap(element_y2,element_x1)
	     gy=DYM+IYS*(rem_y-7.5)/16.*(DYM1-DYM)
	     PI2=min(pi,256)
	     PI2=max(1,PI2)
	     GAY=ga(PI2)*gy
	     yprime=dy_inter - ft_ran2(ISEED) + 0.5 + GAY
	   ELSE 
		yprime=dy_inter
	   ENDIF
	   c1b=INT(pha)
        return
        end
