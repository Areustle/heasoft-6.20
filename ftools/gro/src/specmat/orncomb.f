* ORNCOMB
*
* Combine adjacent orientations with identical pointing directions
* and other relevant parameters
*
* Written for Spectral version 2.7
* December 1994, PLN, Stanford
*
* Note:  This code used to be in the main routine "matrix.f".
*
*  @(#) orncomb.f 1.3@(#)

      subroutine orncomb(norns,orn,ornview,orntasc,orntime,calset)

      implicit none

      include '../SPECMAT_COMMON/spectral.inc'

      integer norns
      real orn(2,2,nornmax)
      real*8 orntime(2,nornmax)
      character(4) ornview(nornmax)
      character(2) calset(nornmax)
      logical orntasc(nornmax)

      integer i,j,k,l

      save

      i = 1
      do while (i.lt.norns)
         if  (abs(orn(1,1,i)-orn(1,1,i+1)).lt.0.005.and. !same direction
     >        abs(orn(1,2,i)-orn(1,2,i+1)).lt.0.005.and.
     >        abs(orn(2,1,i)-orn(2,1,i+1)).lt.0.005.and.
     >        abs(orn(2,2,i)-orn(2,2,i+1)).lt.0.005.and.
     >        calset(i).eq.calset(i+1).and.  ! same calib file family
     >        abs(orntime(2,i)-orntime(1,i+1)).lt.60..and. ! no time gap
     >        (ornview(i).eq.ornview(i+1)).and. ! same viewing period
     >        (orntasc(i).eqv.orntasc(i+1))) then ! same TASC in/out 
            write (*,*) 'Combining orientations ',i,' and ',i+1
            orntime(1,i+1)=orntime(1,i)
            do j = i+1,norns
               ornview(j-1) = ornview(j)
	       orntasc(j-1) = orntasc(j)
               calset(j-1)  = calset(j)
               do k = 1,2
                  orntime(k,j-1) = orntime(k,j)
                  do l = 1,2
                     orn(k,l,j-1) = orn(k,l,j)
                  end do
               end do
            end do
            norns = norns - 1
         else
            i = i + 1
         end if
      end do

      return
      end
