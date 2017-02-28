c   Routine name - csint
c
c   Language     - Fortran
c
c   Purpose      - Integrates the function s(x)[*x],
c                    where s is sampled in a table.
c
c   Date         - July 26, 1990
c                 Rewritten December 1994 by PLN for Spectral version
c                 2.7.  Removed obsolete references to cubic splines
c                 and implemented bin-centered integration.
c
c   Usage        - call csint(x,y,nx,a,b,m,q,ierr)
c
c   Arguments    x    -  Array of length nx containing the x-values
c                        of the data points.  X must be ordered so
c                        that x(i) < x(i+1).  (real*8,input)
c                y    -  Array of length nx containing the y-values
c                        (s(x(i),i=1..nx) of the data points.
c                        (real*8,input)
c                nx   -  number of elements in arrays.  Must be => 2.
c                        (integer,input)
c                a,b  -  Limits of integration.  If the order is switched
c                        the sign of the output switches. (real*8,input)
c                m    -  If m <> 0 integral is of (function * x).
c                q    -  Integral from a to b.  (real*8,output)
c                ierr -  Warning parameter.  If ierr = 1, a and/or b is
c                        <x(1); if ierr = 2, a and/or b is >x(nx).
c                        (integer,output)

c  @(#) csint.f 1.2@(#)

      subroutine csint(x,y,nx,a,b,m,q,ierr)

      implicit none

c     ARGUMENTS
      integer nx,ierr,m
      real*8 x(nx),y(nx),a,b,q
c     LOCAL VARIABLES
      integer ia,ib,i
      real*8 da,db,dx,qa,qb,qab,vmin,vmax

      save

c     EXECUTABLE STATEMENTS

      ierr = 0
      
c     FIND THE STARTING POINT
      vmin = min(a,b)
      call locate8(x,nx-1,vmin,ia)
      if (ia.eq.0) then
         ia = 1
         ierr = 1
      end if
      da = vmin - x(ia)
      
c     FIND THE ENDING POINT
      vmax = max(a,b)
      call locate8(x,nx,vmax,ib)
      if (ib.eq.nx) then
         ib = nx-1
         ierr= 2
      end if
      if (ib.eq.0) ib = 1
      db = vmax - x(ib)

c     INTEGRATE FUNCTION
      if (m.eq.0) then    ! integrate function
         qa = (y(ia+1)+y(ia))*da
         qb = (y(ib+1)+y(ib))*db
         qab = 0.0d0
         do i = ia,ib-1
            dx = x(i+1) - x(i)
            qab = qab + (y(i)+y(i+1))*dx
         end do   
         q = .5d0 * (qab + qb - qa)
      else   ! integrate x * function
         qa = da*(y(ia)*(vmin+2.d0*x(ia))+y(ia+1)*(2.d0*vmin+x(ia)))
         qb = db*(y(ib)*(vmax+2.d0*x(ib))+y(ib+1)*(2.d0*vmax+x(ib)))
         qab = 0.0d0
         do i = ia,ib-1
            dx = x(i+1) - x(i)
            qab = qab + dx*(y(i)*(2.d0*x(i)+x(i+1))+y(i+1)*(x(i)+
     >           2.d0*x(i+1)))
         end do
         q = (qab + qb - qa) / 6.d0
      end if
      if (b.lt.a) q = -q

      return

      end
