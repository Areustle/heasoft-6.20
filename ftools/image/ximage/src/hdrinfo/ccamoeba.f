c
c  Contains amoeba (externally callable) and amotry (internal use only)
c
c  Modified version of NUMERICAL RECIPES IN FORTRAN 77 (p. 404-6)
c
      SUBROUTINE ccamoeba(p,y,mp,np,ndim,ftol,funk,pdim,npts,xref,yref,
     &                    xskew,yskew,iter)

      IMPLICIT NONE
c
c  I/O  p         (r) Input matrix
c  I/O  y         (r) Input vector
c   I   mp/np     (i) Dimensions of p
c   I   ndim      (i) Contents of p are ndim,ndim+1 in size
c   I   ftol      (r) Fractional convergence tolerance
c   I   funk      (f) Minimization function
c   I   pdim      (i) Dimensions of x/yref, x/yskew
c   I   npts      (i) Number of points in x/yref, x/yskew
c   I   x/yref    (r) Points in reference coordinates
c   I   x/yskew   (r) Points in skewed coordinates
c   O   iter      (i) Number of iterations
c
      INTEGER iter,mp,ndim,np,pdim,npts,NMAX,ITMAX
      REAL ftol,p(mp,np),y(mp),funk,TINY
      REAL xref(pdim),yref(pdim),xskew(pdim),yskew(pdim)
      PARAMETER (NMAX=20,ITMAX=5000,TINY=1.e-10)

c      NMAX = Maximum allowed dimensions
c      ITMAX = Maximum allowed function evaluations
c      TINY = Small number

      EXTERNAL funk
c
c  Multidimensional minimization of the function funk(x,...) where
c  x(1:ndim) is a vector in ndim dimensions and the other arguments
c  are constants, by the downhill simplex method of Nelder and Mead.  
c  The matrix p(1:ndim+1,1:ndim)
c  is input.  Its ndim+1 rows are ndim-dimensional vectors which are
c  the vertices of the starting simplex.  Also input is the vector
c  y(1:ndim+1), whose components must be pre-initialized to the values
c  of funk evaluated ar the ndim+1 vertices (rows) of p; and ftol
c  the fractional convergence tolerance to be achieved in the function
c  value (n.b.!). On output, p and y will have been reset to ndim+1 
c  new points all within ftol of a minimum function value, and iter
c  gives the number of function evaluations taken.
c
      INTEGER i,ihi,ilo,inhi,j,m,n
      REAL rtol,sum,swap,ysave,ytry,psum(NMAX),amotry
      iter=0
 1    do n=1,ndim
         sum=0.
         do m=1,ndim+1
            sum=sum+p(m,n)
         enddo
         psum(n)=sum
      enddo
 2    ilo=1
      if (y(1).gt.y(2)) then
         ihi=1
         inhi=2
      else
         ihi=2
         inhi=1
      endif
      do i=1,ndim+1
         if (y(i).le.y(ilo)) ilo=i
         if (y(i).gt.y(ihi)) then
            inhi=ihi
            ihi=i
         else if(y(i).gt.y(inhi)) then
            if (i.ne.ihi) inhi=i
         endif
      enddo
      rtol=2.*abs(y(ihi)-y(ilo))/(abs(y(ihi))+abs(y(ilo))+TINY)
c
c  Compute the fractional range from the highest to lowest and return
c  if satisfactory
c
      if (rtol.lt.ftol) then
c
c        If returning, place best point and value in slot 1
c
         swap=y(1)
         y(1)=y(ilo)
         y(ilo)=swap
         do n=1,ndim
            swap=p(1,n)
            p(1,n)=p(ilo,n)
            p(ilo,n)=swap
         enddo
         return
      endif
      if (iter.ge.ITMAX) then
         call xwarn('ITMAX exceeded in amoeba', 5)
         return
      endif 
      iter=iter+2
c
c      Begin new iteration.  First extrapolate by a factor -1 through the
c      face of the simplex across from the high point, i.e. reflect the
c      simplex from the high point
c
      ytry=amotry(p,y,psum,mp,np,ndim,funk,pdim,npts,xref,yref,
     &                xskew,yskew,ihi,-1.0)
      if (ytry.le.y(ilo)) then
c
c         Gives a result better than the best point, so try an
c         additional extrapolation by a factor of 2
c
         ytry=amotry(p,y,psum,mp,np,ndim,funk,pdim,npts,xref,yref,
     &                xskew,yskew,ihi,2.0)
      else if (ytry.ge.y(inhi)) then
c
c         The reflected point is worse than the second-highest, so look
c         for an intermediate lower point, i.e., do a one-dimensional
c         contraction.
c
         ysave=y(ihi)
         ytry=amotry(p,y,psum,mp,np,ndim,funk,pdim,npts,xref,yref,
     &                xskew,yskew,ihi,0.5)
         if (ytry.ge.ysave) then
c
c            Can't seem to get rid of that high point.  Better contract
c            around the lowest (best) point
c
            do i=1,ndim+1
               if (i.ne.ilo) then
                  do j=1,ndim
                     psum(j)=0.5*(p(i,j)+p(ilo,j))
                     p(i,j)=psum(j)
                  enddo
                  y(i)=funk(psum,pdim,npts,xref,yref,xskew,yskew)
               endif
            enddo
            iter=iter+ndim
            goto 1
         endif
      else
         iter=iter-1
      endif
      goto 2
      END

      FUNCTION amotry(p,y,psum,mp,np,ndim,funk,pdim,npts,xref,yref,
     &                xskew,yskew,ihi,fac)
c
c  I/O  p         (r) Input matrix
c  I/O  y         (r) Input vector
c   I   psum      (r) Array of point sums
c   I   mp/np     (i) Dimensions of p
c   I   ndim      (i) Contents of p are ndim,ndim+1 in size
c   I   funk      (f) Minimization function
c   I   pdim      (i) Dimensions of x/yref, x/yskew
c   I   npts      (i) Number of points in x/yref, x/yskew
c   I   x/yref    (r) Points in reference coordinates
c   I   x/yskew   (r) Points in skewed coordinates
c   I   ihi       (i) Index of high point
c   O   fac       (r) Extrapolation factor
c
      INTEGER ihi,mp,ndim,pdim,np,NMAX,npts
      REAL amotry,fac,p(mp,np),psum(np),y(mp),funk
      REAL xref(pdim),yref(pdim),xskew(pdim),yskew(pdim)
      PARAMETER (NMAX=20)
      EXTERNAL funk
c
c  Extrapolates by a factor fac through the face of the simplex across
c  from the high point, tries it, and replaces the high point is the new
c  point is better
c
      INTEGER j
      REAL fac1,fac2,ytry,ptry(NMAX)
      fac1=(1.-fac)/ndim
      fac2=fac1-fac
      do j=1,ndim
         ptry(j)=psum(j)*fac1-p(ihi,j)*fac2
      enddo
      ytry=funk(ptry,pdim,npts,xref,yref,xskew,yskew)
c
c      Evaluate the function at the trial point
c
      if (ytry.lt.y(ihi)) then
c
c         If it's better than the highest replace the highest
c
         y(ihi)=ytry
         do j=1,ndim
            psum(j)=psum(j)-p(ihi,j)+ptry(j)
            p(ihi,j)=ptry(j)
         enddo
      endif
      amotry=ytry
      return
      END

