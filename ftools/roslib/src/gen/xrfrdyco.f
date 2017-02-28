      subroutine xrfrdyco(lui,dtp,irec,iopt,ivect,frow,trow,crunit
     &                   ,naxis,naxes,eaxis,taxis,y,sy,anynul,ierr)

c ReaD a Y-COlumn value or values from a XRonos Fits file.

c This routine reads a single count rate from a column in a binary
c table.  The column can contain array of any number of dimensions.

c The format of the column is found by using subroutine xrftgycf.
c The information so obtained is passed to this routine as arguments
c (listed below).

c The count rate is returned in y, in counts/sec regardless of the units 
c specified in the file.
c If no error column is present, the poisson error is returned in sy,
c also in counts/sec.

c The structure for the error array, if present, is assumed
c to be identical to that in the rate array.

c If energy bounds are specified, all the counts in the array elements
c lying within those bounds are summed.  If energy bounds are not specified,
c all array elements in the energy-direction are summed.  Only
c one TIME element is read at a time.

c If other dimensions besides energy and time are present, they are
c ignored and only the first elements in those directions are included.

c If the reader encounters any undefined values, the current bin is set
c to the gap value.

c This routine does not apply any deadtime or vignetting corrections.

c  I  lui    (i) = lu of input FITS file
c  I  dtp    (d) = bin integration time, or 
c  I  irec   (i) = current "record number", used as a flag
c  I  iopt   (i) = array of file options
c  I  ivect  (i) = column number for rate (2) and error (3)
c  I  frow   (i) = current row in the fits file
c  I  trow   (i) = current time slice in the current packet
c  I  crunit (i) = 1 for rate, 0 for counts
c  I  naxis  (i) = Number of dimensions in time column array
c  I  naxes  (i) = Length of array in each dimension
c  I  eaxis  (i) = Index of energy axis
c  I  taxis  (i) = Index of time axis
c  O  y      (i) = count rate (c/s; units converted from those in the
c                     file, if necessary)
c  O  sy     (i) = error on count rate
c  O  anynul (l) = .true. if any y-value was undefined in the file
c  O  ierr   (i) = fitsio error

c Author: Eric Lufkin, HEASARC/GSFC, November 1993
c Revised                            August 1994

      integer vmax, maxdim
      parameter (vmax=1024, maxdim=9)
      logical anynul
      character(80) errm
      integer lui,iopt(*),ivect(*),irec,naxis,ierr
     &  ,naxes(*),nelem,taxis,eaxis,crunit,frow,trow
     &  ,blc(maxdim),trc(maxdim),incs(maxdim),i
      real y,nulve,yv(vmax),sy,syv(vmax)
      double precision dtp
      data incs /maxdim*1/
      data nulve /-1.2e34/

      save

      if(ierr.ne.0) return

c Initialize all matrix indices to 1.

      do i = 1, naxis
         blc(i) = 1
         trc(i) = 1
      enddo
         
c Length of yv vector is vmax - hence vmax is the maximum allowable number
c of elements to read.
c Y in the file can be a scalar, vector, or 2-or-higher-D array.  It is stored
c temporarily in yv as a 1-D vector (time held constant) of length > or = 1.

c Load up the blc (Bottom Left Corner) and trc (Top Right Corner) vectors.

      if(eaxis.gt.0) then
         blc(eaxis) = max(1,iopt(1))
         if(iopt(4).gt.0) then
            nelem = min(iopt(4)       - blc(eaxis) + 1, vmax)
         else
            nelem = min(naxes(eaxis) - blc(eaxis) + 1, vmax)
         endif
         trc(eaxis) = blc(eaxis) + nelem - 1
      else
         nelem = 1
      endif

c A single value for the time.

      if(taxis.gt.0) then
         blc(taxis) = trow
         trc(taxis) = trow
      endif
 
c The row number gets passed down as an naxis+1-th array element.

      blc(naxis + 1) = frow
      trc(naxis + 1) = frow

c FITSIO N-dimensional array reader.

      anynul = .false.
      CALL ftgsve(lui,ivect(2),naxis,naxes,blc,trc
     &           ,incs,nulve,yv,anynul,ierr)

c If any values were undefined in the file, the current point will 
c be taken as a gap.

      if(anynul) return

c Read error values, if present.

      if(ivect(3).gt.0) CALL ftgsve(lui,ivect(3),naxis,naxes,blc,trc
     &                             ,incs,nulve,syv,anynul,ierr)
      if(ierr.ne.0) goto 999
      if(anynul) return

c Calculation of Y according to tunit (units of the column),

      if(crunit.eq.0) then
c        Y is in counts.  Sum counts over all energy bins.
         y = yv(1)
         do i = 2, nelem
            y = y + yv(i)
         enddo
         y = y/dtp
      else
c        Y is in counts/sec.  Average counts/s over all energy bins.
         y = yv(1)
         do i = 2, nelem
            y = y + yv(i)
         enddo
         y = y/nelem
      endif

c Count rate error.

      if(ivect(3).gt.0) then
         sy = syv(1)*syv(1)
         do i = 2, nelem
            sy = sy + syv(i)*syv(i)
         enddo
         sy = sqrt(sy)
         if(crunit.eq.0) sy = sy/dtp
      else
c        If no error column, assign Poisson errors.
         sy = sqrt(abs(y/dtp))
      endif
         
      return

999   continue

      write(errm,*) 'xrftgyco: fitsio error = ',ierr
      CALL xaerror(errm,1)

      return
      end
