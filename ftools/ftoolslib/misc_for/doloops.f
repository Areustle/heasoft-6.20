

C******************************************************************************
C subroutine:
C      doloops
C
C FILE:
C      doloops.f
C
C DESCRIPTION:
C       This routine takes the place of ndims do loops appropriately
C       It is used because the number of dimensions in the column array,
C       and thus the number of do loops necessary to go through all
C       the data appropriately is unknown.  When all of the needed
C       iterations are made, the routine returns done=true
C
C       the variable done must be initialized to .false. and elements
C       must be initialized to 0 before calling the routine for the
C       first time
C
C AUTHOR:
C       Emily A. Greene
C       Hughes STX
C       February 16, 1994
C
C MODIFICATION HISTORY:
C       12/19/94 JKB 1.1 save rngpointer
C
C NOTES:
C
C CALLING SEQUENCE:
C       call doloops (ndims, ranges, startrange, stoprange, incrange,
C                     elements, done, status)
C
C ARGUMENTS:
C       ndims      - number of dimensions
C       ranges     - the number of ranges for each dimension
C       startrange - the start element for each range and each dimension
C       stoprange  - the stop element for each range and each dimension
C       incrange   - the increment for each range and each dimension
C       elements   - vector containing the current element numbers
C       done       - whether we've gone through all the ranges and dimensions
C       status     - status of operation
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C******************************************************************************

      subroutine doloops (ndims, ranges, startrange, stoprange,
     &     incrange, elements, done, status)

      integer maxranges, maxdims
      parameter (maxranges = 15)
      parameter (maxdims = 10)

      integer ndims, ranges(ndims), startrange(maxranges, ndims)
      integer stoprange(maxranges, ndims), incrange(maxranges, ndims)
      integer elements(ndims), status
      logical done

      integer rngpointer(10), curdim, i
      save rngpointer
      character(3) version

      version = '1.1'

C if this is the first time through
      if (elements(ndims) .eq. 0) then
         do 10 i = 1, ndims
            rngpointer(i) = 1
            elements(i) = startrange(rngpointer(i), i)
 10      continue
         return
      endif

C if this is not the first time through, figure out the next element
      curdim = 1
C add one to the current dimension element
 100  elements(curdim) = elements(curdim) + 1

C is it now out of it's current range?
      if (elements(curdim) .gt.
     &     stoprange(rngpointer(curdim), curdim)) then

C check if there is another range for this dimension
         if (rngpointer(curdim) .lt. ranges(curdim)) then
            rngpointer(curdim) = rngpointer(curdim) + 1
            elements(curdim) =
     &           startrange(rngpointer(curdim), curdim)
         else
C if not, reset this dimension's range
            rngpointer(curdim) = 1
            elements(curdim) =
     &           startrange(rngpointer(curdim), curdim)
C and go on to the next dimension
            curdim = curdim + 1
            if (curdim .gt. ndims) then
               done = .true.
               return
            endif
            goto 100
         endif
      endif

 999  return
      end
