C Ftools info: $Header: /headas/headas/ftools/ftoolslib/gen/gtilib.f,v 1.10 2013/07/27 19:45:00 irby Exp $
C
C General routines for manipulating Good Time Interval (GTI) lists.
C Contents:
C     GTIMERGE: Merge two Good Time Interval lists.
C     GTICLEAN: Make sure a GTI list is ordered, and has no overlaps or
C         zero-length intervals
C     GTIEXP: Calculate what part of a given time interval is within the GTIs
C
C Jeff Guerber, HSTX, April 1997.
C
C $Log: gtilib.f,v $
C Revision 1.10  2013/07/27 19:45:00  irby
C Change all udmget arrays MEMB/S/I/L/R/D/X of size (1) to size (100)
C to get past new aggressive optimization in gfortran 4.8.x.  The new
C compiler decides that e.g. MEMI(1) can never have more than one element
C so when doing loop unrolling optimization appears to (incorrectly)
C remove the loop over its elements.
C
C Revision 1.9  2013/05/21 19:08:17  irby
C Change character*n to character(n) to silence warnings: "Obsolescent
C feature: Old-style character length".
C
C Revision 1.8  2011/07/11 18:27:39  irby
C Fix gfortran 4.6.x (pedantic) compilation error (rank mismatch):
C calls to gtimerge() from gticlean() require 9th & 10th arguments
C to be arrays, not scalars.
C
C Revision 1.7  2009/05/13 18:36:37  irby
C Initialize udmget pointers.  If these variables are not zero, udmget
C tries to reallocate the block at that address.  Usually it fails silently
C and then goes on to allocate some fresh memory, but sometimes the random
C memory address matches the address of some already allocated memory
C causing either incorrect answers or a seg fault.
C
C Revision 1.6  2001/12/28 16:28:11  irby
C Converted rcsid definition to type definition with separate data
C statement (standard fortran).  This prevents compilation errors
C under f90/95.
C
C Revision 1.5  1998/02/19 03:12:17  guerber
C Added gticlean and gtiexp, and made a few improvements to gtimerge.
C
C Revision 1.4  1997/10/21 00:06:23  guerber
C Moved to ftools/library/utilities/gen/, to be part of libftools.a.
C
C Revision 1.3  1997/05/05 19:12:10  miket
C switched lines 169/170 !
C
C Revision 1.2  1997/05/03 07:38:07  guerber
C New file: general GTI routines. Currently just gtimerge -- AND or OR lists.
C
C Revision 1.1  1997/04/04 10:09:13  guerber
C *** empty log message ***
C
C=========================================================================
C
C     GTIMERGE: Merge two Good Time Interval lists.
C
C     Take two lists of Good Time Intervals (GTIs), and depending on the
C     value of MODE, either AND the lists (a given time will be within an
C     output GTI only if it was within GTIs from *both* input lists), or
C     OR them (GTIs from *either* input list).  The lists need not be
C     sorted or cleaned, but the output will be.
C
      subroutine gtimerge (mode, gtistart, gtistop, ngti, mxgti,
     &    astart, astop, anum, bstart, bstop, bnum, status)

      implicit none
C
C     ARGUMENTS
C
      character*(*) mode
C     = 'AND' or 'OR'.  Currently must be uppercase.
      double precision gtistart(*), gtistop(*)
C     = output start, stop time lists. May be one of the input lists.
      integer ngti, mxgti
C     = output number of entries; physical size of gtistart and gtistop
      double precision astart(*), astop(*), bstart(*), bstop(*)
C     = input start, stop time lists
      integer anum, bnum
C     = number of elements in respective input lists.
      integer status
C
C     LOCAL VARIABLES
C
      integer i, j, ilist
      integer lmxgti
C
C     The following definitions are for the udmget dynamic memory manager.
C     The "pointer" it returns is an offset (in units of the size of the
C     entity, eg. 8 bytes for a complex real) from the MEM common block, so
C     that MEMR(ptr) is the start of the allocated array.  Thus it *relies*
C     on Fortran's ability to reference an array beyond its bounds!
C     Ingenious and frightening...
C
      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD
C
C     The udmget pointers we use:
C
      integer timesp, typesp, idxp
C
      character(70) rcsid
      data rcsid
     &/'$Id: gtilib.f,v 1.10 2013/07/27 19:45:00 irby Exp $'/
C---------------------------------------------------------------------
C
C     Build a combined list
C     times(i) holds the time
C     types(i) holds the type:  0 for GTI list A start, 1 for list A stop,
C         2 for list B start, and 3 for list B stop.
C     In the process, also filter out invalid GTIs of negative or zero length.
C
C     timesp, typesp, idxp are the udmget "pointers".
C     Udmget type codes: 1 is boolean, 2 is character, 3 is short integer,
C     4 is integer, 5 is long integer (udmget.c says 8 bytes),
C     6 is single precision real, 7 is double precision real, 8 is complex
C
      timesp = 0
      typesp = 0
      idxp = 0
      call udmget( 2*(anum+bnum), 7, timesp, status )
      call udmget( 2*(anum+bnum), 3, typesp, status )
      call udmget( 2*(anum+bnum), 4, idxp, status )
      if (status .ne. 0) return
C
      ilist = 0
      do i = 1, anum
          if (astart(i) .lt. astop(i)) then
              ilist = ilist + 1
              memd(timesp-1+ilist) = astart(i)
              mems(typesp-1+ilist) = 0
              ilist = ilist + 1
              memd(timesp-1+ilist)= astop(i)
              mems(typesp-1+ilist) = 1
          endif
      enddo
C
      do i = 1, bnum
          if (bstart(i) .lt. bstop(i)) then
              ilist = ilist + 1
              memd(timesp-1+ilist) = bstart(i)
              mems(typesp-1+ilist) = 2
              ilist = ilist + 1
              memd(timesp-1+ilist)= bstop(i)
              mems(typesp-1+ilist) = 3
          endif
      enddo
C
      if (ilist .eq. 0) then
          ngti = 0
          return
      endif
C
C     lmxgti is in case mxgti and ngti are actually the same variable
C     (eg, with dynamically-allocated arrays)
C
      lmxgti = mxgti
      call gtimrg1( gtistart, gtistop, ngti, lmxgti, memd(timesp),
     &    mems(typesp), memi(idxp), ilist, mode, status )
C
C     The FSM can leave zero-length GTIs, so clean them up...
C
      i = 1
      do while ( i .le. ngti )
          if ( gtistart(i) .ge. gtistop(i) ) then
              do j = i+1, ngti
                  gtistart(j-1) = gtistart(j)
                  gtistop(j-1) = gtistop(j)
              enddo
              ngti = ngti - 1
          else
              i = i + 1
          endif
      enddo
c
c     ...and combine any contiguous ones.
c
      i = 2
      do while (i .le. ngti)
          if ( gtistop(i-1) .eq. gtistart(i) ) then
              gtistop(i-1) = gtistop(i)
              do j = i, ngti-1
                  gtistart(j) = gtistart(j+1)
                  gtistop(j) = gtistop(j+1)
              enddo
              ngti = ngti - 1
          else
              i = i + 1
          endif
      enddo
C
      call udmfre(timesp, 7, status)
      call udmfre(typesp, 3, status)
      call udmfre(idxp, 4, status)
C
      return
      end
C
C==========================================================================
C     Internal routine to generate the output GTI.  Only reason to make
C     this a separate routine is that it's easier to use the udmget-
C     allocated arrays this way.
C
      subroutine gtimrg1( gtistart, gtistop, ngti, mxgti, times,
     &    types, idx, ilist, mode, status )
C
C     Arguments:
C
      implicit none
      integer ngti, mxgti
      double precision gtistart(mxgti), gtistop(mxgti)
      double precision times(*)
      integer*2  types(*)
      integer*4  idx(*), ilist, status
      character*(*) mode
C
C     Local variables:
C
      logical mode_or, mode_and
      integer a, b, i
      character(80) msg
C-------------------------------------------------------------------------
C
      ngti = 0
C
C     Just make it easier in the FSM...
C
      mode_or = ( index(mode,'OR') .eq. 1 )
      mode_and = ( index(mode,'AND') .eq. 1 )
      if (mode_or .eqv. mode_and) then
          msg = 'gtimerge: ERROR, bad mode string: '//mode
          call fcerr(msg)
          status = 20
          return
      endif
C
C     Indexed sort on the times.  Fcidxd is in /ftools/library/utilities/gen/.
C
      call fcidxd( ilist, times, idx )

C     Now produce a merged gti in gtistart/gtistop.
C     This section is a finite-state machine that takes the combined, sorted
C     time list (times) and the type codes and creates an ANDed or ORed GTI.
C     If the two original lists are A and B, A+ means list A is on, A- means
C     it's off, etc, and the numbers are the type codes given above, then the
C     FSM looks like:
C
C       (A+B+)  3->  <-2 (A+B-)
C
C       1| ^             1| ^
C        v |0             v |0
C
C       (A-B+)  3->  <-2 (A-B-) <-start
C
C     For ANDing, a new GTI is begun on the transitions to state A+B+, and
C     a currently open one is ended on the transtitions away from it.
C     For ORing, a new GTI is begun on transitions away from state A-B-,
C     and an open one ended on transtiions to it.
C
C     There's a complication: If one of the input lists has overlapping
C     GTIs, we might do the wrong thing, such as starting an extra GTI or
C     ending one too soon.  The counters A and B keep track of how many
C     levels of GTI we've seen, so we only take action at the right times.
C
C     This code is derived from the FSM in /ftools/xselect/extractor/
C     extractor.f, subroutine fingti(), which was probably written by
C     Bruce O'Neel.  He presorted the arrays instead of using level
C     counters, however.

      a = 0
      b = 0

      do 500 i = 1, ilist
c
c         A turns on.  Start a new GTI if A is not already on, and:
c         AND case: B is on too;  OR case: B is off (else one is already
c         active).
c
          if ( types(idx(i)) .eq. 0 ) then
              a = a + 1
              if (    (mode_and .and. (a .eq. 1) .and. (b .ge. 1))
     &            .or. (mode_or .and. (a .eq. 1) .and. (b .le. 0))) then
                  if (ngti .eq. mxgti) goto 900
                  ngti = ngti + 1
                  gtistart(ngti) = times(idx(i))
              endif
c
c         A turns off.  End the current GTI if: AND: B is on; OR: B is off too.
c
          elseif ( types(idx(i)) .eq. 1 ) then
              a = a - 1
              if (    (mode_and .and. (a .eq. 0) .and. (b .ge. 1))
     &            .or. (mode_or .and. (a .eq. 0) .and. (b .le. 0))) then
                  gtistop(ngti) = times(idx(i))
              endif
c
c         B turns on. Start a new GTI if B isn't already on, and:
c         AND: A is on too;  OR: B is off.
c
          elseif ( types(idx(i)) .eq. 2 ) then
              b = b + 1
              if (    (mode_and .and. (b .eq. 1) .and. (a .ge. 1))
     &            .or. (mode_or .and. (b .eq. 1) .and. (a .le. 0))) then
                  if (ngti .eq. mxgti) goto 900
                  ngti = ngti + 1
                  gtistart(ngti) = times(idx(i))
              endif
c
c         B turns off.  End the current GTI if: AND: A is on; OR: A is off too.
c
          elseif ( types(idx(i)) .eq. 3 ) then
              b = b - 1
              if (    (mode_and .and. (b .eq. 0) .and. (a .ge. 1))
     &            .or. (mode_or .and. (b .eq. 0) .and. (a .le. 0))) then
                  gtistop(ngti) = times(idx(i))
              endif
          endif
  500 continue
      return
C
C     Come here if too many output GTIs for the array
C
  900 call fcerr('gtimerge: WARNING: Too many output GTIs, truncating.')
      status = 10
C
      RETURN
      END
C
C=========================================================================
C
C     gticlean:  Sort a gti list and remove invalid and overlapping
C     intervals by calling gtimerge to OR it with nothing.
C
      subroutine gticlean( gtistart, gtistop, ngti, mxgti,
     &    astart, astop, anum, status )
      implicit none
C
C     ARGUMENTS
C
      double precision gtistart(*), gtistop(*)
C       = output start, stop time lists
      integer ngti, mxgti
C       = output number of entries; input physical size of gtistart, gtistop
      double precision astart(*), astop(*)
C       = input start, stop time lists
      integer anum, status
C       = number of elements in input lists;  output status
      double precision bstart(1), bstop(1)
      integer bnum
C       = placeholder variables
      bstart(1) = 0
      bstop(1) = 0
      bnum = 0
C--------------------------------------------------------------------
      call gtimerge( 'OR', gtistart, gtistop, ngti, mxgti,
     &    astart, astop, anum, bstart, bstop, bnum, status )
      return
      end
C
C====================================================================
C
C     GTIEXP: Exposure: What part of the time interval (t1, t2)
C     is contained within the Good Time Intervals?  (For the fractional
C     exposure, just divide gtiexp by (t2-t1).)
C
C         NOTE: It is *VITAL* that the GTI list be ordered and
C         contain no overlaps!  If in doubt, call gticlean first.
C         Also, we assume that t1, t2, and the gti list are all
C         in the same units.
C
C     Jeff Guerber, RSTX/NASA GSFC, Feb. 1998
C
      double precision function gtiexp( t1, t2, gtistart, gtistop,
     &    ngti, status )

      implicit none
C
C     ARGUMENTS
C
      double precision t1, t2, gtistart(*), gtistop(*)
C         = time interval; gti start and stop times
      integer ngti, status
C         = number of entries in GTI list; returned status
C
C     LOCAL VARS
C
      integer i
      double precision total
      character(80) msg

      character(70) rcsid
      data rcsid
     &/'$Id: gtilib.f,v 1.10 2013/07/27 19:45:00 irby Exp $'/
C---------------------------------------------------------------------
C
      if (t1 .ge. t2) then
          write (msg, *) 'gtiexp: ERROR, t1>=t2: ', t1, t2
          call fcerr(msg)
          gtiexp = 0.0d0
          status = 30
          return
      endif
C
C     If GTI list empty, assume it's all good. (May need to rethink.)
C
      if (ngti .eq. 0) then
          gtiexp = t2 - t1
          return
      endif

      total = 0d0
      do i = 1, ngti
C
C         There are 6 cases:
C         1) g1 g2 t1 t2: cycle to next interval
          if (gtistop(i) .le. t1) then
              continue

C         2) t1 t2 g1 g2: gti list is assumed ordered, so we're done!
          else if (t2 .le. gtistart(i)) then
              goto 20

C         3) g1 t1 g2 t2  (Note: Only 2 comparisons to reach any branch!)
          else
              if (gtistart(i) .le. t1) then
                  if (gtistop(i) .le. t2) then
                      total = total + gtistop(i) - t1

C         4) g1 t1 t2 g2
                  else
                      total = total + t2 - t1
                  endif
C         5) t1 g1 g2 t2
              else
                  if (gtistop(i) .le. t2) then
                      total = total + gtistop(i) - gtistart(i)

C         6) t1 g1 t2 g2
                  else
                      total = total + t2 - gtistart(i)
                  endif
              endif
          endif
      enddo
   20 continue
C
      gtiexp = total
      return
      end
