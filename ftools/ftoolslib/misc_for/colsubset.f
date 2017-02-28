
C******************************************************************************
C SUBROUTINE:
C       colsubset
C
C DESCRIPTION:
C       Parse the requested subset of a column from an inupt column name
C
C AUTHOR:
C       Emily A. Greene
C       Hughes STX
C       27 July, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C       Currently, only vector columns supported
C
C USAGE:
C       call colsubset (iunit, column, sensecase, tform, colno, vecelem,
C    &                  vrange, vstart, vstop, status)
C
C ARGUMENTS:
C       status  - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C
C******************************************************************************
      subroutine colsubset (iunit, column, sensecase, tform, colno,
     &     vecelem, vrange, vstart, vstop, status)

      character*(*) column
      integer iunit, colno, vrange, status, vecelem
      logical sensecase

      integer maxranges
      parameter (maxranges = 15)

      integer maxcl
      parameter (maxcl = 512)

      integer maxvec
      parameter (maxvec = 1024)

      integer vstart(maxranges), vstop(maxranges), pos1, pos2, width
      integer dtype
      character(80) context, tform(maxcl), colname
      logical good

      pos1 = 0
      pos2 = 0

C check for any range specification
      pos1 = index(column, '[')
      if (pos1 .le. 0) then
         colname = column
      else

C subset information included
         colname = column (1:pos1-1)
         pos2 = index(column, ']')
         if (pos2 .le. 0) then
            context = ' Error parsing column subset' // column
            call fcerr (context)
            status = 1
            return
         endif
      endif

C check if main column exists
      call ftgcno (iunit, sensecase, colname , colno, status)
      if (status .ne. 0) then
         context = ' Requested column does not exist: ' // colname
         call fcerr (context)
         return
      endif

C find the number of elements in the vector
      call ftbnfm (tform(colno), dtype, vecelem, width, status)
      if (vecelem .gt. maxvec) then
         context = ' Requested column has too many elements' //
     &        tform(colno)
         call fcerr (context)
         status = 1
         return
      endif

C initialize values
      vrange = 1
      vstart(1) = 1
      vstop(1) = vecelem

C check for allowable characters
      if (pos1 .gt. 0) then
         call fccmpr (column(pos1+1:pos2-1), good)
         if (column(pos1+1:pos2-1) .eq. '-') then
            continue
         else if (good) then
            call fcgrgs (column(pos1+1:pos2-1), vecelem, vrange,
     &           vstart, vstop)
         else
            context = ' Illegal value in subset' // column
            call fcerr (context)
            status = 1
         endif
      endif

 999  return
      end
