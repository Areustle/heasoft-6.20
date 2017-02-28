
C*****************************************************************************
C SUBROUTINE:
C       copyfirst
C
C DESCRIPTION:
C       This routine copies the primary array and any extensions before the
C       specified extension from the input to output file as requested by
C       copyall and copyprime
C
C AUTHOR:
C       Emily A. Greene
C       Hughes STX
C       27 July, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C        call copyfirst (iunit, ounit, extnum, copyprime, copyall, history,
C                        nhists, record, status)
C ARGUMENTS:
C       iunit     - input unit number
C       ounit     - output unit number
C       extnum    - the extension of the input file that will be acted on
C       copyprime - whether to copy the primary array
C       copyall   - whether to copy all other extensions
C       history   - whether to include history records
C       nhists    - number of history records
C       record    - array of nhists history records
C       status    - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C
C*****************************************************************************
      subroutine copyfirst (iunit, ounit, extnum, copyprime, copyall,
     &     history, nhists, record, status)

      integer iunit, ounit, extnum, nhists, status
      logical copyprime, copyall, history
      character*(*) record(nhists)

      logical simple, extend
      integer bitpix, naxis, pcount, gcount, naxes, i, htype
      character(80) context

C make sure the input file is at the correct place
      call ftmahd (iunit, 1, htype, status)

C copy the primary array, if so requested
      if ((copyprime) .or. (copyall)) then
         call ftcopy (iunit, ounit, nhists, status)
      else
         simple = .true.
         bitpix = 16
         naxis = 0
         pcount = 0
         gcount = 1
         extend = .true.
         call ftphpr (ounit, simple, bitpix, naxis, naxes,
     &        pcount, gcount, extend, status)
         call ftpdef (ounit, bitpix, naxis, naxes, pcount,
     &        gcount, status)
      endif
      if (history) then
         do 10 i = 1, nhists
            call ftphis (ounit, record(i), status)
 10      continue
C          call fptime (ounit, status)
         if (status .ne. 0) then
            context = ' Error creating primary array'
            call fcerr (context)
            goto 999
         endif
      endif

C copy other extension, if so requested
      if ((copyall) .and. (extnum .gt. 1)) then
         do 100 i = 1, extnum-1
            call ftmrhd (iunit, 1, htype, status)
            call ftcrhd (ounit, status)
            call ftcopy (iunit, ounit, 0, status)
 100     continue
         if (status .ne. 0) then
            context = ' Error copying extensions'
            call fcerr (context)
            goto 999
         endif
      endif


 999  return
      end
