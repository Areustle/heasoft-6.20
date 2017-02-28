C******************************************************************************
C SUBROUTINE:
C      fgrfdr
C
C DESCRIPTION:
C	Returns the path to the refdata directory
C
C AUTHOR/DATE:
C       Ron Zellar  2/17/94
C
C MODIFICATION HISTORY:
C       Keith Arnaud  5/16/97  Uses LHEA_DATA environment variable
C
C       Jeff Guerber 5/23/1997. Add trailing '/' to match what fmpfdr() (which
C           this used to call) returned, unless on VMS (or if LHEA_DATA had
C           one already).  Also, the "no room in rfdir" status & message were
C           set but there was no fcerr call.  Untabified.
C
C USAGE:
C      call fgrfdr(rfdir,status)
C
C ARGUMENTS:
C      rfdir - the path to the refdata directory
C      status - success status for this routine
C
C PRIMARY LOCAL VARIABLES:
C	pfvar - the translated LHEA_DATA environment variable (logical)
C	pflen - the length of pfvar
C
C******************************************************************************
      subroutine fgrfdr(rfdir,status)

      implicit none
      character*(*) rfdir
      character(160) pfvar
      integer status,pflen
      character(80) context

C     initialize the variables
      pflen = 0
      pfvar = ' '
      rfdir = ' '

C     translate the LHEA_DATA environment variable (logical)
      call ctrlog('LHEA_DATA',9,pfvar,pflen)

C     Check to see that something useful was returned
      if (pflen .eq. 0) then
          status = 1
          context = 'fgrfdr: Unable to translate LHEA_DATA'
          call fcerr(context)
          return
      endif

C     If the last character is ']' (ie, VMS), or is already a '/', return
C     pfvar as is; otherwise, concatenate a slash.

      if ( (pfvar(pflen:pflen) .eq. ']') .or.
     &     (pfvar(pflen:pflen) .eq. '/') ) then

          if (pflen .gt. len(rfdir)) then
              status = 3
              context = 'fgrfdr: Not enough room in rfdir'
              call fcerr(context)
              return
          endif

          rfdir = pfvar(:pflen)

      else

          if (pflen+1 .gt. len(rfdir)) then
              status = 3
              context = 'fgrfdr: Not enough room in rfdir'
              call fcerr(context)
              return
          endif

          rfdir = pfvar(:pflen) // '/'

      endif

      return
      end
