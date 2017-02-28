
C******************************************************************************
C SUBROUTINE:
C      fdmpsl
C
C DESCRIPTION:
C       Check for scaled B, I, and J columns
C
C AUTHOR:
C      Emily A. Greene
C       Hughes STX
C       29 July, 1993
C
C MODIFICATION HISTORY:
C       8/24/93 EAG:  Changed to increase speed
C      03/24/1999 toliver:  disp and lform now set if either tscal or
C                           tzero set
C
C NOTES:
C
C USAGE:
C    call fdmpsl (iunit, colpos, tform, disp, lform, ftstatus)
C
C ARGUMENTS:
C   input:
C       iunit    - input unit number
C       colpos   - column number
C       tform    - the format of this column
C   output:
C       disp     - output TDISP value, if needed
C       lform    - the corresponding width
C       status   - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context   - error message
C
C CALLED ROUTINES:
C       FTxxxx   - FITSIO routines
C       fcstln   - function returning index of last non-blank character
C
C******************************************************************************
      subroutine fdmpsl (iunit, colpos, tform, disp, lform, status)

      integer iunit, colpos
      character*(*) disp, tform
      integer lform, status

      double precision tscal, tzero
      integer imax, jmax
      character(10) keyroot, keyword
      character(80) context

C Special case TZERO values for I and J columns (though note that
C in the J case we're actually testing for 2147483648 = jmax+1):
      imax = 32768
      jmax = 2147483647

C check for B, I or J column
      if ((index(tform, 'B').ne.0).or.(index(tform, 'I').ne.0)
     &     .or. (index(tform, 'J') .ne. 0)) then

C check for scaling
         tscal = 1.0D0
         tzero = 0.0D0
         keyroot = 'TSCAL'
         call ftkeyn (keyroot, colpos, keyword, status)
         call ftgkyd (iunit, keyword, tscal, context, status)
         keyroot = 'TZERO'
         call ftkeyn (keyroot, colpos, keyword, status)
         call ftgkyd (iunit, keyword, tzero, context, status)
         if ((status .ne. 202) .AND. (status .ne. 0)) then
C FITSIO error getting scaling
            context = ' Error determining scaling'
            call fcerr (context)
         else
C scaling present, but only act if it does anything
            if ((tscal.ne.1.D0).or.(tzero.ne.0.D0)) then
C Handle special case TZERO values for I and J columns, i.e.
C do not change display format to 1PE15.7 if column is I or J
C type with tscal=1 and tzero = imax or jmax:
               if (.not.((index(tform, 'I').ne.0).and.
     &              (tscal.eq.1.D0).and.(int(tzero).eq.imax)).and.
     &             .not.((index(tform, 'J').ne.0).and.
     &              (tscal.eq.1.D0).and.((int(tzero-1.D0)).eq.jmax)))
     &         then
C set display to be E15.7 (float)
                  disp = '1PE15.7'
                  lform = 15
               endif
            endif
            status = 0
         endif
      endif

      return
      end
