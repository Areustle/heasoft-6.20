      SUBROUTINE ACTWIN(Ipwin, Ngroup, Mxwin, Iactw)
      INTEGER   Ngroup, Mxwin
      INTEGER   Ipwin(*), Iactw(*)
C---
C Runs throught the Ipwin array and returns 1 if ICWIN is currently
C active, and 0 otherwise
C---
C Ipwin   I    Array telling which window each group is to appear.
C Ngroup  I    Number of plot groups
C Mxwin   I    Number of windows to consider
C Iactw     O  =1 is window contains something to be plotted, =0 otherwise.
C---
C 1992-Apr-14 - New routine [AFT]
C---
      INTEGER   ig, iw
C---
C Scan through all windows in window list
      DO 190 iw=1,Mxwin
C If any groups being plotted in window, set active window flag and jump
C to next window.
         DO ig=1,Ngroup
            IF ( Ipwin(ig).EQ.iw ) THEN
               Iactw(iw) = 1
               GOTO 190
            END IF
         END DO
C No groups, contour plots or images are being plotted in current window,
C so it must be inactive.
         Iactw(iw) = 0
  190 CONTINUE
      RETURN
      END
