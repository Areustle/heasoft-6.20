c
c This include file is for the window setup of fplot.
c     
      character(80) fpltwin
      character(6)  fpltbkg
      integer flagfv
C
C     flagfv = 1: use fv
C     flagfv = 0: use fdump
C
      common /xsel_fplt/ fpltwin,fpltbkg
      common /xsel_fv/ flagfv
