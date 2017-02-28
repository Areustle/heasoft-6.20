C This subroutine removes the background counts from the raw data.
C Two models are possible:
C   0: No background subtraction 
C   1: Linear background subtraction
C   2: Sine background subtraction
C
C IPOS is the position of the data point in a 64 pt spin
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C   Vers. 1.0  13 Feb 1992  SVELA routine (LAW)
C         2.0  14 Dec 1994  FTOOL routine (JSA)

       subroutine background(cnts, backgnd, variance, erradd, option,
     +            ipos, spinper)

       integer option, ipos

       real cnts, backgnd(3), variance(3), spinper
       real t, tback, erradd
       
       if (option .eq. 1) then
          cnts = cnts - backgnd(3)
          erradd = variance(3)
       else if (option .eq. 2) then
          t = 6.28318531 * REAL(ipos) / spinper
          tback = (backgnd(1)) * (COS(t)) + (backgnd(2) * SIN(t))
     +           + backgnd(3)
          cnts = cnts - tback
          erradd = (variance(1)*(COS(t)**2)) + (variance(2)*(SIN(t)**2))
     +            + variance(3)
       endif

       return

       end
