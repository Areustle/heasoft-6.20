C This subroutine sorts the data into time order.  It is (loosely) based
C  on the subroutine SHELL.
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Vers. 1.0  5 Dec 1994 

       subroutine sortdata(mjdtime, cnts, err, longitude, latitude, 
     +            elements)

       integer elements
       integer i, iex, igap, imax, iplusg

       real cnts(elements), err(elements)
       real longitude(elements), latitude(elements)

       double precision mjdtime(elements)


       igap = elements
 5     if (igap .le. 1) go to 999
       igap = igap / 2
       imax = elements - igap
 10    iex = 0
       do 20 i = 1, imax
          iplusg = i + igap
          if (mjdtime(i) .gt. mjdtime(iplusg)) then
             call swapdouble(mjdtime(i), mjdtime(iplusg))
             call swapreal(cnts(i), cnts(iplusg))
             call swapreal(err(i), err(iplusg))
             call swapreal(longitude(i), longitude(iplusg))
             call swapreal(latitude(i), latitude(iplusg))
             iex = iex + 1
          endif
 20    continue
       if (iex .gt. 0) go to 10
       go to 5

 999   return

       end


C---------------------------------------------------------------------------
C  This subroutine is based on the subroutine SHELLA included in the Vela 5B 
C GETBOX program by Jim Lochner.  It has been adapted for use with FTOOLS and
C unnecessary features and flags were removed.  It sorts the list of box
C numbers into ascending numerical order.
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C   Vers. 1.0  13 Feb 1992  Adapted from NUOPT by Jim Lochner 
C         2.0  22 Nov 1994  Adapted for FTOOLS

       subroutine sortbox(boxlist, numofboxes)

       integer i, iex, numofboxes, igap, imax, iplusg
       integer boxlist(80)

       igap = numofboxes
 5     if (igap .le. 1) go to 999
       igap = igap / 2
       imax = numofboxes - igap
 10    iex = 0
       do 20 i = 1, imax
          iplusg = i + igap
          if (boxlist(i) .gt. boxlist(iplusg)) then
             call swapint(boxlist(i), boxlist(iplusg))
             iex = iex + 1
          endif
 20    continue
       if (iex .gt. 0) go to 10
       go to 5

 999   return

       end

C ---------------------------------------------------------------------------
C This is a set of swapping routines used in various sorting procedures
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Vers. 1.0  5 Dec 1994  

       subroutine swapint(int1, int2)

       integer int1, int2, temp

       temp = int1
       int1 = int2
       int2 = temp

       return

       end


       subroutine swapreal(float1, float2)

       real float1, float2, temp

       temp = float1
       float1 = float2
       float2 = temp

       return

       end


       subroutine swapdouble(dbl1, dbl2)

       double precision dbl1, dbl2, temp

       temp = dbl1
       dbl1 = dbl2
       dbl2 = temp

       return

       end
