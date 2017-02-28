C  This subroutine is based on the subroutine AROUND in the Vela 5B GETBOX 
C program by Jim Lochner.  It has been adapted for use with the FVELALC FTOOL.
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Vers. 1.0  13 Feb 1992  Adapted from NUOPT by Jim Lochner 
C        2.0  22 Nov 1994  Adapted from AROUND for FVELALC
C        2.1   6 Jan 1995  Added sorting of box numbers

       subroutine finddata(long, lat, radius, numofboxes, boxlist)

       integer inc, i, j, k, icnt, numofboxes
       integer boxlist(144), boxnumber

       real long, lat, radius, delta
       real longmin, longmax, latmin, latmax, curlong, curlat

       delta = 1.0
       inc = 144
       if (radius .eq. 0.0) inc = 2
       if (radius .le. 2.0) delta = radius
       longmin = long - radius
       longmax = long + radius
       latmin = lat - radius
       latmax = lat + radius
       curlat = latmin
       icnt = 1
       do 500 j = 1, inc
          curlong = longmax
          do 300 i = 1, inc
             call getboxnum(curlong, curlat, boxnumber)
             boxlist(icnt) = boxnumber
             do 100 k = 1, (icnt-1)
                if (boxlist(icnt) .eq. boxlist(k)) goto 200
 100         continue
             icnt = icnt + 1
 200         curlong = curlong - delta
             if (curlong .lt. longmin) go to 400
 300      continue
 400      curlat = curlat + delta
          if (curlat .gt. latmax) go to 600
 500   continue
 600   numofboxes = icnt - 1
       call sortbox(boxlist, numofboxes)

       return

       end

C--------------------------------------------------------------------------
C  This subroutine converts a galactic L and B coordinate into its matching 
C box number.
C
C  Author:  Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C  History:
C    Vers. 1.0  12 Feb 1992  Adapted from NUOPT for GETBOX (Jim Lochner)
C          2.0  22 Nov 1994  Adapted for FTOOLS 

       subroutine getboxnum(long, lat, boxnumber)

       integer j, boxnumber, hcolat

       real long, lat, curlong, curlat, suplong


       curlong = long
       curlat = lat
       curlong = mod(curlong,360.0)
       if (curlong .lt. 0.0) curlong = curlong + 360.0

C Calculate the supplement of the longitude and normalize to 0.0 - 360.0

       suplong = 180.0 - curlong
       if (suplong .lt. 0.0) suplong = suplong + 360.0
       if (suplong .gt. 360.0) suplong = suplong - 360.0

C Calculate one half of the colatitude
 
       hcolat = INT(0.5 * (lat + 90.0))

C Sort counts into boxes with a  maximum area of 4 square degrees.

       if (lat .lt . (-88.0)) go to 226
       if (lat .lt . (-86.0)) go to 228
       if (lat .lt . (-78.0)) go to 230
       if (lat .lt . (-66.0)) go to 240
       if (lat .lt . (-48.0)) go to 250
       if (lat .lt . (48.0)) go to 260
       if (lat .lt . (66.0)) go to 270
       if (lat .lt . (78.0)) go to 280
       if (lat .lt . (86.0)) go to 285
       if (lat .lt . (88.0)) go to 290
       go to 295

C       BOXES 9-12, 90X2 DEGREES.
  226  j = INT(suplong / 90.0) + 9
       go to 300

C       BOXES 13-24, 30X2 DEGREES.
  228  j = INT(suplong / 30.0) + 1 + (12 * hcolat)
       go to 300

C       BOXES 25-168, 10X2 DEGREES.
  230  j = INT(suplong / 10.0) - 47 + (36 * hcolat)
       go to 300

C       BOXES 169-600, 5X2 DEGREES.
  240  j = INT(suplong / 5.0) - 263 + (72 * hcolat)
       go to 300

C       BOXES 601-1680, 3X2 DEGREES.
  250  j = INT(suplong / 3.0) - 839 + (120 * hcolat)
       go to 300

C       BOXES 1681-10320, 2X2 DEGREES.
  260  j = INT(suplong / 2.0) - 2099 + (180 * hcolat)
       go to 300

C       BOXES 10321-11400, 3X2 DEGREES.
  270  j = INT(suplong / 3.0) + 2041 + (120 * hcolat)
       go to 300

C       BOXES 11401-11832, 5X2 DEGREES.
  280  j = INT(suplong / 5.0) + 5785 + (72 * hcolat)
       go to 300

C       BOXES 11833-11976, 10X2 DEGREES.
  285  j = INT(suplong / 10.0) + 8809 + (36 * hcolat)
       go to 300

C       BOXES 11977-11988, 30X2 DEGREES.
  290  j = INT(suplong / 30.0) + 10921 + (12 * hcolat)
       go to 300

C       BOXES 11989-11992, 90X2 DEGREES.
  295  j = INT(suplong / 90.0) + 11989

300    boxnumber = j

       return

       end
