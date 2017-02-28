C This subroutine is based on the FINDDATA subroutine from the FVELALC FTOOL
C by JSA.  
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Vers. 1.0  25 Jan 1995  FVELMAP FTOOL version
C        1.1  31 Oct 1995  Dropped address array, alter corners if necessary

      subroutine findmaps(long_cnr1, lat_cnr1, long_cnr2, lat_cnr2,
     +           numofboxes, boxlist, long_cntr, lat_cntr, iwp, jwp)

      integer maxnumboxes, icnt, numofboxes, boxlist(400), boxnumber
      integer iwp, jwp, iwpmx, jwpmx

      real long_cnr1, lat_cnr1, long_cnr2, lat_cnr2
      real lat, long, l_cntr, b_cntr, long_cntr(400), lat_cntr(400)

      character(80) message

      maxnumboxes = 400

C Sort the order of the corners so (long_cnr1, lat_cnr1) is the bottom left
C corner 

      if (long_cnr2 .lt. long_cnr1) then
         long = long_cnr1
         long_cnr1 = long_cnr2
         long_cnr2 = long
      endif
      if (lat_cnr2 .lt. lat_cnr1) then
         lat = lat_cnr1
         lat_cnr1 = lat_cnr2
         lat_cnr2 = lat
      endif

      iwpmx = 1
      jwpmx = 1
      icnt = 0
      lat = lat_cnr1 - 2.0
      jwp = 0

C Start loop which increments latitude by 2.0 degrees

 10   lat = lat + 2.0
      if (lat .gt. lat_cnr2) go to 100
      jwp = jwp + 1
      if (jwpmx .lt. jwp) jwpmx = jwp
      long = long_cnr2 + 2.0
      iwp = 0

C Start loop which increments longitude by -2.0 degrees

 20      long = long - 2.0
         if (long .lt. long_cnr1) go to 10
         iwp = iwp + 1
         if (iwpmx .lt. iwp) iwpmx = iwp
         call boxnumbers(long, lat, boxnumber, l_cntr, b_cntr)
         icnt = icnt + 1
         if (icnt .gt. maxnumboxes) then
            message = 'Maximum number of boxes exceeded'
            call fcerr(message)
            jwp = jwpmx - 1
            iwp = iwpmx - 1
            icnt = iwp * jwp
            go to 100
         endif

C Record the box number and its center coordinates

         boxlist(icnt) = boxnumber
         long_cntr(icnt) = l_cntr
         lat_cntr(icnt) = b_cntr
         go to 20

C All box numbers found 

 100  numofboxes = icnt
      
      return

      end


C--------------------------------------------------------------------------
C  This subroutine converts a galactic L and B coordinate into its matching 
C box number.
C
C  Author:  Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C  History:
C    Vers. 1.0  12 Feb 1992  Adapted from NUOPT for GETBOX (Jim Lochner)
C          2.0  22 Nov 1994  FVELALC routine getboxnumber
C          3.0  26 Jan 1995  FVELMAP routine boxnumbers (Galactic plane only)

      subroutine boxnumbers(longpos, latpos, boxnumber, longcntr, 
     +           latcntr)

      integer boxnumber, j

      real longpos, latpos, longcntr, latcntr
      real curlong, suplong, hcolat

      character(80) message

      j = 0

      curlong = longpos
      curlong = mod(curlong,360.0)
      if (curlong .lt. 0.0) curlong = curlong + 360.0

C Calculate the supplement of the longitude and normalize to 0.0 - 360.0

      suplong = 180.0 - curlong
      if (suplong .lt. 0.0) suplong = suplong + 360.0
      if (suplong .gt. 360.0) suplong = suplong - 360.0

C Calculate one half of the colatitude
 
      hcolat = INT(0.5 * (latpos + 90.0))

C Convert sky positions into boxes with a maximum area of 4 square degrees.
C Accept only boxes within +/- 48 degrees galactic latitude.

      if (latpos .lt . (-88.0)) go to 200
      if (latpos .lt . (-86.0)) go to 200
      if (latpos .lt . (-78.0)) go to 200
      if (latpos .lt . (-66.0)) go to 200
      if (latpos .lt . (-48.0)) go to 200
      if (latpos .lt . (48.0)) go to 100
      if (latpos .lt . (66.0)) go to 200
      if (latpos .lt . (78.0)) go to 200
      if (latpos .lt . (86.0)) go to 200
      if (latpos .lt . (88.0)) go to 200
      go to 200

C     BOXES 1681-10320, 2X2 DEGREES.

 100  j = INT(suplong / 2.0) - 2099 + (180 * hcolat)
      latcntr = ((REAL(hcolat) + 0.5) * 2.0) - 90.0
      longcntr = (INT(suplong/2.0) + 0.5) * 2.0
      longcntr = -1.0 * (longcntr - 180.0)
      if (longcntr .lt. 0.0) longcntr = longcntr + 360.0
      if (longcntr .gt. 360.0) longcntr = longcntr - 360.0
      go to 300

C All other boxes are unacceptable

 200  message = 'Latitude outside galactic plane (< -48 or > 48)'
      call fcerr(message)

 300  boxnumber = j

      return

      end
