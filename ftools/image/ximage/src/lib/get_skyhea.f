      subroutine get_skyhea (Mapid,Ra,Dec,Rnorth,Roll,Pixsize,
     &                       imgequ, Ctype,status)

      implicit none
c
c Read internal header and convert ra dec roll north pixsize to degrees
c
c  DEPRECATED - Only kept around to enable old grid command
c
c  I  MapID   (C)  Internal XIMAGE mapid 
c  O  Ra      (d)  ra center in degrees
c  O  Dec     (d)  dec center in degrees
c  O  Rnorth  (d)  north image in degrees
c  O  Roll    (d)  roll image in degrees
c  O  Pixsize (d)  pixel size in degrees
c  O  imgequ  (i)  equinox as interger
c  O  Ctype   (c)  type of projection
c  O  Status  (i)  Error flag (0=OK)
c
      CHARACTER*(*) Mapid
      REAL*8 Ra , Dec , Rnorth , Roll, Pixsize(2)
      character(80) ctype 
      INTEGER*4 imgequ, Status
c
c local variable 
      INTEGER*4 szx, szy
      REAL*8 equimap, crot, crval1, crval2, crpix1, crpix2
      REAL*4 xref, yref, dr1, dr2
      LOGICAL isloaded, ISDNULL

      if ( .not. isloaded(Mapid) ) then
         call XWRITE (' Image not loaded', 10)
         status=1 
         RETURN
      endif
c
c RA/Dec returned is expected to be center
c
      call gheadi(Mapid, 'SZX', szx, 0, status)
      call gheadi(Mapid, 'SZY', szy, 0, status)
      call gheadd(Mapid, 'CRVAL1', crval1, 0, status)
      call gheadd(Mapid, 'CRVAL2', crval2, 0, status)
      call gheadd(Mapid, 'CRPIX1', crpix1, 0, status)
      call gheadd(Mapid, 'CRPIX2', crpix2, 0, status)
      call gheadd(Mapid, 'CDELT1', Pixsize(1), 0, status)
      call gheadd(Mapid, 'CDELT2', Pixsize(2), 0, status)
      xref = (float(szx)/2.0 + 0.5) - crpix1
      yref = (float(szy)/2.0 + 0.5) - crpix2
      dr1 = 0.
      dr2 = 0.
      call gheadd(Mapid, 'XIMNORTH',  Rnorth, 0, status)
      call xytrd(xref, yref, Pixsize, ra, dec, crval1, crval2, Rnorth,
     &           dr1, dr2)


      call gheadd(Mapid, 'CROTA2', crot, 0, status)
      if ( ISDNULL(crot) ) crot = 0.
      Roll = crot - 90.
      call gheads(Mapid, 'CTYPE1', ctype, 0, status)
      call gheadd(Mapid, 'EQUINOX', equimap, 0, status)
      if ( ISDNULL(equimap) ) then
         imgequ = 2000
      else 
         imgequ = equimap
      endif
      RETURN
      END
