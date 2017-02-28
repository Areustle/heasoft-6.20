C $Log: fitsdt.cmn.inc,v $
C Revision 1.1  1998/09/30 17:01:06  peachey
C New tool, delivered by Jeff Silvis
C
c Revision 2.3  1998/02/19  18:40:26  wang
c Changed the dd/mm/yy to yyyy-mm-dd after Dec 31, 98.
c
c Revision 2.2  1992/03/27  15:58:59  albert
c Used variable array sizes for the maps. Allowed to save pointing direction
c in both galactic and celestial systems. Increased maximum number of bin
c on 1 axis from 200 to 1800 for polar and Aitoff maps.
c
c Revision 2.1  1991/08/28  19:36:16  esp
c First really controlled version on SUN (ported from IBM 3081)
c
C $Id: fitsdt.cmn.inc,v 1.1 1998/09/30 17:01:06 peachey Exp $
C---> COMMON FOR THE FITS DATA
      INTEGER         BITPIX,NAXIS,NAXIS1,NAXIS2,NAXIS3,NAXIS4,PCOUNT
Cesp  ! F77 does not compile BINDAT*2(200,200,20) correctly !
Cesp  INTEGER         GCOUNT,MAPSIZ,DATIME(4),BINDAT*2(200,200,20)
      INTEGER         GCOUNT,MAPSIZ,DATIME(4),MAXBIN
      PARAMETER	      (MAXBIN=648000)
      INTEGER*2       BINDAT(MAXBIN)
Cae   INTEGER         GALDAT(594,66,20)
      REAL            CRVAL1,CRPIX1,CDELT1,CRVAL2,CRPIX2,CDELT2,CRVAL3
      REAL            CRPIX3,CDELT3,BSCALE,BZERO,PDIREC(8)
      REAL            FTPARM(5,1800)
      CHARACTER       CTYPE1*4,CTYPE2*4,CTYPE3*4,BUNIT*12,FITSRC*2880
      CHARACTER       STRTIM*23,ENDTIM*23
      LOGICAL         SIMPLE,GROUPS

Cesp  ! This common block is generating huge 3 meg object file !
Cesp  ! because a field FTPARM is being initialized. To avoid  !
Cesp  ! this, move FTPARM to a separate common block.          !
Cesp  COMMON /FITSDT/ BITPIX,NAXIS,NAXIS1,NAXIS2,NAXIS3,NAXIS4,
Cesp +                CRVAL1,CRPIX1,CDELT1,CTYPE1,CRVAL2,CRPIX2,CDELT2,
Cesp +                CTYPE2,CRVAL3,CRPIX3,CDELT3,CTYPE3,BUNIT,BSCALE,
Cesp +                BZERO,PCOUNT,GCOUNT,MAPSIZ,DATIME,PDIREC,FTPARM,
Cesp +                BINDAT,FITSRC,SIMPLE,GROUPS,STRTIM,ENDTIM

      COMMON /FITSDT/ BITPIX,NAXIS,NAXIS1,NAXIS2,NAXIS3,NAXIS4,
     +                CRVAL1,CRPIX1,CDELT1,CTYPE1,CRVAL2,CRPIX2,CDELT2,
     +                CTYPE2,CRVAL3,CRPIX3,CDELT3,CTYPE3,BUNIT,BSCALE,
     +                BZERO,PCOUNT,GCOUNT,MAPSIZ,DATIME,
     +                PDIREC,BINDAT,FITSRC,SIMPLE,GROUPS,STRTIM,ENDTIM
      common /ftparm/ ftparm

Cae   EQUIVALENCE(BINDAT(1,1,1),GALDAT(1,1,1))

