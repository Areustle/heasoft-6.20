C $Id: oufits.cmn.inc,v 1.2 2013/05/21 19:08:23 irby Exp $
C $Log: oufits.cmn.inc,v $
C Revision 1.2  2013/05/21 19:08:23  irby
C Change character*n to character(n) to silence warnings: "Obsolescent
C feature: Old-style character length".
C
C Revision 1.1  1997/11/07 20:48:59  silvis
C Name changed from oufits.cmn.f to oufits.cmn.inc
C
C Revision 1.1  1997/09/03 20:16:03  silvis
C This is the intial input to CVS of the ftool eaddmap.  This Ftool will add
C two EGRET maps.
C
C Jeff Silvis
C 3 Sept 1997
C Hughes STX
C
c Revision 1.4  1993/02/22  14:28:33  albert
c Added variable for the current run number to how many times 1st input map
c has been added.
c
c Revision 1.3  1992/06/15  18:06:41  albert
c Increased the size of the buffer to hold the FITS header entries from the
c input files to the output files.
c
c Revision 1.2  1992/04/16  16:43:57  albert
c Removed the bin data arrays from the common block and added variables to
c set the limits on the number of bins on the X and Y axis.
c
c Revision 1.1  1992/01/29  19:54:11  albert
c Initial revision
c
c
C---> oufits common block. Contains variables for the output FITS files
      integer	       oxl,oyl,runnum
      parameter	       (oxl=720, oyl=360)
      real             obscal(3),obzero(3),oftprm(5,oxl),oznmax(10)
      real             ocrpx1,ocrpx2,oenrgy(2,10),omaxvl(3),ominvl(3)
      real             oxmaxp(3),oymaxp(3)
      integer          obitpx,onaxis,onaxs1
      integer          onaxs2,onaxs3,onax12(oxl)
      integer          ogrcnt,oblank(3),oprcnt
      integer          omapsz,ozmaxp(3),oindex(2)
C**************************************************************************
C
C  FTOOL Change
C
C The fits file is no longer read into a buffer.  Rather fitsio is 
C is used, so obuffr is no longer used by the code.
C
C  Jeff Silvis 
C 
C  Sept 1997
C**************************************************************************
C      character        obuffr(3)*2880,ogridt*4,ocoord*4,oheadr(2,1000)*80
      character      ogridt*4,ocoord*4
      character(80)     oheadr(2,1000)
      double precision ocrvl1,ocdel1,ocrvl2,ocdel2

      common /oufits/  ocrvl1,ocdel1,ocrvl2,ocdel2,obitpx,
     &                 onaxis,onaxs1,onaxs2,onaxs3,obscal,
     &                 obzero,oindex,oftprm,ogridt,oenrgy,
     &                 oprcnt,ogrcnt,onax12,ocrpx1,ocrpx2,
     &                 oznmax,oblank,omapsz,omaxvl,ominvl,
     &                 oxmaxp,oymaxp,ozmaxp,runnum,ocoord,
     &                 oheadr
C**************************************************************************
C
C  FTOOL Change
C
C The fits file is no longer read into a buffer.  Rather fitsio is 
C is used, so obuffr is no longer used by the code.
C
C  Jeff Silvis 
C 
C  Sept 1997
C      &                 obuffr,oheadr
C**************************************************************************
















