C $Id: infits.cmn.inc,v 1.2 1997/11/12 15:27:57 elwin Exp $
C $Log: infits.cmn.inc,v $
C Revision 1.2  1997/11/12 15:27:57  elwin
C Are we dead yet
C
C Revision 1.1  1997/11/07  20:51:17  silvis
C The name infits.cmn.f was changed to infits.cmn.inc
C
C
C J Silvis
C
C Revision 1.1  1997/09/03 20:15:58  silvis
C This is the intial input to CVS of the ftool eaddmap.  This Ftool will add
C two EGRET maps.
C
C Jeff Silvis
C 3 Sept 1997
C Hughes STX
C
C Revision 1.5  1994/01/28  20:47:13  albert
C Added variable to store the cutoff angle specified by the user.
C
C Revision 1.4  1993/07/29  19:14:02  albert
C Increased length of a variable to store longer BUNIT FITS values.
C
C Revision 1.3  1993/05/24  13:57:17  albert
C Changed the variable for the total counts from integer to real because
C counts are now reals.
C
C Revision 1.2  1992/04/16  16:38:50  albert
C Modified to remove the input bin data arrays from the common block and add
C variables for the limits on the number of bins on the axis.
C
C Revision 1.1  1992/01/29  19:54:11  albert
C Initial revision
C
C
C---> infits common block. Contains variables for the input FITS files
      integer	       ixl,iyl,cutoff
      parameter	       (ixl=720, iyl=360)
      real             ibscal(2),ibzero(2),iftprm(5,ixl),iznmax(10)
      real             ienrgy(2,10),icrpx1,icrpx2,itotcn
      real	       imaxvl(2),iminvl(2),ixmaxp(2),iymaxp(2)
      integer          ibitpx(2),inaxis,inaxs1,inaxs2,inaxs3,inax12(ixl)
      integer          igrcnt,iblank(2),iprcnt
      integer          izmaxp(2)
      character        igridt*4,icoord*4,ibunit(2)*20
      double precision icrvl1,icdel1,icrvl2,icdel2

      common /infits/  icrvl1,icdel1,icrvl2,icdel2,ibitpx,inaxis,
     &                 inaxs1,inaxs2,inaxs3,ibscal,ibzero,iftprm,
     &                 igridt,ienrgy,iprcnt,igrcnt,inax12,icrpx1,
     &                 icrpx2,iznmax,iblank,icoord,ibunit,cutoff,
     &		       itotcn,imaxvl,iminvl,ixmaxp,iymaxp,izmaxp










