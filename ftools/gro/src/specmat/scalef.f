C SCALEF
C  Find the effective area scale factor for a given viewing period
C  Reads the file $EGRET_SEQUENCE_DIR/sequence/scale.factor.
C  There are supposed to be 10 different factors for the 10 standard
C  energy bands.  Just return the third one (100-150 MeV) because the
C  energy dependence isn't there and Spectral doesn't use standard bands.

C Written for Spectral version 2.7, December 1994
C Modified for version 2.10, October 1995: The scale factor is now
C  energy-dependent.  There is an array of values.  The call to wtdfac
C  is added to interpolate from the standard energy bands.
C PLN, Stanford
C
C     Modifications:
C     --------------
CHG1
CHG1  Added vp to call list of wtdfac.  11/09/00.  D.L.Bertsch
CHG1

C  @(#) scalef.f 1.3@(#)

      subroutine scalef(vp,scalefac,nregns,eobs,data_dir,status)
      implicit none
      character*(*) data_dir
      character(4) vp
      integer nregns, status
      real scalefac(0:nregns),eobs(0:nregns)
      character(80) inline
      character(80) scalefile
      real sf(10),wtdfac
      integer j
      integer  viewP

      include '../SPECMAT_COMMON/lunits.inc'   ! for luscale

      save

c      call getenv('EGRET_SEQUENCE_DIR',progdir)
      read(vp,5) viewP
 5    format(i4)
      status = 0
      write(*,*) 'vp: ', vp, ' viewP: ', viewP
      call read_scale_factors(data_dir, viewP, sf, status)
      write(*,*) sf


c      scalefile = data_dir(1:len_trim(data_dir))//'/scale.factor'


c      status = 1

c      open (unit=luscale,file=scalefile,status='old',err=666)
c 10   continue
c      read(luscale,'(a)',end=100,err=650) inline
c      if (inline(1:4).ne.vp) go to 10
c      read(inline(5:80),*,err=660) sf
      do j = 1,nregns
         scalefac(j) = wtdfac(vp,eobs(j-1),eobs(j),sf)
      end do
      scalefac(0) = wtdfac(vp,30.,60.,sf)

c      close(luscale)
      status = 0
      return

 100  write (*,*) 'Read entire scale factor file ',scalefile,
     >     ' without finding desired VP ',vp
c      close(luscale)
      return

 650  write (*,*) 'I/O error while reading scale factor file ',
     >     scalefile,'; Previous line:',inline
c      close(luscale)
      return

 660  write (*,*) 'Error parsing line from scale factor file: ',inline
c      close(luscale)
      return

 666  write (*,*) 'Unable to open scale factor file ',scalefile
      return
      end
