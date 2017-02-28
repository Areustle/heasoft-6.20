      subroutine fstepr4(unit,hdunum,radin,radout,rdel,t,prs,
     $             xcol,xee,xpx,xi,
     $             idat1,rdat1,kdat1,nptrs,npnxt,npfi,npar,
     $             epi,ncn2,dpthc,opakc,rccemis,nloopctl,
     $             lun11,lpri,status)
C
C     Append a FITS extension binary table containing
C     nrhs columns and at most nrhdimj rows
c     author: T. Kallman
C
C     Parameters:
C        unit    integer            File unit number
C        hdunum  integer            Number of last HDU written
C        radin   real*8               inner radius of shell
C        radout  real*8               outer radius of shell
C                                   nb but now it is delr in the call
C        t    real*8               temperature of shell
C        prs    real*8               pressure in shell
C        nrhdimj  integer            Maximum number of rows
C        idat1   integer(nidat1)    Needed by the atomic database
C        rdat1   real(nidat1)       Needed by the atomic database
C        kdat1   char*nidat1        Needed by the atomic database
C        nptrs                      Needed by the atomic database
C        npnxt                      Needed by the atomic database
C        npfi                       Needed by the atomic database
C        npfirst                    Needed by the atomic database
C        npcon                      Needed by the atomic database
C        npconi                     Needed by the atomic database
C        npcon2                     Needed by the atomic database
C        xilev   real(nrhdimj)       Fractional level population array
C        cemab   real(2,nrhdimj)     Recombination emission
C        opakab  real(nrhdimj)       Opacity
C        tauc    real(2,nrhdimj)     Optical depth
C        poptol  real*8               Tolerance for population level
C        nloopctl integer           Loop control variable
C        nzone   integer            Pass number through iteration process
C        status  integer            Returned status code
C
      implicit none
      include './PARAM'
      integer mllz

      integer nptmpdim
      parameter (nptmpdim=ncn)
c
C     Allocation for passed parameters
      real*8 rdat1(nrdat1)
      real rtmp
      real*8 radin, radout,rdel, t, prs, xcol,xee,xpx,xi
      integer unit,hdunum, nrows, status, nloopctl
      integer idat1(nidat1),nptrs(nptt,ndat2)
c     energy bins
      real*8 epi(ncn)
c     continuum opacities
      real*8 opakc(ncn)
c     continuum optical depths
      real*8 dpthc(2,ncn)
c     continuum emissivities
      real*8 rccemis(2,ncn)
      integer ncn2

c     pointers to master data
      integer npnxt(ndat2),npfirst(ntyp)
      integer npfi(ntyp,nni),npar(ndat2)

C     Internal work areas
      real ntptr(nptmpdim)
      real rwrk1(nptmpdim)
      integer tfields,varidat
      character(16) ttype(5),tform(5),tunit(5)
      integer colnum,frow,felem,hdutype,ll, ltyp
      integer lrtyp, lcon, nrdt, nidt, mm, lun11, lpril,lpri
      integer jkk,nidti,nlines
      real*8 eliml,elimh,elin,elmmtpp,elcomp,eth,xeltp
      character(33) extname
      character(20) ktmp20
      character(20) ktmp2
      character(1) kdat1(nkdat1)

C     Database manipulation quantities
      integer nkdt
      character(1) kblnk,kdtmp(200)
      logical done

      data kblnk/' '/
c
      data tform/'1J','1E','1E','1E','1E'/

      data ttype/'index','energy','opacity','fwd dpth',
     $ 'bck dpth'/

      data tunit/' ','ev','/cm',' ',' '/

      varidat=0
c
      if (lpri.ne.0)
     $ write (lun11,*)'in fstepr4 input hdu',hdunum
c
C     Move to the last HDU (hdunum) in the file
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr4: Moving to end-of-FITS file'
      call ftmahd(unit,hdunum,hdutype,status)
      if (status .gt. 0)call printerror(lun11,status)

C     append a new empty extension after the last HDU
      if (lpri.ne.0)
     $ write (lun11,*)'fstepr4: Create the new extension'
      call ftcrhd(unit,status)
      if (status .gt. 0)call printerror(lun11,status)

C----------------------------------------------------------------
C
C     Extracting data from the Atomic Database here
C
c

C     End of atomic database extraction
C----------------------------------------------------------------
C     define parameters for the binary table (see the above data statements)
      nrows=ncn2
      if (lpri.ne.0)
     $ write (lun11,*)'before header write'
      tfields=5
C     Build extension name
      extname='XSTAR_RADIAL'
      if(nloopctl.gt.0) then
          write(ktmp2,'(I4.4)')nloopctl
          extname='XSTAR_RADIAL_' // ktmp2
          endif

      if (lpri.ne.0)
     $ write (lun11,*)'fstepr4: Write table headers'
C     write the required header parameters for the binary table
      call ftphbn(unit,nrows,tfields,ttype,tform,tunit,extname,
     $              varidat,status)
      if (status .gt. 0)call printerror(lun11,status)

      if (lpri.ne.0)
     $ write (lun11,*)'fstepr4: Add some more keywords'

C     Write some model parameters in the extension header
      call ftpcom(unit,'***********************************',status)
      if (status .gt. 0)call printerror(lun11,status)

      call ftpcom(unit,'Model Keywords',status)
      if (status .gt. 0)call printerror(lun11,status)

C     Write values to 3 decimal places
      rtmp=radin
      call ftpkye(unit,'RINNER',rtmp,3,'[cm] Inner shell radius',
     $ status)
      if (status .gt. 0)call printerror(lun11,status)

      rtmp=radout
      call ftpkye(unit,'ROUTER',rtmp,3,'[cm] Outer shell radius',
     $ status)
      if (status .gt. 0)call printerror(lun11,status)

      rtmp=rdel
      call ftpkye(unit,'RDEL',rtmp,3,'[cm] distance from face',
     $ status)
      if (status .gt. 0)call printerror(lun11,status)

      rtmp=t
      call ftpkye(unit,'TEMPERAT',rtmp,3,'[10**4K] Shell Temperature',
     $ status)
      if (status .gt. 0)call printerror(lun11,status)

      rtmp=prs
      call ftpkye(unit,'PRESSURE',rtmp,3,'[dynes/cm**2] Shell Pressure',
     $ status)
      if (status .gt. 0)call printerror(lun11,status)
c
      rtmp=xcol
      call ftpkye(unit,'COLUMN',rtmp,3,'[/cm**2] Column ',
     $ status)
      if (status .gt. 0)call printerror(lun11,status)

      rtmp=xee
      call ftpkye(unit,'XEE',rtmp,3,'electron fraction',
     $ status)
      if (status .gt. 0)call printerror(lun11,status)
c
      rtmp=xpx
      call ftpkye(unit,'DENSITY',rtmp,3,'[/cm**3] Density',
     $ status)
      if (status .gt. 0)call printerror(lun11,status)
c
      rtmp=xi
      call ftpkye(unit,'LOGXI',rtmp,3,
     $ '[erg cm/s] log(ionization parameter)',status)
      if (status .gt. 0)call printerror(lun11,status)

      if (lpri.ne.0)
     $ write (lun11,*)'after header write'
C-------------------------------------------------------------------
C     Step through the columns and write them to the file
C
C     set 'global' parameters for writing FITS columns
      frow=1
      felem=1

      do mm=1,ncn2
        ntptr(mm)=mm
        enddo

C     column  1  (continuum index)
      colnum=1
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr4: Writing Column ',colnum
      nlines=ncn2
      call ftpclj(unit,colnum,frow,felem,nlines,ntptr,status)
      if (status .gt. 0)call printerror(lun11,status)
c
      do mm=1,ncn2
        rwrk1(mm)=epi(mm)
        enddo
c
C     column  2 energy
      colnum=2
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr4: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nlines,rwrk1,status)
      if (status .gt. 0)call printerror(lun11,status)
      if (status .gt. 0) return

c
      do mm=1,ncn2
        rwrk1(mm)=opakc(mm)
        enddo
c
C     column  3 opacity
      colnum=3
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr4: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nlines,rwrk1,status)
      if (status .gt. 0)call printerror(lun11,status)
      if (status .gt. 0) return

c
      do mm=1,ncn2
        rwrk1(mm)=dpthc(1,mm)
        enddo
c
C     column  4 depth forward
      colnum=4
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr4: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nlines,rwrk1,status)
      if (status .gt. 0)call printerror(lun11,status)
      if (status .gt. 0) return

c
      do mm=1,ncn2
        rwrk1(mm)=dpthc(2,mm)
        enddo
c
C     column  5 depth backward
      colnum=5
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr4: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nlines,rwrk1,status)
      if (status .gt. 0)call printerror(lun11,status)
      if (status .gt. 0) return




c----------------------------------------------------------------
C     Compute checksums
      call ftpcks(unit,status)
      if (status .gt. 0)call printerror(lun11,status)

      return
      end
