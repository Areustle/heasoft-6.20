      subroutine rstepr2(unit,hdunum,radin,radout,rdel,t,prs,
     $             xcol,xee,xpx,xi,
     $             idat1,rdat1,kdat1,nptrs,npnxt,npfi,npar,
     $             nplin,nlsvn,rcem,oplin,tau0,nloopctl,
     $             lun11,lpri,status)
C
C     Reads a FITS extension binary table containing
C     nrhs columns and at most nrhdimj rows
c     author: T. Kallman
C
C     Parameters:
C        unit    integer            File unit number
C        hdunum  integer            Number of last HDU written
C        radin   real               inner radius of shell
C        radout  real               outer radius of shell
C                                   nb but now it is delr in the call
C        t    real               temperature of shell
C        prs    real               pressure in shell
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
C        poptol  real               Tolerance for population level
C        nloopctl integer           Loop control variable
C        nzone   integer            Pass number through iteration process
C        status  integer            Returned status code
C
      implicit none
      include './PARAM'
      integer mllz
c
C     Allocation for passed parameters
      real*8 tau0(2,nnnl), rcem(2,nnnl)
      real*8 rdat1(nrdat1)
      real rtmp
      real*8 radin, radout,rdel, t, prs, xcol,xee,xpx,xi
      integer unit,hdunum, nrows, status, nloopctl
      integer idat1(nidat1),nptrs(nptt,ndat2)
c     line opacities
      real*8 oplin(nnnl)

c     pointers to master data
      integer npnxt(ndat2),npfirst(ntyp)
      integer npfi(ntyp,nni),npar(ndat2)
      integer nplin(nnnl),nlsvn

C     Internal work areas
      integer ntptr
      integer tfields,varidat
      character(16) ttype(5),tform(5),tunit(5)
      integer colnum,frow,felem,hdutype,ll, ltyp
      integer lrtyp, lcon, nrdt, nidt, mm, lun11, lpril,lpri
      integer jkk,nidti,nlines
      real eliml,elimh,elin,elmmtpp,elcomp,eth,xeltp
      character(33) extname
      character(20) kcom
      character(1) kdat1(nkdat1)
      integer nhdu

C     Database manipulation quantities
      integer nelems,nullj,nkeys,irow2,nspace
      real anynull,nulle
      character(1) kblnk,kdtmp(200),nullstr
      logical done

      data kblnk/' '/
c
      data tform/'1J','1E','1E','1E','1E'/

      data ttype/'index','energy','opacity','fwd dpth',
     $ 'bck dpth'/

      data tunit/' ','ev','/cm',' ',' '/

      varidat=0
c
c
      if (lpri.ne.0)
     $ write (lun11,*)'in rstepr2 ',hdunum

      call FTGHDN(unit, nhdu)
      if (lpri.ne.0)
     $ write (lun11,*)'current hdu ',nhdu
      call ftmahd(unit,1,hdutype,status)
      call FTGHDN(unit, nhdu)
      if (lpri.ne.0)
     $ write (lun11,*)'current hdu ',nhdu
c
C     Move to the appropriate HDU (hdunum) in the file
      mm=hdunum
c
      if (lpri.ne.0)
     $ write(lun11,*)'rstepr2: Moving to extension',mm
      call ftmahd(unit,mm,hdutype,status)
      if (lpri.ne.0)
     $ write (lun11,*)unit,mm,hdutype,status
      if (status .gt. 0)call printerror(lun11,status)
      call FTGHDN(unit, nhdu)
      if (lpri.ne.0)
     $ write (lun11,*)'current hdu ',nhdu

C     Determine the number of keywords in the header
      nkeys=0
      call ftghsp(unit,nkeys,nspace,status)
      if (lpri.ne.0)
     $ write (lun11,*)'after ftghsp:',unit,nkeys,nspace,status
c
c
c
C     Read each 80-character keyword record, and print it out
      call ftgkyj(unit,'NAXIS2',nrows,kcom,status)
      if (lpri.ne.0)
     $ write (lun11,*)'after ftgkyj:',nrows,kcom,status
      if (status .gt. 0)call printerror(lun11,status)
c
      call ftgkye(unit,'RINNER',rtmp,kcom,status)
      radin=rtmp
      if (lpri.ne.0)
     $ write (lun11,*)'after ftgkye',radin,kcom,status
      if (status .gt. 0)call printerror(lun11,status)
c
      call ftgkye(unit,'ROUTER',rtmp,kcom,status)
      radout=rtmp
      if (lpri.ne.0)
     $ write (lun11,*)'after ftgkye',radout,kcom,status
      if (status .gt. 0)call printerror(lun11,status)

      call ftgkye(unit,'RDEL',rtmp,kcom,status)
      rdel=rtmp
      if (lpri.ne.0)
     $ write (lun11,*)'after ftgkye',rdel,kcom,status
      if (status .gt. 0)call printerror(lun11,status)

      call ftgkye(unit,'TEMPERAT',rtmp,kcom,status)
      t=rtmp
      if (lpri.ne.0)
     $ write (lun11,*)'after ftgkye',t,kcom,status
      if (status .gt. 0)call printerror(lun11,status)

      call ftgkye(unit,'PRESSURE',rtmp,kcom,status)
      if (lpri.ne.0)
     $ write (lun11,*)'after ftgkye',prs,kcom,status
      if (status .gt. 0)call printerror(lun11,status)
c
      call ftgkye(unit,'COLUMN',rtmp,kcom,status)
      xcol=rtmp
      if (lpri.ne.0)
     $ write (lun11,*)'after ftgkye, xcol=',xcol,kcom,status
      if (status .gt. 0)call printerror(lun11,status)

      call ftgkye(unit,'XEE',rtmp,kcom,status)
      xee=rtmp
      if (lpri.ne.0)
     $ write (lun11,*)'after ftgkye',xee,kcom,status
      if (status .gt. 0)call printerror(lun11,status)
c
      call ftgkye(unit,'DENSITY',rtmp,kcom,status)
      xpx=rtmp
      if (lpri.ne.0)
     $ write (lun11,*)'after ftgkye',xpx,kcom,status
      if (status .gt. 0)call printerror(lun11,status)
c
      call ftgkye(unit,'LOGXI',rtmp,kcom,status)
      xi=rtmp
      if (lpri.ne.0)
     $ write (lun11,*)'after ftgkye',xi,kcom,status
      if (status .gt. 0)call printerror(lun11,status)


      felem=1
      nelems=1
      nullstr=' '
      nullj=0
      nulle=0.
      do irow2=1,nrows
      if (lpri.ne.0)
     $   write (lun11,*)'row=',irow2
        colnum=1
        call ftgcvj(unit,colnum,irow2,felem,nelems,nullstr,
     $                  ntptr,anynull,status)
        colnum=6
        call ftgcve(unit,colnum,irow2,felem,nelems,nullstr,
     $                  rtmp,anynull,status)
        rcem(1,ntptr)=rtmp
        colnum=7
        call ftgcve(unit,colnum,irow2,felem,nelems,nullstr,
     $                  rtmp,anynull,status)
        rcem(2,ntptr)=rtmp
        colnum=8
        call ftgcve(unit,colnum,irow2,felem,nelems,nullstr,
     $                  rtmp,anynull,status)
        oplin(ntptr)=rtmp
        colnum=9
        call ftgcve(unit,colnum,irow2,felem,nelems,nullstr,
     $                  rtmp,anynull,status)
        tau0(1,ntptr)=rtmp
        colnum=10
        call ftgcve(unit,colnum,irow2,felem,nelems,nullstr,
     $                  rtmp,anynull,status)
        tau0(2,ntptr)=rtmp
        enddo
C

c----------------------------------------------------------------
C     Compute checksums
      call ftpcks(unit,status)
      if (status .gt. 0)call printerror(lun11,status)

      return
      end
