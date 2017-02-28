      subroutine writespectra3(lun11,lpri,nparms,parname,partype,parval,
     $       parcomm,atcredate,epi,ncn2,dpthc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,elum,zrems,zremsz,kmodelname,nloopctl)

C
C     Write extension containing the spectra for
C     this particular model.
C
C     Modifications:
C       04/01/1999,WTB: Disabled appending loop control value to
C               extension name due to changes in xstar2xspec design
C
c     author:  T. Bridgman
c
      implicit none

      integer ncn2

      include './PARAM'
c
c
c     passed parameters
      character(30) kmodelname
      integer nparms, nloopctl, lun11
      character(20) parname(55)
      character(10) partype(55)
      real*8 parval(55)
      character(30) parcomm(55)
c     master data
      integer idat1(nidat1),nptrs(nptt,ndat2)
      real*8 rdat1(nrdat1)
      character(1) kdat1(nkdat1)
c     pointers to master data
      integer npar(ndat2),npnxt(ndat2),npfirst(ntyp)
      integer npfi(ntyp,nni)
c     pointers to line data
      integer nplin(nnnl),nplini(ndat2)
c     pointers to line data
      integer npcon(nnml),npconi2(ndat2),npconi(ndat2)
      integer npilev(nd,nni),npilevi(nnml)
c     line luminosities
      real*8 elum(3,nnnl)
c     energy bins
      real*8 epi(ncn)
c     the atomic data creation date
      character(63) atcredate
c     continuum lum
      real*8 zrems(4,ncn),zremsz(ncn)
c     continuum optical depths
      real*8 dpthc(2,ncn)
      real*8 zrtmp(5,ncn)
      real rtmp(ncn)
      character(16) knam,klabs(5),kunits(5),kform(5),kblnk16
c      character(30) ktmp2
      character(30) extname
      integer unit,istatus, kl,lpri
      integer nlsvn, ll, numcon
      integer np2, tbcol(5), nrows, rowlen, kk
      integer frow, felem, colnum, tfields, status, verbose,mm
c
c     Not used
      real*8 javir
      integer javi
c      character(80) javik
c
      data kblnk16/'                '/

      javir=epi(1)
c      epi(1)=javir
      javir=dpthc(1,1)
      javi=ncn2
      javi=np2
c      np2=javi
      javi=npfirst(1)
      javi=nplini(1)
      javi=npcon(1)
      javi=npconi(1)
      javi=npilev(1,1)
      javi=npilevi(1)
      javi=npconi2(1)

      javi=idat1(1)
      javir=rdat1(1)
c      javik=kdat1(1)
      javi=nptrs(1,1)
      javi=npar(1)
      javi=npnxt(1)
      javi=npfi(1,1)
      javi=nplin(1)
      javi=nlsvn
      javir=elum(1,1)

c

      verbose=lpri
c
c     open and prepare the fits file for spectral data
      if(verbose.gt.0) write (lun11,*)'writespectra3: opening header',
     $  kmodelname
      knam='xout_cont1.fits'
      call fheader(unit,knam,atcredate,kmodelname,istatus)
      if(istatus.gt.0) call printerror(lun11,istatus)

c     write extension of parameter values
      if(verbose.gt.0)
     $  write (lun11,*)'writespectra: write parameter list'
      call fparmlist(unit,1,kmodelname,nparms,parname,partype,parval,
     $               parcomm,nloopctl,istatus,lun11)
      if(istatus.gt.0) call printerror(lun11,istatus)
      if(verbose.gt.0)
     $  write (lun11,*)'writespectra: building data tables'

c     build spectra data tables
      numcon=ncn2
      do ll=1,ncn2
        zrtmp(4,ll)=0.
        zrtmp(5,ll)=0.
        enddo
      do kl=1,numcon
         zrtmp(4,kl)=zrtmp(4,kl)+zrems(2,kl)
         zrtmp(5,kl)=zrtmp(5,kl)+zrems(3,kl)
         zrtmp(3,kl)=zremsz(kl)*exp(-dpthc(1,kl))
c         write (lun11,968)kl,epi(kl),zremsz(kl),
c     $          zrtmp1(kl),zrtmp2(kl)
         zrtmp(2,kl)=zremsz(kl)
         zrtmp(1,kl)=epi(kl)
         enddo

c     write the spectral data to the extension
      do mm=1,5
        kunits(mm)=kblnk16
        klabs(mm)=kblnk16
        kform(mm)=kblnk16
        enddo
      klabs(1)='energy          '
      kform(1)='E11.3'
      kunits(1)='eV'
      klabs(2)='incident        '
      kform(2)='E11.3'
      kunits(2)='erg/s/erg'
      klabs(3)='transmitted     '
      kform(3)='E11.3'
      kunits(3)='erg/s/erg'
      klabs(4)='emit_inward     '
      kform(4)='E11.3'
      kunits(4)='erg/s/erg'
      klabs(5)='emit_outward    '
      kform(5)='E11.3'
      kunits(5)='erg/s/erg'
c     build extension name
      extname='XSTAR_SPECTRA'
C      if(nloopctl.gt.0) then
C          write(ktmp2,'(i4.4)')nloopctl
C          extname='xstar_spectra_' // ktmp2
C          endif
      if(verbose.gt.0)
     $  write (lun11,*)'writespectra: writing spectral data'
c      call writespectra(unit,ktmp1,zrtmp,5,999,ncn,
c     $                 klabs,kform,kunits)

c     append a new empty extension onto the end of the primary array
      status=0
      call ftcrhd(unit,status)
      if(verbose.gt.0)
     $   write (lun11,*)'writespectra: writing header table'

      tfields=5
      nrows=ncn2
      rowlen=0
      do mm=1,5
      tbcol(mm)=0
      enddo

c     write the required header parameters for the ascii table
      status=0
      call ftphtb(unit,rowlen,nrows,tfields,klabs,tbcol,kform,kunits,
     &            extname,status)
      if (status .gt. 0)call printerror(lun11,status)
      status=0
c
c     map each column to a 1-d array before writing to the file
      do kk=1,tfields
        if(verbose.gt.0)
     $    write (lun11,*)'writespectra: building column ',kk
        frow=1
        felem=1
        colnum=kk
        do ll=1,nrows
          rtmp(ll)=zrtmp(kk,ll)
          enddo
        if(verbose.gt.0)
     $    write (lun11,*)'writespectra: writing column ',kk
        status=0
        call ftpcle(unit,colnum,frow,felem,nrows,rtmp,status)
        if (status .gt. 0)call printerror(lun11,status)
        enddo

c     compute checksums
      if(verbose.gt.0) write (lun11,*)'writespectra: writing checksum'
      call ftpcks(unit,status)
c     check for any error, and if so print out error messages
      if (status .gt. 0)call printerror(lun11,status)

      if(verbose.gt.0) write (lun11,*)'writespectra: closing file'
      call fitsclose(lun11,unit,istatus)
c
      return
      end
