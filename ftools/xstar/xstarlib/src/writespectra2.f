      subroutine writespectra2(lun11,lpri,nparms,parname,partype,parval,
     $       parcomm,atcredate,epi,ncn2,dpthc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,elum,tau0,kmodelname,nloopctl)

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
c
      integer ncn2
      include './PARAM'
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
c     continuum optical depths
      real*8 dpthc(2,ncn)
c     line optical depths
      real*8 tau0(2,nnnl)
      real rtmp(ncn)
      character(16) knam,klabs(9),kunits(9),kform(9),kblnk16
      character(30) extname
      integer unit,istatus, nilin, nkdt,nidt,lcon,lrtyp,ltyp,ml,status
      integer nlsvn, ln, ll, lnn, nrdt,mllz
      integer tbcol(9), nrows, rowlen
      integer np2, kk
      integer frow, felem, colnum, tfields, verbose,mm
      real*8 eliml, elimh, elmmtpp,elin
C     Internal work areas
      integer ntptr(nnnl)
      character(10) kion(nnnl)
      character(20) klevl(nnnl),klevu(nnnl)
      integer lpri,lpril
      integer jkk, nlev
      integer nlplmx,nilin2,nlpl,lmm,kltmpn,kltmpo,
     $         llo,lup,llofnd,lupfnd,
     $         k,kl2,lm,kk2,mlpar,mlm,np1i,np1k,np1r
      real*8 elcomp
      character(20) ktmp2,kblnk20
c     Database manipulation quantities
      character(1) kblnk,kdtmp(200)
      integer kltmp(1000)
      real elsv(1000)
      logical done
c
c     Not used
      real*8 javir
      integer javi
c
      data kblnk/' '/
      data kblnk16/'                '/
      data kblnk20/'                    '/
c
      if (nlsvn.le.5) return

      javir=epi(1)
      epi(1)=javir
      javir=dpthc(1,1)
      javi=ncn2
      np2=javi
      javi=npfirst(1)
      javi=nplini(1)
      javi=npcon(1)
      javi=npconi(1)
      javi=npilev(1,1)
      javi=npilevi(1)
      javi=npconi2(1)
c

      verbose=lpri
      eliml=0.1
      elimh=1.0e10
c
c     open and prepare the fits file for spectral data
      if(verbose.gt.0) write (lun11,*)'writespectra2: opening header',
     $  kmodelname
      knam='xout_lines1.fits'
      call fheader(unit,knam,atcredate,kmodelname,istatus)
      if(istatus.gt.0) call printerror(lun11,istatus)

c     write extension of parameter values
      if(verbose.gt.0)
     $ write (lun11,*)'writespectra2: write parameter list'
      call fparmlist(unit,1,kmodelname,nparms,parname,partype,parval,
     $               parcomm,nloopctl,istatus,lun11)
      if(istatus.gt.0) call printerror(lun11,istatus)
      if(verbose.gt.0)
     $  write (lun11,*)'writespectra2: building data tables'
c
c     build spectra data tables
      if (verbose.gt.0) write (lun11,*)' '
      kltmpo=0
      lpril=0
      if (verbose.gt.0)
     $  write (lun11,*)'emission line luminosities (erg/sec/10**38))'
      nlplmx=1000
      eliml=0.1
      elimh=1.0e10
c     find the strongest lines.
      do  lm=1,nlplmx
       kltmp(lm)=0
       enddo
      nlpl=1
      do lnn=1,nlsvn
        ln=lnn
        ml=nplin(ln)
        mlm=ml-1
        call drd(ltyp,lrtyp,lcon,
     $    nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $    nptrs,0,lun11)
        elin=abs(rdat1(np1r))
        if ((lrtyp.ne.14).and.(lrtyp.ne.9)) then
          nilin=npar(ml)
          mlm=nilin-1
          call drd(ltyp,lrtyp,lcon,
     $      nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $      nptrs,0,lun11)
          nilin2=idat1(np1i-1+nidt)
          elmmtpp=(elum(2,ln)+elum(1,ln))/2.
          if (verbose.gt.0)
     $         write (lun11,*)lnn,elin,nilin,elmmtpp,ln,ml
          if ((ln.gt.0).and.(ln.lt.nnnl)
     $         .and.(elin.ge.eliml).and.(elin.le.elimh)
     $         .and.(elin.le.8.9e+6)
     $         .and.(elmmtpp.gt.1.e-36)
     $         .and.(nilin2.gt.0).and.(nilin2.le.nni))
     $           then
c
            lmm=0
            elcomp=1.e+10
            do while ((lmm.lt.nlpl).and.(elmmtpp.lt.elcomp))
              lmm=lmm+1
              kl2=kltmp(lmm)
              elcomp=0.
              if (kl2.gt.0)
     $          elcomp=(elum(2,kl2)+elum(1,kl2))/2.
              enddo
c
            if (verbose.gt.0)
     $       write (lun11,8516)ln,elin,elmmtpp
8516        format (1h ,i4,2e12.4)
            kltmpo=ln
            do  k=lmm,min(nlplmx,nlpl)
              if ((lpril.ne.0).and.(kltmp(k).ne.0))
     $         write (lun11,*)'in 557 loop',k,kltmp(k),kltmpo
              kltmpn=kltmp(k)
              kltmp(k)=kltmpo
              kltmpo=kltmpn
              enddo
             nlpl=min(nlplmx,nlpl+1)
            if (verbose.gt.0)
     $       write (lun11,*)'done with 557 loop',lm
            endif
          endif
        enddo
      if (nlpl.gt.0) kltmp(nlpl)=kltmpo
c      nlpl=nlpl-1
      if (verbose.gt.0)
     $    write (lun11,959)
959   format (1x,'index, ion, wavelength, transmitted, reflected')
      kk2=0
      do  kk=1,nlpl
        ln=kltmp(kk)
        if (ln.ne.0) then
          ml=nplin(ln)
          klevl(kk)=kblnk20
          klevu(kk)=kblnk20
          if (ml.ne.0) then
            if (verbose.gt.0)
     $        write (lun11,*)'   ',ln,ml
            mlm=ml-1
            call drd(ltyp,lrtyp,lcon,
     $        nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $        nptrs,0,lun11)
            llo=idat1(np1i)
            lup=idat1(np1i+1)
            elsv(kk)=abs(rdat1(np1r))
            nilin=npar(ml)
            mlm=nilin-1
            call drd(ltyp,lrtyp,lcon,
     $        nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $        nptrs,0,lun11)
            do mm=1,nkdt
              kdtmp(mm)=kdat1(np1k-1+mm)
              enddo
            do mm=nkdt+1,10
              kdtmp(mm)=kblnk
              enddo
c            nilin=idat(nidt)
            write(kion(kk),'(10a1)')(kdtmp(mm),mm=1,10)
            done=.false.
            jkk=1
            do while (.not.done)
              ml=npfi(13,jkk)
              if (ml.ne.0) then
                if ((npar(ml).eq.nilin).or.(jkk.gt.nni)) 
     $            done=.true.
                endif
              jkk=jkk+1
              enddo
            if (jkk.gt.nni) ml=0
            if (ml.ne.0) then
              mllz=npar(ml)
              mlpar=npar(ml)
              lupfnd=0
              llofnd=0
              do while ((ml.ne.0).and.(mlpar.eq.mllz)
     $           .and.((llofnd.ne.1).or.(lupfnd.ne.1)))
                mlm=ml-1
                call drd(ltyp,lrtyp,lcon,
     $            nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $            nptrs,0,lun11)
                nlev=idat1(np1i+nidt-2)
                if (lpri.ne.0)
     $            call dprinto(ltyp,lrtyp,lcon,
     $            nrdt,np1r,nidt,np1i,nkdt,np1k,rdat1,idat1,kdat1,lun11) 
                if (lpri.ne.0)
     $            write (lun11,*)nlev,llo,lup,llofnd,lupfnd
                if (nlev.eq.llo) then
                  do mm=1,20
                    if (mm.le.nkdt) then
                        write(ktmp2(mm:mm),'(a1)')kdat1(np1k-1+mm)
                      else
                        write(ktmp2(mm:mm),'(a1)')kblnk
                      endif
                    enddo
                  klevl(kk)=ktmp2
                  llofnd=1
                  endif
                if (nlev.eq.lup) then
                  do mm=1,20
                    if (mm.le.nkdt) then
                        write(ktmp2(mm:mm),'(a1)')kdat1(np1k+mm-1)
                      else
                        write(ktmp2(mm:mm),'(a1)')kblnk
                      endif
                    enddo
                  klevu(kk)=ktmp2
                  lupfnd=1
                  endif
                ml=npnxt(ml)
                mlpar=0
                if (ml.ne.0) mlpar=npar(ml)
                enddo
              kk2=kk2+1
              ntptr(kk2)=ln
              if (verbose.gt.0) then
                write (lun11,*)ml,nilin,npar(ml)
                write (lun11,9955)kk,ln,(kdtmp(mm),mm=1,9),elsv(kk),
     $               elum(1,ln),elum(2,ln)
                write (lun11,*)klevu(kk)
                write (lun11,*)klevl(kk)
                endif
 9955           format (1x,2i8,1x,9a1,3(1pe11.3))
              endif
            endif
          endif
        enddo
c      if (nlpl.le.0) return
c
      nlpl=kk2
      nlpl=max(nlpl,1)
c
      if (verbose.gt.0) then
        do kk=1,nlpl
          write (lun11,*)kk,ntptr(kk)
          enddo
        endif
c

c     write the spectral data to the extension
      do mm=1,9
        kunits(mm)=kblnk16
        klabs(mm)=kblnk16
        kform(mm)=kblnk16
        enddo
      klabs(1)='index           '
      kform(1)='I6'
      kunits(1)='  '
      klabs(2)='ion             '
      kform(2)='A9'
      kunits(2)=' '
      klabs(3)='lower_level     '
      kform(3)='A20'
      kunits(3)='  '
      klabs(4)='upper_level     '
      kform(4)='A20'
      kunits(4)='  '
      klabs(5)='wavelength      '
      kform(5)='F10.2'
      kunits(5)='A'
      klabs(6)='emit_inward     '
      kform(6)='E11.3'
      kunits(6)='erg/s/10**38'
      klabs(7)='emit_outward    '
      kform(7)='E11.3'
      kunits(7)='erg/s/10**38'
      klabs(8)='depth_inward    '
      kform(8)='E11.3'
      kunits(8)='  '
      klabs(9)='depth_outward   '
      kform(9)='E11.3'
      kunits(9)='  '
c     build extension name
      extname='XSTAR_LINES'
C      if(nloopctl.gt.0) then
C          write(ktmp2,'(i4.4)')nloopctl
C          extname='xstar_spectra_' // ktmp2
C          endif
      if(verbose.gt.0)
     $   write (lun11,*)'writespectra2: writing spectral data'

c     append a new empty extension onto the end of the primary array
      status=0
      call ftcrhd(unit,status)
      if(verbose.gt.0)
     $    write (lun11,*)'writespectra2: writing header table'

      tfields=9
      nrows=nlpl
      rowlen=0
      do mm=1,9
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
      kk=1
        if(verbose.gt.0)
     $      write (lun11,*)'writespectra2: building column ',kk
        frow=1
        felem=1
        colnum=kk
        if(verbose.gt.0)
     $     write (lun11,*)'writespectra2: writing column ',kk,nrows
        status=0
        call ftpclj(unit,colnum,frow,felem,nrows,ntptr,status)
        if (status .gt. 0)call printerror(lun11,status)
      kk=2
        if(verbose.gt.0)
     $      write (lun11,*)'writespectra2: building column ',kk
        frow=1
        felem=1
        colnum=kk
        if(verbose.gt.0)
     $     write (lun11,*)'writespectra2: writing column ',kk,nrows
        status=0
        call ftpcls(unit,colnum,frow,felem,nrows,kion,status)
        if (status .gt. 0)call printerror(lun11,status)
      kk=3
        if(verbose.gt.0)
     $      write (lun11,*)'writespectra2: building column ',kk
        frow=1
        felem=1
        colnum=kk
        if(verbose.gt.0)
     $     write (lun11,*)'writespectra2: writing column ',kk
        status=0
        call ftpcls(unit,colnum,frow,felem,nrows,klevl,status)
        if (status .gt. 0)call printerror(lun11,status)
      kk=4
        if(verbose.gt.0)
     $      write (lun11,*)'writespectra2: building column ',kk
        frow=1
        felem=1
        colnum=kk
        if(verbose.gt.0)
     $     write (lun11,*)'writespectra2: writing column ',kk
        status=0
        call ftpcls(unit,colnum,frow,felem,nrows,klevu,status)
        if (status .gt. 0)call printerror(lun11,status)
      kk=5
        if(verbose.gt.0)
     $      write (lun11,*)'writespectra2: building column ',kk
        frow=1
        felem=1
        colnum=kk
        if(verbose.gt.0)
     $     write (lun11,*)'writespectra2: writing column ',kk
        status=0
        call ftpcle(unit,colnum,frow,felem,nrows,elsv,status)
        if (status .gt. 0)call printerror(lun11,status)
      kk=6
        if(verbose.gt.0)
     $      write (lun11,*)'writespectra2: building column ',kk
        frow=1
        felem=1
        colnum=kk
        do ll=1,nrows
          rtmp(ll)=elum(1,ntptr(ll))
          enddo
        if(verbose.gt.0)
     $     write (lun11,*)'writespectra2: writing column ',kk
        status=0
        call ftpcle(unit,colnum,frow,felem,nrows,rtmp,status)
        if (status .gt. 0)call printerror(lun11,status)
      kk=7
        if(verbose.gt.0)
     $      write (lun11,*)'writespectra2: building column ',kk
        frow=1
        felem=1
        colnum=kk
        do ll=1,nrows
          rtmp(ll)=elum(2,ntptr(ll))
          enddo
        if(verbose.gt.0)
     $     write (lun11,*)'writespectra2: writing column ',kk
        status=0
        call ftpcle(unit,colnum,frow,felem,nrows,rtmp,status)
        if (status .gt. 0)call printerror(lun11,status)
      kk=8
        if(verbose.gt.0)
     $      write (lun11,*)'writespectra2: building column ',kk
        frow=1
        felem=1
        colnum=kk
        do ll=1,nrows
          rtmp(ll)=tau0(1,ntptr(ll))
          enddo
        if(verbose.gt.0)
     $     write (lun11,*)'writespectra2: writing column ',kk
        status=0
        call ftpcle(unit,colnum,frow,felem,nrows,rtmp,status)
        if (status .gt. 0)call printerror(lun11,status)
      kk=9
        if(verbose.gt.0)
     $      write (lun11,*)'writespectra2: building column ',kk
        frow=1
        felem=1
        colnum=kk
        do ll=1,nrows
          rtmp(ll)=tau0(2,ntptr(ll))
          enddo
        if(verbose.gt.0)
     $     write (lun11,*)'writespectra2: writing column ',kk
        status=0
        call ftpcle(unit,colnum,frow,felem,nrows,rtmp,status)
        if (status .gt. 0)call printerror(lun11,status)

c     compute checksums
      if(verbose.gt.0) write (lun11,*)'writespectra2: writing checksum'
      status=0
      call ftpcks(unit,status)
c     check for any error, and if so print out error messages
      if (status .gt. 0)call printerror(lun11,status)

      if(verbose.gt.0) write (lun11,*)'writespectra2: closing file'
      call fitsclose(lun11,unit,istatus)
c
c
      return
      end
