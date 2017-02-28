      subroutine fstepr2(unit,hdunum,radin,radout,rdel,temp,pres,
     $                xcol,xee,xpx,xi,
     $                idat1,rdat1,kdat1,nptrs,npnxt,npfi,npar,
     $                nplin,nlsvn,rcem,oplin,tau0,nloopctl,
     $                lun11,lpri,status)
C
C     Append a FITS extension binary table containing
C     nrhs columns and at most nrhdimj rows
c     author: T. Bridgman
C
C     Parameters:
C        unit    integer            File unit number
C        hdunum  integer            Number of last HDU written
C        radin   real*8               inner radius of shell
C        radout  real*8               outer radius of shell
C                                   nb but now it is delr in the call
C        temp    real*8               temperature of shell
C        pres    real*8               pressure in shell
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
      parameter (nptmpdim=500000)
c
C     Allocation for passed parameters
      real*8 tau0(2,nnnl), rcem(2,nnnl)
      real*8 rdat1(nrdat1)
      real rtmp
      real*8 radin, radout,rdel, temp, pres,xcol,xee,xpx,xi
      integer unit,hdunum, nrows, status, nloopctl
      integer idat1(nidat1),nptrs(nptt,ndat2)
c     line opacities
      real*8 oplin(nnnl)

c     pointers to master data
      integer npnxt(ndat2)
      integer npfi(ntyp,nni),npar(ndat2)
      integer nplin(nnnl)

C     Internal work areas
      real rwrk1(nptmpdim)
      integer ntptr(nptmpdim)
      character(10) kion(nptmpdim)
      character(20) klevl(nptmpdim),klevu(nptmpdim),kblnk20
      integer tfields,varidat
      character(16) ttype(10),tform(10),tunit(10)
      integer colnum,frow,felem,hdutype,ll, ltyp
      integer lrtyp, lcon, nrdt, nidt, mm, lun11, lpril,lpri
      integer jkk, nlev, mlpar
      integer nlplmx,ln,lnn,ml,nilin2,nlpl,lmm,kltmpn,kltmpo,
     $         llo,lup,llofnd,lupfnd,nlines,nlsvn,
     $         k,kl2,kk,nilin,lm,mlm
      integer np1i,np1r,np1k
      real*8 eliml,elimh,elin,elmmtpp,elcomp
      real*8 rniss(nnml)
      real*8 aij,ergsev,etst,gglo,ggup,flin,ener
      integer idest1,idest2,ilevlo,ilevup,j,kl,ktt,lk
      real*8  rlev(10,nd)
      integer ilv(10,nd),nlpt(nd),iltp(nd)
      character(1) klev(100,nd)
      character(33) extname
      character(20) ktmp2,klablo,klabup
      character(1) kdat1(nkdat1)
      character(9) kinam1

C     Database manipulation quantities
      integer nkdt
      character(1) kblnk,kdtmp(200)
      integer kltmp(nptmpdim)
      real elsv(nptmpdim)
      logical done

      data kblnk/' '/
      data kblnk20/'                    '/
c
      data tform/'1J','1E','8A','20A','20A','1E','1E','1E',
     $ '1E','1E'/

      data ttype/'index','wavelength','ion',
     $ 'lower_level','upper_level','emis_inward',
     $ 'emis_outward','opacity','tau_in','tau_out'/

      data tunit/' ','A',' ',' ',' ','erg/cm^3/s',
     $ 'erg/cm^3/s','/cm',' ',' '/

      varidat=0
c

C     Move to the last HDU (hdunum) in the file
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr2: Moving to end-of-FITS file'
      call ftmahd(unit,hdunum,hdutype,status)
      if (status .gt. 0)call printerror(lun11,status)

C     append a new empty extension after the last HDU
      if (lpri.ne.0)
     $ write (lun11,*)'fstepr2: Create the new extension'
      call ftcrhd(unit,status)
      if (status .gt. 0)call printerror(lun11,status)

C----------------------------------------------------------------
C
C     Extracting data from the Atomic Database here
C
      if (lpri.ne.0)
     $ write (lun11,*)' '
c
c     print important lines
      if (lpri.ne.0)
     $ write (lun11,*)'emission line luminosities (erg/sec/10**38))',
     $                 nlsvn
         nlplmx=nptmpdim
c
c     step through lines
      nlpl=0
      do lnn=1,nlsvn
c
        if ((rcem(1,lnn).gt.1.d-64).or.
     $      (rcem(2,lnn).gt.1.d-64).or.
     $      (oplin(lnn).gt.1.d-64)) then
c
c         get line data
          ln=lnn
          ml=nplin(ln)
          mlm=ml-1
          call drd(ltyp,lrtyp,lcon,
     $      nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $      nptrs,0,lun11)
          if (lpri.ne.0) write (lun11,*)ln,ml,rdat1(np1r)
c
c         exclude rate type 14
          elin=abs(rdat1(np1r))
          if ((lrtyp.ne.14).and.(abs(elin).gt.0.1).and.(lrtyp.ne.9)
     $       .and.(abs(elin).lt.9.e+9)) then
c
            ergsev=1.602197e-12
            ener=ergsev*(12398.41)/max(elin,1.e-24)
            etst=ener/ergsev
            idest1=idat1(np1i)
            idest2=idat1(np1i+1)
            aij=rdat1(np1r+2)
            if (lpri.ne.0) write (lun11,*)'line data',elin,ener,etst,
     $                       idest1,idest2,aij
c
c           get ion data
            nilin=npar(ml)
            mlm=nilin-1
            call drd(ltyp,lrtyp,lcon,
     $        nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $        nptrs,0,lun11)
            do ktt=1,min(8,nkdt)
              write (kinam1(ktt:ktt),'(a1)')kdat1(np1k-1+ktt)
              enddo
            do ktt=nkdt+1,9
              write (kinam1(ktt:ktt),'(a1)')kblnk
              enddo
c
c           now find level data
            jkk=idat1(np1i+nidt-1)
            if (lpri.ne.0) write (lun11,*)'ion',kinam1,jkk
            call func2l(jkk,lpri,lun11,temp,xee,xpx,
     $              idat1,rdat1,kdat1,nptrs,
     $              npar,npnxt,npfi,
     $              rniss,rlev,ilv,
     $              nlpt,iltp,nlev,klev)

            ggup=rlev(2,idest1)
            gglo=rlev(2,idest2)
            do lk=1,20
              write (klablo(lk:lk),'(a1)')klev(lk,idest1)
              write (klabup(lk:lk),'(a1)')klev(lk,idest2)
              enddo
            flin=(1.e-16)*aij*ggup*elin*elin/((0.667274)*gglo)
            ilevlo=idest1
            ilevup=idest2
c
            nlpl=nlpl+1
            nlpl=min(nlpl,nlplmx)
            ntptr(nlpl)=lnn
            elsv(nlpl)=elin
            kion(nlpl)=kinam1
            klevl(nlpl)=klablo
            klevu(nlpl)=klabup
            j=ln
            if (lpri.ne.0) 
     $        write (lun11,9929)j,elin,kinam1,
     $        (klev(mm,ilevlo),mm=1,20),(klev(mm,ilevup),mm=1,20),
     $        rlev(1,ilevlo),rlev(1,ilevup),rlev(2,ilevlo),
     $        rlev(2,ilevup),rlev(3,ilevlo),rlev(3,ilevup),
     $        ilv(1,ilevlo),ilv(1,ilevup),ilv(2,ilevlo),ilv(2,ilevup),
     $        ilv(3,ilevlo),ilv(3,ilevup)
 9929       format (1h ,i9,1pe13.5,1x,a9,1x,2(20a1,1x),6(1pe13.5),
     $          6i6)
            if (lpri.ne.0) 
     $       write (lun11,*)j,elin,oplin(j),rcem(1,j),
     $                      rcem(2,j)
c
            endif
c
          endif
c
        enddo

c      if (nlpl.le.0) return
      nlpl=max(nlpl,1)
c

C     End of atomic database extraction
C----------------------------------------------------------------
C     define parameters for the binary table (see the above data statements)
      nrows=nlpl
      if (lpri.ne.0)
     $ write (lun11,*)'before header write'
      tfields=10
C     Build extension name
      extname='XSTAR_RADIAL'
      if(nloopctl.gt.0) then
          write(ktmp2,'(I4.4)')nloopctl
          extname='XSTAR_RADIAL_' // ktmp2
          endif

      if (lpri.ne.0)
     $ write (lun11,*)'fstepr2: Write table headers'
C     write the required header parameters for the binary table
      call ftphbn(unit,nrows,tfields,ttype,tform,tunit,extname,
     $              varidat,status)
      if (status .gt. 0)call printerror(lun11,status)

      if (lpri.ne.0)
     $ write (lun11,*)'fstepr2: Add some more keywords'

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

      rtmp=temp
      call ftpkye(unit,'TEMPERAT',rtmp,3,'[10**4K] Shell Temperature',
     $ status)
      if (status .gt. 0)call printerror(lun11,status)

      rtmp=pres
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


C     column  1  (Line number)
      colnum=1
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr2: Writing Column ',colnum,nlpl
      nlines=nlpl
      call ftpclj(unit,colnum,frow,felem,nlines,ntptr,status)
      if (status .gt. 0)call printerror(lun11,status)


C     column  2  (wavelength)
      colnum=2
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr2: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nlines,elsv,status)
      if (status .gt. 0)call printerror(lun11,status)
      if (status .gt. 0) return


C     column  3  (Ion)
      colnum=3
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr2: Writing Column ',colnum
      call ftpcls(unit,colnum,frow,felem,nlines,kion,status)
      if (status .gt. 0)call printerror(lun11,status)


C     column  4 (lower Level Designation)
      colnum=4
      if (lpri.ne.0)
     $ write (lun11,*)'fstepr2: Writing Column ',colnum
      call ftpcls(unit,colnum,frow,felem,nlines,klevl,status)
      if (status .gt. 0)call printerror(lun11,status)

C     column  5 (Level Designation)
      colnum=5
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr2: Writing Column ',colnum
      call ftpcls(unit,colnum,frow,felem,nlines,klevu,status)
      if (status .gt. 0)call printerror(lun11,status)

C----------------------------------------------------------------

C     column  6
      colnum=6
      do ll=1,nlines
         rwrk1(ll)=0.
         if (ntptr(ll).ne.0)
     $      rwrk1(ll)=rcem(1,ntptr(ll))
         if (lpri.ne.0) write (lun11,*)ll,ntptr(ll),rwrk1(ll)
         enddo
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr2: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nlines,rwrk1,status)
      if (status .gt. 0)call printerror(lun11,status)

C     column  7
      colnum=7
      do ll=1,nlines
         rwrk1(ll)=0.
         if (ntptr(ll).ne.0)
     $    rwrk1(ll)=rcem(2,ntptr(ll))
         enddo
      if (lpri.ne.0)
     $ write (lun11,*)'fstepr2: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nlines,rwrk1,status)
      if (status .gt. 0)call printerror(lun11,status)


C     column  8
      colnum=8
      do ll=1,nlines
         rwrk1(ll)=0.
         if (ntptr(ll).ne.0)
     $    rwrk1(ll)=oplin(ntptr(ll))
         enddo
      if (lpri.ne.0)
     $ write (lun11,*)'fstepr2: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nlines,rwrk1,status)
      if (status .gt. 0)call printerror(lun11,status)


C     column  9
      colnum=9
      do ll=1,nlines
         rwrk1(ll)=0.
         if (ntptr(ll).ne.0)
     $    rwrk1(ll)=tau0(1,ntptr(ll))
         enddo
      if (lpri.ne.0)
     $ write (lun11,*)'fstepr2: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nlines,rwrk1,status)
      if (status .gt. 0)call printerror(lun11,status)

C     column  10
      colnum=10
      do ll=1,nlines
         rwrk1(ll)=0.
         if (ntptr(ll).ne.0)
     $    rwrk1(ll)=tau0(2,ntptr(ll))
         enddo
      if (lpri.ne.0)
     $ write (lun11,*)'fstepr2: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nlines,rwrk1,status)
      if (status .gt. 0)call printerror(lun11,status)


c----------------------------------------------------------------
C     Compute checksums
      call ftpcks(unit,status)
      if (status .gt. 0)call printerror(lun11,status)

c
c
      return
      end
