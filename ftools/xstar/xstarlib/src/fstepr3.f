      subroutine fstepr3(unit,hdunum,radin,radout,rdel,t,prs,abel,
     $             xcol,xee,xpx,xi,
     $             idat1,rdat1,kdat1,nptrs,npnxt,npfi,npar,
     $             npfirst,npilev,npconi2,ncsvn,
     $             rniss,cemab,cabab,opakab,tauc,nloopctl,
     $             lun11,lpri,status)
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
      integer mllz,mllz2

      integer nptmpdim
      parameter (nptmpdim=500000)
c
C     Allocation for passed parameters
      real*8 rdat1(nrdat1)
      real rtmp
      real*8 radin, radout,rdel, t, prs, xcol,xee,xpx,xi
      integer unit,hdunum, nrows, status, nloopctl
      integer idat1(nidat1),nptrs(nptt,ndat2)
c     line opacities
      real*8 oplin(nnnl)
      real*8 abel(nl)
      real*8 tauc(2,nnml)
      real*8 cemab(2,nnml),opakab(nnml),cabab(nnml)

c     pointers to master data
      integer npnxt(ndat2),npfirst(ntyp)
      integer npfi(ntyp,nni),npar(ndat2)
      integer npilev(nd,nni)
      integer nplin(nnnl)
      integer npcon(nnml),npconi2(ndat2),npconi(ndat2)

C     Internal work areas
      real*8 rniss(nnml)
      real*8 rlev(10,nd)
      integer ilev(10,nd),nlpt(nd),iltp(nd),nlev
      real rwrk1(nptmpdim),rwrk2(nptmpdim),rwrk3(nptmpdim),
     $  rwrk4(nptmpdim),rwrk5(nptmpdim),
     $  rwrk6(nptmpdim),rwrk7(nptmpdim),rwrk8(nptmpdim)
      integer ntptr(nptmpdim),ntptr2(nptmpdim)
      character(20) klevl(nptmpdim),klevu(nptmpdim),kblnk20
      character(1) klev(100,nd)
      integer tfields,varidat
      character(16) ttype(12),tform(12),tunit(12)
      integer colnum,frow,felem,hdutype,ll, ltyp
      integer lrtyp, lcon, nrdt, nidt, mm, lun11, lpril,lpri
      integer jkk,nidti
      integer nlplmx,ln,lnn,ml,nilin2,nlpl,lmm,kltmpn,kltmpo,
     $         llo,lup,llofnd,lupfnd,nlines,nlsvn,
     $         k,kl2,kk,nilin,lm,mlm,kkkl,idest1,idest2,lk,
     $         mlel,mlion,mllel,mlleltp,mlpar,mt2,mltype,nkdti,
     $         jk,kl,klel,klion,ncsvn,nlevmx,nnz,np1ki,mmlv
      integer np1i,np1r,np1k
      real*8 eliml,elimh,elin,elmmtpp,elcomp,eth,xeltp
      character(33) extname
      character(20) ktmp20
      character(8) ktmp8,kion(nptmpdim)
      character(20) ktmp2
      character(1) kdat1(nkdat1)
c     needed for upper level search
      integer jkk3,nlevp,ndtmp,iltmp,lcon2,lrtyp2,ltyp2,
     $         np1r2,nrdt2,np1i2,nidt2,np1k2,nkdt2
      real*8 ett

C     Database manipulation quantities
      integer nkdt
      character(1) kblnk,kdtmp(200)
      integer kltmp(50000),ilevlo(50000),ilevup(50000)
      real elsv(50000)
      logical done

      data kblnk/' '/
      data kblnk20/'                    '/
c
      data tform/'1J','1J','1E','8A','20A','20A','1E','1E','1E',
     $ '1E','1E','1E'/

      data ttype/'rrc index','level index','energy','ion',
     $ 'lower_level','upper_level','emis_inward',
     $ 'emis_outward','integrated absn','opacity',
     $  'tau_in','tau_out'/

      data tunit/' ',' ','ev',' ',' ',' ','erg/cm^3/s',
     $ 'erg/cm^3/s','erg/cm^3/s','/cm',' ',' '/

      varidat=0
c
c

C     Move to the last HDU (hdunum) in the file
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr3: Moving to end-of-FITS file'
      call ftmahd(unit,hdunum,hdutype,status)
      if (status .gt. 0)call printerror(lun11,status)

C     append a new empty extension after the last HDU
      if (lpri.ne.0)
     $ write (lun11,*)'fstepr3: Create the new extension'
      call ftcrhd(unit,status)
      if (status .gt. 0)call printerror(lun11,status)

C----------------------------------------------------------------
C
C     Extracting data from the Atomic Database here
C
      if (lpri.ne.0)
     $ write (lun11,*)'in fstepr3 '
      kltmpo=0
c
C     First look for element data (jk is element index)        
      klel=11
      mlel=npfirst(klel)
      jk=0
      kk=0
      jkk=0
c
c     step through elements
      do while (mlel.ne.0)
c
c       get element data
        jk=jk+1
        mt2=mlel-1
        call drd(ltyp,lrtyp,lcon,
     $        nrdt,np1r,nidt,np1i,nkdt,np1k,mt2,
     $        nptrs,0,lun11)
        mllel=idat1(np1i+nidt-1)
        xeltp=rdat1(np1r)
        xeltp=abel(mllel)
        nnz=idat1(np1i)
        if (lpri.ge.1)
     $        write (lun11,*)'element:',jk,mlel,mllel,nnz,
     $                  (kdat1(np1k-1+mm),mm=1,nkdt)
c
C       ignore if the abundance is small
        if (xeltp.lt.1.e-10) then
            jkk=jkk+nnz
          else
c
c           now step thru ions (jkk is ion index)
            klion=12
            mlion=npfirst(klion)
            jkk=0
            kl=0
            do while ((mlion.ne.0).and.(kl.lt.nnz))
              jkk=jkk+1
c
C             retrieve ion name from kdati
              mlm=mlion-1
              call drd(ltyp,lrtyp,lcon,
     $            nrdt,np1r,nidti,np1i,nkdti,np1ki,mlm,
     $            nptrs,0,lun11)
c
C             if not accessing the same element, skip to the next element
              mlleltp=idat1(np1i+nidti-2)
              if (mlleltp.eq.mllel) then
c
                kl=kl+1
                if (lpri.ge.1)
     $            write (lun11,*)'  ion:',kl,jkk,mlion,mlleltp,
     $                        (kdat1(np1ki+mm-1),mm=1,nkdti)
c
c               now find level data
                call func2l(jkk,lpri,lun11,t,xee,xpx,
     $              idat1,rdat1,kdat1,nptrs,
     $              npar,npnxt,npfi,
     $              rniss,rlev,ilev,
     $              nlpt,iltp,nlev,klev)
c
c               now step through rate type 7 data
                mltype=7
                ml=npfi(mltype,jkk)
                mllz=0
                if (ml.ne.0) mllz=npar(ml)
                mlpar=0
                if (ml.ne.0) mlpar=npar(ml)
                do while ((ml.ne.0).and.(mlpar.eq.mllz))
c
c                 get rrc data
                  kkkl=npconi2(ml)
                  if (lpri.ne.0) write (lun11,*)kkkl,ml,idest1,
     $                    cemab(1,kkkl),cemab(2,kkkl)
c
c                 test for non-zero rrc data
                  if ((kkkl.gt.0).and.(kkkl.le.ndat2)
     $                .and.((cemab(1,kkkl).gt.1.e-36)
     $                .or.(cemab(2,kkkl).gt.1.e-36)
     $                .or.(cabab(kkkl).gt.1.e-36)
     $                .or.(opakab(kkkl).gt.1.e-36))) then
c
c
c                   increment buffer counter
                    kk=kk+1
                    if (kk.ge.nptmpdim) then
                      write (lun11,*)'buffer overflow in fstepr33'
                      kk=nptmpdim
                      endif                      
c
c                   get rrc  data
                    mlm=ml-1
                    call drd(ltyp,lrtyp,lcon,
     $                nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $                nptrs,0,lun11)
                    idest1=idat1(np1i+nidt-2)
                    nlevp=nlev
                    idest2=nlevp+idat1(np1i-1+nidt-3)-1
c
c                   label for lower level
                    do lk=1,20
                      write (ktmp20(lk:lk),'(a1)')klev(lk,idest1)
                      enddo
                    klevl(kk)=ktmp20
c
c                   label for upper level
                    write (ktmp20(1:20),'(a20)')'continuum           '
                    klevu(kk)=ktmp20
c
c                   ion label
                    do lk=1,nkdti
                      write (ktmp8(lk:lk),'(a1)')kdat1(np1ki+lk-1)
                      enddo
                    do lk=nkdti+1,8
                      write (ktmp8(lk:lk),'(a1)')kblnk
                      enddo
c
                    eth=rlev(4,idest1)-rlev(1,idest1)
                    ett=eth
c
c                   get upper level data
                    if (idest2.gt.nlevp) then
                      jkk3=jkk+1
                      if (lpri.gt.1)
     $                  write (lun11,*)jkk3,ndtmp,nlevp,idest2
                      ndtmp=npfi(13,jkk3)
                      if (lpri.gt.1)
     $                  write (lun11,*)jkk3,ndtmp,nlevp,idest2
                      if (ndtmp.le.0) stop 'ndtmp error'
                      mllz2=npar(ndtmp)
                      iltmp=0
                      do while ((ndtmp.ne.0).and.
     $                    (iltmp.ne.(idest2-nlevp+1)).and.
     $                    (npar(ndtmp).eq.mllz2)) 
                        mlm=ndtmp-1
                        call drd(ltyp2,lrtyp2,lcon2,
     $                    nrdt2,np1r2,nidt2,np1i2,nkdt2,np1k2,mlm,
     $                    nptrs,0,lun11)
                        iltmp=idat1(np1i2+nidt2-2)
                        if (lpri.gt.1) write (lun11,*)nidt2,iltmp,ndtmp
                        ndtmp=npnxt(ndtmp)     
                        if (ndtmp.le.0) stop 'ndtmp error'           
                        enddo
c                     NB fix to excited level PI and rec
                      ett=ett+rdat1(np1r2)
                      eth=ett
                      if (lpri.gt.1)
     $                  write (lun11,*) ndtmp,iltmp,idest2,ett
c                     label for lower level
                      ktmp20=kblnk20
                      do lk=1,nkdt2
                        write (ktmp20(lk:lk),'(a1)')kdat1(np1k2+lk-1)
                        enddo
                      klevu(kk)=ktmp20
                      endif
c
c                   other data
                    kion(kk)=ktmp8
                    elsv(kk)=eth
                    ilevlo(kk)=idest1
                    ilevup(kk)=idest2
                    ntptr(kk)=kkkl
                    mmlv=npilev(idest1,jkk)
                    ntptr2(kk)=mmlv
                    if (lpri.ge.1)
     $                  write (lun11,981)kkkl,eth,idest1,
     $                    cemab(1,kkkl),cemab(2,kkkl)
 981                  format (1x,i6,1pe11.3,i6,6(1pe11.3))
c
c                   done with this rrc
                    endif
c
c                 end of loop over rrcs
                  ml=npnxt(ml)
                  if (ml.ne.0) mlpar=npar(ml)
                  enddo
c
c               end of test for element
                endif
c
C             Go to next ion
              mlion=npnxt(mlion)
              enddo
c
c         end of test for non-zero element abund
          endif
c
        mlel=npnxt(mlel)
C       Go to next element
        enddo
c
      nlpl=max(nlpl,1)
c

C     End of atomic database extraction
C----------------------------------------------------------------
C     define parameters for the binary table (see the above data statements)
      nrows=kk
      if (lpri.ne.0)
     $ write (lun11,*)'before header write'
      tfields=12
C     Build extension name
      extname='XSTAR_RADIAL'
      if(nloopctl.gt.0) then
          write(ktmp2,'(I4.4)')nloopctl
          extname='XSTAR_RADIAL_' // ktmp2
          endif

      if (lpri.ne.0)
     $ write (lun11,*)'fstepr3: Write table headers'
C     write the required header parameters for the binary table
      call ftphbn(unit,nrows,tfields,ttype,tform,tunit,extname,
     $              varidat,status)
      if (status .gt. 0)call printerror(lun11,status)

      if (lpri.ne.0)
     $ write (lun11,*)'fstepr3: Add some more keywords'

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


C     column  1  (continuum index)
      colnum=1
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr3: Writing Column ',colnum,nlpl
      nlines=kk
      call ftpclj(unit,colnum,frow,felem,nlines,ntptr,status)
      if (status .gt. 0)call printerror(lun11,status)


C     column  2  (level index)
      colnum=2
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr3: Writing Column ',colnum,nlpl
      nlines=kk
      call ftpclj(unit,colnum,frow,felem,nlines,ntptr2,status)
      if (status .gt. 0)call printerror(lun11,status)


C     column  3  (wavelength)
      colnum=3
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr3: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nlines,elsv,status)
      if (status .gt. 0)call printerror(lun11,status)
      if (status .gt. 0) return


C     column  4  (Ion)
      colnum=4
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr3: Writing Column ',colnum
      call ftpcls(unit,colnum,frow,felem,nlines,kion,status)
      if (status .gt. 0)call printerror(lun11,status)


C     column  5 (lower Level Designation)
      colnum=5
      if (lpri.ne.0)
     $ write (lun11,*)'fstepr3: Writing Column ',colnum
      call ftpcls(unit,colnum,frow,felem,nlines,klevl,status)
      if (status .gt. 0)call printerror(lun11,status)

C     column  6 (Level Designation)
      colnum=6
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr3: Writing Column ',colnum
      call ftpcls(unit,colnum,frow,felem,nlines,klevu,status)
      if (status .gt. 0)call printerror(lun11,status)

C----------------------------------------------------------------

      do ll=1,nlines
         rwrk1(ll)=0.
         if (ntptr(ll).ne.0) then
           rwrk3(ll)=cemab(1,ntptr(ll))
           rwrk4(ll)=cemab(2,ntptr(ll))
           rwrk5(ll)=cabab(ntptr(ll))
           rwrk6(ll)=opakab(ntptr(ll))
           rwrk7(ll)=tauc(1,ntptr(ll))
           rwrk8(ll)=tauc(2,ntptr(ll))
           endif
         enddo
c
c
C     column  7
      colnum=7
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr3: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nlines,rwrk3,status)
      if (status .gt. 0)call printerror(lun11,status)

c
C     column  8
      colnum=8
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr3: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nlines,rwrk4,status)
      if (status .gt. 0)call printerror(lun11,status)

c
C     column  9
      colnum=9
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr3: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nlines,rwrk5,status)
      if (status .gt. 0)call printerror(lun11,status)

c
C     column  10
      colnum=10
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr3: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nlines,rwrk6,status)
      if (status .gt. 0)call printerror(lun11,status)

c
C     column  11
      colnum=11
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr3: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nlines,rwrk7,status)
      if (status .gt. 0)call printerror(lun11,status)

C     column  12
      colnum=12
      if (lpri.ne.0)
     $ write (lun11,*)'fstepr3: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nlines,rwrk8,status)
      if (status .gt. 0)call printerror(lun11,status)



c----------------------------------------------------------------
C     Compute checksums
      call ftpcks(unit,status)
      if (status .gt. 0)call printerror(lun11,status)

      return
      end
