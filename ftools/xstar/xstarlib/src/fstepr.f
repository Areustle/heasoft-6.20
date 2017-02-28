      subroutine fstepr(unit,hdunum,radin,radout,rdel,t,pres,
     $                xcol,xee,xpx,xi,
     $                idat1,rdat1,kdat1,nptrs,npnxt,npfi,
     $                npfirst,npar,npilev,
     $                xilev,bilev,rniss,nloopctl,
     $                lun11,lpri,status)
C
C
C     Write Data for each radial zone to an individual extension
c     author: T. Bridgman
C
C     Append a FITS extension binary table containing
C     nrhs columns and at most nrhdimj rows
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
c
C     Allocation for passed parameters
      real*8 xilev(nnml),bilev(nnml),rniss(nnml)
      real*8 rdat1(nrdat1)
      real*8 radin, radout,rdel, t, pres, xcol,xee,xpx,xi
      real rtmp
      integer unit,hdunum, nrows, status, nloopctl
      integer idat1(nidat1),nptrs(nptt,ndat2)

c     pointers to master data
      integer npnxt(ndat2),npfirst(ntyp)
      integer npfi(ntyp,nni),npar(ndat2)
      integer npilev(nd,nni)
C     Internal work areas
      real rwrk1(nnml),rwrk2(nnml), elev(nnml)
      integer ntptr(nnml)
      integer natomic(nnml), mllev(nnml),nupper(nnml)
      character(10) kion(nnml)
      character(20) klevt(nnml)
      integer tfields,varidat
      character(16) ttype(9),tform(9),tunit(9)
      integer colnum,frow,felem,hdutype, klel, mlel, jk, ltyp
      integer lrtyp, lcon, nrdt, nidt, mmlv, mm, lun11, lpril,lpri
      integer mllel, klion, mlion, jkk, kl
      integer mt2, mlleltp, nnz, nions
      character(43) extname
      character(30) ktmp2
      character(1) kdat1(nkdat1)
C     Database manipulation quantities
      real*8  xeltp
      integer  nkdt
      character(1) klev(100,nd)
      real*8 rnissl(nnml)
      integer j,nkdti,np1ki
      real*8 rlev(10,nd)
      integer ilev(10,nd),nlpt(nd),iltp(nd),nlev
      integer mm2,mmtmp,kkkl,lk,mlm
      integer np1i,np1r,np1k
      real*8 eth
      character(10) kdtmp

      data tform/'1J','1I','1E','8A','1I','20A','1E','1E','1I'/
      data ttype/'index','ion_index','e_excitation','ion',
     $ 'atomic_number','ion_level','population','lte',
     $  'upper index'/
      data tunit/' ',' ','eV',' ',' ',' ',' ',' ',' '/
c
      lpril=lpri
      varidat=0
c
      status=0
c
C     Move to the last HDU (hdunum) in the file
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Moving to end-of-FITS file'
      call ftmahd(unit,hdunum,hdutype,status)
      if (status .gt. 0)call printerror(lun11,status)
c
c
C     append a new empty extension after the last HDU
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Create the new extension'
      call ftcrhd(unit,status)
      if (status .gt. 0)call printerror(lun11,status)
C
C     Extracting data from the Atomic Database here
c
C
C     lpril is flag for printing debug information
       nions=0
      if (lpril.ne.0) then
        write (lun11,*)'raw data'
        do j=1,nnml
          if (xilev(j).gt.1.e-37)
     $     write (lun11,*)j,xilev(j)
          enddo
        endif
c
C     initialize line counter
      mmlv=0
C     First look for element data (jk is element index)
      klel=11
      mlel=npfirst(klel)
      jkk=0
      jk=0
      do while (mlel.ne.0)
c
c       get element data
       jk=jk+1
        mt2=mlel-1
        call drd(ltyp,lrtyp,lcon,
     $     nrdt,np1r,nidt,np1i,nkdt,np1k,mt2,
     $     nptrs,0,lun11)
        mllel=idat1(np1i+nidt-1)
        xeltp=rdat1(np1r)
        nnz=idat1(np1i)
        if (lpril.ne.0)
     $    write (lun11,*)'element:',jk,mlel,mllel,nnz,
     $                  (kdat1(np1k-1+mm),mm=1,nkdt),xeltp
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
c
              jkk=jkk+1
C             retrieve ion name from kdati
              mlm=mlion-1
              call drd(ltyp,lrtyp,lcon,
     $            nrdt,np1r,nidt,np1i,nkdti,np1ki,mlm,
     $            nptrs,0,lun11)

C             if not accessing the same element, skip to the next element
              mlleltp=idat1(np1i+nidt-2)
              if (mlleltp.eq.mllel) then
c
                kl=kl+1
                if (lpril.ne.0)
     $              write (lun11,*)'  ion:',kl,jkk,mlion,mlleltp,
     $                          (kdat1(np1ki-1+mm),mm=1,nkdti)
c
c               get level data
                call func2l(jkk,lpril,lun11,t,xee,xpx,
     $              idat1,rdat1,kdat1,nptrs,
     $              npar,npnxt,npfi,
     $              rnissl,rlev,ilev,
     $              nlpt,iltp,nlev,klev)
c
c               step thru levels
                do mm2=1,nlev
c
c                 get level pointer
                  mmtmp=npilev(mm2,jkk)
                  if (mmtmp.ne.0) then
                    kkkl=mmtmp
                    mmlv=mmtmp
c
c                   test for level pop
                    if (xilev(kkkl).gt.1.d-34) then
c
c                     get data
                      eth=rlev(1,mm2)
                      nions=nions+1
                      mllev(nions)=idat1(np1i+nidt-2)
C                     Note that rwrk1 must be written to the file before
C                     it is overwritten in subsequent columns
                      rwrk1(nions)=xilev(mmlv)
                      rwrk2(nions)=rniss(mmlv)
                      elev(nions)=eth
                      ntptr(nions)=kkkl
                      natomic(nions)=nnz
                      nupper(nions)=mm2
                      do mm=1,nkdti
                        write (kdtmp(mm:mm),'(a1)')kdat1(np1ki-1+mm)
                        enddo
                      do mm=nkdti+1,9
                        write (kdtmp(mm:mm),'(a1)')' '
                        enddo
                      kion(nions)=kdtmp
                      write(klevt(nions),'(20a1)')
     $                        (klev(mm,mm2),mm=1,20)
                      if (lpri.ne.0) then
                        write (lun11,*)nions,xilev(mmlv),
     $                         rdat1(np1r),nnz,mmlv,kkkl
                        write (lun11,9296)kkkl,
     $                      (kdat1(np1i-1+mm),mm=1,20),
     $                      (klev(lk,mm2),lk=1,20),eth,xilev(kkkl),
     $                      rniss(kkkl),bilev(kkkl)
 9296                   format (1x,i6,1x,(40a1),7(1pe13.5))
                        endif
c
c                     end of test for level pop
                      endif
c
c                   end of test for level pointer
                    endif
c
c                 end of step thru levels
                  enddo
c
c               end of test for element
                endif
c
C             Go to next ion
              mlion=npnxt(mlion)
              enddo
c
C           end of test for abundance 
            endif
c
        mlel=npnxt(mlel)
C       Go to next element
        enddo

c

C     End of atomic database extraction
C----------------------------------------------------------------
C     define parameters for the binary table (see the above data statements)
      nrows=nions
      tfields=9
C     Build extension name
      extname='XSTAR_RADIAL'
      if(nloopctl.gt.0) then
          write(ktmp2,'(I4.4)')nloopctl
          extname='XSTAR_RADIAL_' // ktmp2
          endif

      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Write table headers'
C     write the required header parameters for the binary table
      call ftphbn(unit,nrows,tfields,ttype,tform,tunit,extname,
     $              varidat,status)
      if (status .gt. 0)call printerror(lun11,status)

      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Add some more keywords'

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

C-------------------------------------------------------------------
C     Step through the columns and write them to the file
C
C     set 'global' parameters for writing FITS columns
      frow=1
      felem=1

C     column  1  (Line number)
      colnum=1
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Writing Column ',colnum
      call ftpclj(unit,colnum,frow,felem,nions,ntptr,status)
      if (status .gt. 0)call printerror(lun11,status)

C     column  2 (Level number of this ion)
      colnum=2
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Writing Column ',colnum
      call ftpclj(unit,colnum,frow,felem,nions,mllev,status)
      if (status .gt. 0)call printerror(lun11,status)

C     column  3  (Energy)
      colnum=3
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nions,elev,status)
      if (status .gt. 0)call printerror(lun11,status)


C     column  4  (Ion)
      colnum=4
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Writing Column ',colnum
      call ftpcls(unit,colnum,frow,felem,nions,kion,status)
      if (status .gt. 0)call printerror(lun11,status)


C     column  5  (Atomic Number)
      colnum=5
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Writing Column ',colnum
      call ftpclj(unit,colnum,frow,felem,nions,natomic,status)
      if (status .gt. 0)call printerror(lun11,status)


C     column  6 (Level Designation)
      colnum=6
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Writing Column ',colnum
      call ftpcls(unit,colnum,frow,felem,nions,klevt,status)
      if (status .gt. 0)call printerror(lun11,status)

C----------------------------------------------------------------
C     column 7 (Level population)
C     rwrk1 can be safely overwritten after this step

      colnum=7
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nions,rwrk1,status)
      if (status .gt. 0)call printerror(lun11,status)


      colnum=8
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nions,rwrk2,status)
      if (status .gt. 0)call printerror(lun11,status)


C     column  9 (upper level index)
      colnum=9
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Writing Column ',colnum
      call ftpclj(unit,colnum,frow,felem,nions,nupper,status)
      if (status .gt. 0)call printerror(lun11,status)

c----------------------------------------------------------------
C     Compute checksums
      call ftpcks(unit,status)
      if (status .gt. 0)call printerror(lun11,status)

      return
      end
