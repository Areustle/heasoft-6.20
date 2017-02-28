      subroutine rread1(trad,xlum,lwri,lpri,r,t,xpx,p,lcdd,numrec,npass,
     $ nlimd,rmax,xpxcol,xi,zeta,lfix,
     $ lun11,abel,cfrac,emult,taumax,xeemin,spectype,specfile,specunit,
     $ kmodelname,nloopctl,critf,vturbi,eptmp,zrtmp,numcon2,ncn2,radexp)
c
c     this routine handles reading of the input data.
c     author:  T. Kallman
c
      implicit none
c
      include './PARAM'
c
      real*8 eptmp(ncn),zrtmp(ncn),abel(nl),abel2(30)
      character(8) stringst,kblnk8
      character(80) specfile,spectype,stringsl,kblnk80,stringst2
      character(30) kmodelname
      integer nloopctl,specunit,ierr,ll,lcdd2,lun13,nenergy,ncn2
      integer lwri,lpri,lcdd,numrec,npass,nlimd,lfix,lun11,numcon2,mm
      real*8 trad,xlum,r,t,xpx,p,rmax,xpxcol,xi,zeta,cfrac,emult,taumax,
     $     xeemin,critf,vturbi,ccc,xlum2,xpcol,r19,radexp
c
      data kblnk8/'        '/
      data kblnk80/
     $'                                                                 
     $               '/ 
c
            ierr=0
            call uclgsr8('cfrac',cfrac,ierr)
c
c           temperature
            call uclgsr8('temperature',t,ierr)
c
c           pressure/density switch
            call uclgsi('lcpres',lcdd2,ierr)
            lcdd=1-lcdd2
c
c           pressure
            call uclgsr8('pressure',p,ierr)
c
c           density
            call uclgsr8('density',xpx,ierr)
c
c           spectrum
            specfile=kblnk80
            spectype=kblnk80
            stringst2=kblnk80
            specunit=0
            stringst=kblnk8
            call uclgst('spectrum',spectype,ierr)
            xlum=1.
            read (spectype(1:8),'(a8)')stringst
            if (stringst.eq.'file    ') then
              specfile=kblnk80
              call uclgst('spectrum_file',specfile,ierr)
              read (specfile(1:80),'(a80)')stringst2
              call getlunx(lun13)
              open (unit=lun13,file=stringst2,status='unknown')
              call uclgsi('spectun',specunit,ierr)
              read (lun13,*)nenergy
              numcon2 = nenergy              
              do ll=1,nenergy
                read (lun13,*)eptmp(ll),zrtmp(ll)
                if (specunit.eq.1) zrtmp(ll)=zrtmp(ll)*eptmp(ll)
                if (specunit.eq.2) zrtmp(ll)=10.**zrtmp(ll)
                enddo
              endif
c
c           trad
            call uclgsr8('trad',trad,ierr)
c
c           luminosity
            call uclgsr8('rlrad38',xlum,ierr)
c
c           column density
            call uclgsr8('column',xpcol,ierr)
c
c           ionization parameter
            call uclgsr8('rlogxi',zeta,ierr)
c
c           number of steps
            call uclgsi('nsteps',numrec,ierr)
c
c           number of iterations
            call uclgsi('niter',nlimd,ierr)
c
c           write switch
            call uclgsi('lwrite',lwri,ierr)
c
c           print switch
            call uclgsi('lprint',lpri,ierr)
c
c           step size choice
            call uclgsi('lstep',lfix,ierr)
c
c           abundances
            call uclgsr8('habund',abel2(1),ierr)
            call uclgsr8('heabund',abel2(2),ierr)
            call uclgsr8('liabund',abel2(3),ierr)
            call uclgsr8('beabund',abel2(4),ierr)
            call uclgsr8('babund',abel2(5),ierr)
            call uclgsr8('cabund',abel2(6),ierr)
            call uclgsr8('nabund',abel2(7),ierr)
            call uclgsr8('oabund',abel2(8),ierr)
            call uclgsr8('fabund',abel2(9),ierr)
            call uclgsr8('neabund',abel2(10),ierr)
            call uclgsr8('naabund',abel2(11),ierr)
            call uclgsr8('mgabund',abel2(12),ierr)
            call uclgsr8('alabund',abel2(13),ierr)
            call uclgsr8('siabund',abel2(14),ierr)
            call uclgsr8('pabund',abel2(15),ierr)
            call uclgsr8('sabund',abel2(16),ierr)
            call uclgsr8('clabund',abel2(17),ierr)
            call uclgsr8('arabund',abel2(18),ierr)
            call uclgsr8('kabund',abel2(19),ierr)
            call uclgsr8('caabund',abel2(20),ierr)
            call uclgsr8('scabund',abel2(21),ierr)
            call uclgsr8('tiabund',abel2(22),ierr)
            call uclgsr8('vabund',abel2(23),ierr)
            call uclgsr8('crabund',abel2(24),ierr)
            call uclgsr8('mnabund',abel2(25),ierr)
            call uclgsr8('feabund',abel2(26),ierr)
            call uclgsr8('coabund',abel2(27),ierr)
            call uclgsr8('niabund',abel2(28),ierr)
            call uclgsr8('cuabund',abel2(29),ierr)
            call uclgsr8('znabund',abel2(30),ierr)
            do mm=1,nl
              abel(mm)=abel2(mm)
              enddo
c
c
            call uclgsi('npass',npass,ierr)
C           Test if npass is even.  If it is, change it to next lowest odd
c            if(mod(npass,2) .eq. 0) then
c              write(lun11,*)'rread1: npass should always be odd.'
c              write(lun11,*)'rread1: resetting to ',npass-1
c              npass=npass-1
c            endif

            stringsl=kblnk80
            call uclgst('modelname',stringsl,ierr)
            read (stringsl(1:30),'(a30)')kmodelname
c
c
c           step parameters
            call uclgsr8('emult',emult,ierr)
            call uclgsr8('taumax',taumax,ierr)
c
c           min xee
            call uclgsr8('xeemin',xeemin,ierr)
c
c           critf
            call uclgsr8('critf',critf,ierr)
c
c           vturbi
            call uclgsr8('vturbi',vturbi,ierr)
c
c           ncn2
            call uclgsi('ncn2',ncn2,ierr)
            ncn2=max(999,min(999999,ncn2))
            if (ierr.ne.0) ncn2=9999
c
c           radexp
            call uclgsr8('radexp',radexp,ierr)
            if (ierr.ne.0) radexp=0.
c
            call uclgsi('loopcontrol',nloopctl,ierr)
c
c
            ccc = 3.e+10
            xlum2=xlum
            xpxcol=xpcol
            xi=10.**zeta
            if (lcdd.ne.1) then
               xpx = p/1.38e-12/max(t,1.e-24)
               r19 = sqrt(xlum2/12.56/ccc/max(1.e-24,p*xi))
            else
               r19 = sqrt(xlum2/max(1.e-24,xpx*xi))
            endif
            rmax = xpxcol/(max(xpx,1.e-36))
            r = r19*(1.e+19)
c
      return
      end
