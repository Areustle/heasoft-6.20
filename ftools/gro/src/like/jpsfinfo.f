c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C      SUBROUTINE JPSFINFO(tinput)
C	
C
C  $Id: jpsfinfo.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C
C	EFFECT: prints parameter values for PSF(i) to console
c
c	returned value: none
c
c	Programmer: J.A.Esposito delivered 25 AUG 1995
c
c	Parameters:
c
c	Input:	char*260 tinput		will contain 'Innn' or 'I nnn'
c					where nnn is a number 1 thru 
c					500; Error on nnn outside 
c					given range - re-input of nnn
c					allowed
c
c	Local:  char*50  sinput		Contains and holds value in 
c					tinput(1:50)
c		int	 npsf		contains PSF number nnn
c
c	all other parameters are either Common block or local dummy vars
c	for use in IO
c
c
C  $Log: jpsfinfo.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:35  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.4  1996/10/31  16:23:54  jae
c Repaired errors associated with the input
c variable.
c
c Revision 5.3  1996/08/26  21:44:02  jae
c Repaired several errors which prevented
c using the P or N commands.  Also, these
c errors prevented Innn from being read
c properly.  These fixes will hopefully solve
c the problems.
c
c Revision 5.2  1996/07/18  16:24:45  jae
c Added more definitions
c
c Revision 5.1  1996/02/29  20:48:28  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:54:29  jae
c Subroutine Module for like V5.00
c
c
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
      SUBROUTINE JPSFINFO(tinput)

c     Common blocks included
c     
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/roirep.copy'
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/gasrep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/bmprep.copy'
      INCLUDE  '../COMMON/tmprep.copy'
      INCLUDE  '../COMMON/psfrep.copy'
      INCLUDE  '../COMMON/psmrep.copy'
      INCLUDE  '../COMMON/xrrrep.copy'
      INCLUDE  '../COMMON/fitrep.copy'
      INCLUDE  '../COMMON/locpos.copy'

      save
c
      character(80) id
      common /id/id
      character tinput*260,sinput*10

      id = '$Id: jpsfinfo.f,v 1.2 2013/05/21 19:08:26 irby Exp $'
      LOC='JPSFINFO'

      if(jae_jpsfinfo)print *,'Inside ',LOC

c     m=1
      numcar=50
 11   continue
      sinput=tinput(1:10)
      do i=1,10
         call to_upper(sinput(i:i))
         if(sinput(i:i).lt.'0'.or.sinput(i:i).gt.'9')
     &        sinput(i:i)=' '
      enddo
      read(sinput(1:10),*,err=112,end=112)npsf
      goto 12
c     do i=1,numcar
c     read(tinput(i:numcar),*,err=111,end=112)npsf
c     goto 12
c111  tinput(i:i)=' '
c     enddo
 112  print *,'ERROR: Could not read input of PSF #'
      print *,'your last input->',sinput(1:10)
      write(*,'("Input PSF number (return to exit):")$')
      read(LU(12),'(a)') tinput
      numcar = index(tinput, ' ') - 1
      if(numcar.eq.0)return
 115  do i=1,10
         call to_upper(tinput(1:1))
      enddo
      if(tinput(1:1).eq.'N')then
         if(npsf.gt.0.and.npsf.lt.NSOURCE)then
            tinput(1:10)='          '
            write(tinput,*)npsf+1
         elseif(npsf.ge.NSOURCE)then
            tinput(1:10)='          '
            write(tinput,*)NSOURCE
         elseif(npsf.le.0)then
            tinput(1:10)='          '
            npsf=1
            write(tinput,*)npsf
         endif
      endif
      if(tinput(1:1).eq.'P')then
         if(npsf.gt.1.and.npsf.le.NSOURCE)then
            tinput(1:10)='          '
            write(tinput,*)npsf-1
         elseif(npsf.gt.NSOURCE)then
            tinput(1:10)='          '
            write(tinput,*)NSOURCE
         elseif(npsf.le.1)then
            tinput(1:10)='          '
            npsf=1
            write(tinput,*)npsf
         endif
      endif
      goto 11
 12   continue
c     m=1
      if(npsf.gt.NSOURCE.or.npsf.lt.1)goto 998
      print *,' '
      print *,'Report on PSF ',npsf,' Named:',SRC_NAMES(npsf)
      print *,' '
      if(coord_sys.eq.'C')then
         print *,
     &        'The coordinate system is celestial RA, DEC'
      else
         print *,
     &        'The coordinate system is galactic Longitude, Latitude'
      endif
      if(sv_dstsv(6,npsf))then
         print *,' '
         print *,'This source has been identified'
         print *,'ID position:',sv_true_x(npsf),
     &        ', ',sv_true_y(npsf)
      else
         print *,' '
         print *,'This source has NOT been identified'
      endif
      print *,' '
      if(sv_dstsv(1,npsf))then
         print *,'-->This PSF has been LPON optimized'
         print *,' '
         print *,'srcL:',SRC_PARMS(npsf,1),
     &        ' srcB:',SRC_PARMS(npsf,2)
         print *,'TS at this position:',svn_TS(npsf)
         if(svn_TS(npsf).gt.TS_max)then
            print *,'Flux:',sv_flx(1,npsf),sv_flx(2,npsf)
            print *,'Counts:',sv_cnts(1,npsf),sv_cnts(2,npsf)
         elseif(svn_TS(npsf).gt.TS_min)then
            print *,'Flux:',sv_flx(1,npsf),sv_flx(2,npsf)
            print *,'Flux upperlimit:',sv_upperlim(npsf)
            xtmp = sv_upperlim(npsf)*sv_expose(npsf)/1.e8
            print *,'Counts:',sv_cnts(1,npsf),sv_cnts(2,npsf)
            print *,'Counts upperlimit:',xtmp
         else
            print *,'Flux upperlimit:',sv_upperlim(npsf)
            xtmp = sv_upperlim(npsf)*sv_expose(npsf)/1.e8
            print *,'Counts upperlimit:',xtmp
         endif
         print *,'Exposure[cm^2-s]=',sv_expose(npsf)
      else
         print *,'-->This PSF has NOT been LPON optimized'
         print *,' '
         print *,'srcL:',SRC_PARMS(npsf,1),
     &        ' srcB:',SRC_PARMS(npsf,2)
         srcL=SRC_PARMS(npsf,1)
         srcB=SRC_PARMS(npsf,2)
         CALL PIXEL_SELECT(.false.,jblank)
         LikTotaled=.false.
         CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
         report=JFALSE
         report2=JTRUE
         calc_uncert=JTRUE
         CALL SRCTEST(JFALSE)
         sv_cnts(1,npsf)=Counts
         sv_cnts(2,npsf)=dCounts
         sv_expose(npsf)=pss_expose
         if(pss_expose.gt.0)then
            sv_flx(1,npsf)=1.e8*Counts/pss_expose
            sv_flx(2,npsf)=1.e8*dCounts/pss_expose
            sv_upperlim(npsf)=1.e8*Counts_limit/pss_expose
         else
            sv_flx(1,npsf)=0
            sv_flx(2,npsf)=0
            sv_upperlim(npsf)=0
         endif
         print *,'TS at this position:',svn_TS(npsf)
         if(svn_TS(npsf).gt.TS_max)then
            print *,'Flux:',sv_flx(1,npsf),sv_flx(2,npsf)
            print *,'Counts:',sv_cnts(1,npsf),sv_cnts(2,npsf)
         elseif(svn_TS(npsf).gt.TS_min)then
            print *,'Flux:',sv_flx(1,npsf),sv_flx(2,npsf)
            print *,'Flux upperlimit:',sv_upperlim(npsf)
            xtmp = sv_upperlim(npsf)*sv_expose(npsf)/1.e8
            print *,'Counts:',sv_cnts(1,npsf),sv_cnts(2,npsf)
            print *,'Counts upperlimit:',xtmp
         else
            print *,'Flux upperlimit:',sv_upperlim(npsf)
            xtmp = sv_upperlim(npsf)*sv_expose(npsf)/1.e8
            print *,'Counts upperlimit:',xtmp
         endif
         print *,'Exposure[cm^2-s]=',sv_expose(npsf)
      endif
      if(sv_dstsv(2,npsf))then
         print *,' '
         print *,'This source has been error analyzed'
         print *,' '
         print *,'best choice contour:',best_choice(npsf)
         print *,'best X:',best_x(npsf),' best Y:',best_y(npsf)
         print *,'68 % error:',sv_err68(npsf)
         print *,'95 % error:',sv_err95(npsf)
         print *,' '
      else
         print *,' '
         print *,'This source has NOT been error analyzed'
         print *,' '
      endif
      xtmp=SRC_PARMS(npsf,8)
      if(xtmp.gt.0.8)then
         print *,'Source activity: Full activity: 2'
      elseif(xtmp.lt.0.8.and.xtmp.gt.0.74)then
         print *,'Source Activity: Fixed Positional: 1'
      elseif(xtmp.gt.0.01.and.xtmp.lt.0.74)then
         print *,
     &        'Source Activity: inactive with error: ',xtmp
      else
         print *,'Source Activity: Inactive: 0'
      endif
 997  print *,' '
      do j=1,50
         tinput(j:j)=' '
      enddo
      write(*,'("Enter PSF number,"$)')
      write(*,'(" <cr> to exit, (N)ext or (P)revious :"$)')
      read(LU(12),'(a)') tinput
      numcar = index(tinput, ' ') - 1
      if(numcar.lt.1)then
         tinput='          '
c     m=1
         return
      endif
c     call to_upper(tinput(1:1))
c     if(tinput(1:1).eq.'N')write(tinput,*)npsf+1
c     if(tinput(1:1).eq.'P')write(tinput,*)npsf-1
c     if(tinput(1:1).eq.'i'.or.tinput(1:1).eq.'I')tinput(1:1)=' '
c     m=1
      goto 115
 998  print *,'ERROR: 	The given source number is higher than'
      print *,'	than the number of sources in the list'
      print *,'Npsf=',npsf,' > ',NSOURCE
      goto 997
 999  print *,'ERROR:	Could not read your input. The syntax is'
      print *,'Innn or I nnn or nnn, where nnn is the PSF number'
      goto 997
      end
