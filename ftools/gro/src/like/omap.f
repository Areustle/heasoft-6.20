c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C	SUBROUTINE OMAP(tmp)
C
C
C  $Id: omap.f,v 1.4 2013/05/21 19:08:26 irby Exp $
c
c
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C  $Log: omap.f,v $
C  Revision 1.4  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.3  2005/08/26 19:36:35  irby
C  Purely cosmetic changes to allow compilation with gfortran/g95, mostly
C  involving fixes to lines longer than 72 chars that were being truncated,
C  but in the case of the CGRO code, also moving misplaced (tabbed) line
C  continuation characters to their appropriate position in column 6.
C
C  Revision 1.2  2002/04/18 19:34:10  irby
C  Bug fixes for OSF build:
C
C  - change "dreal" to acceptable replacement "dble"
C  - changed null string comparisons '' to ' ' - this may not be the best
C    solution.
C  - removed malloc.h from like.h (unnecessary, and not available under Darwin)
C  - Makefile generated using: mkmk version 1.81
C
C  Revision 1.1  2002/04/16 20:27:41  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:52:17  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:34  jae
c Subroutine Module for like V5.00
c
C
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	SUBROUTINE OMAP(tmp)

c Common blocks included
	INCLUDE  '../COMMON/ctlrep.copy'
	INCLUDE  '../COMMON/cnfrep.copy'
	INCLUDE  '../COMMON/roirep.copy'
	INCLUDE  '../COMMON/likrep.copy'
	INCLUDE  '../COMMON/maprep.copy'
	INCLUDE  '../COMMON/errrep.copy'
	INCLUDE  '../COMMON/gasrep.copy'
	INCLUDE  '../COMMON/emprep.copy'
	INCLUDE  '../COMMON/bmprep.copy'
	INCLUDE  '../COMMON/tmprep.copy'
	INCLUDE  '../COMMON/psfrep.copy'
	INCLUDE  '../COMMON/psmrep.copy'
	INCLUDE  '../COMMON/xrrrep.copy'
	INCLUDE  '../COMMON/fitrep.copy'
	INCLUDE  '../COMMON/locpos.copy'

	save

	character(80) id
	common /id/id
	character(260) tmp
	character(50) input,sav_NEWMAP,catalog_file
	LOGICAL FEXIST
c
	id = '$Id: omap.f,v 1.4 2013/05/21 19:08:26 irby Exp $'
	LOC='OMAP  '
	k=0
c
	nsrc=NSOURCE

 11	do jj=1,20
	   call to_upper(tmp(jj:jj))
	enddo

	in_dex1=index(tmp,'L')
	in_dex2=index(tmp,'S')
	in_dex3=index(tmp,'M')
	in_dexc=index(tmp,'C')
	in_dexi=index(tmp,'I')
	in_dexp=index(input,'+')

 12	if (in_dex1.gt.0) then
	   tttflg2=JFALSE
	   nsrc1=503
	   nsrc2=nsrc1
	   write(*,'("Enter catalog filename( A to Abort):"$)')
	   read(LU(12),'(a)') catalog_file
	   numcar1 = index(catalog_file, ' ') - 1

	   if (numcar1.eq.1.and.catalog_file(1:1).eq.'A') return

	   INQUIRE(file=input(1:numcar),EXIST=FEXIST)
	   if (.not.FEXIST) then
	      write(*,'("FILE:",A," DOES NOT EXIST !")')
     1                catalog_file(1:numcar1)
	      print *,' Input filename again or abort'
	      print *,' '
	      goto 12
	   endif

	   open(unit=73,file=catalog_file)

	else
	   nsrc1=1
	   nsrc2=NSOURCE
	endif

	CALL MAPCPY(nmap,bmap,CTLMSZ1,CTLMSZ2)
	sav_NEWMAP=NEWMAPFILE
	sv_CTLORG1=CTLORG(1)
	sv_CTLORG2=CTLORG(2)
	sv_CTLEND1=CTLEND(1)
	sv_CTLEND2=CTLEND(2)
	sv_ROIORG1=ROIORG(1)
	sv_ROIORG2=ROIORG(2)
	sv_ROIEND1=ROIEND(1)
	sv_ROIEND2=ROIEND(2)
	sv_CTLMSZ1=CTLMSZ1
	sv_CTLMSZ2=CTLMSZ2
	write(*,'("PSF          NAME",8x,"  Long           Lat")')
	write(*,'(" ")')

	do j=1,nsrc
	   if (SRC_PARMS(j,8).gt.0.74) then
	      write(*,'(i4,2x,A,2x,2f10.3)')j,SRC_NAMES(j),
     1              SRC_PARMS(j,1),SRC_PARMS(j,2)
	   endif
	enddo

 15	write(*,'(" ")')

	if (in_dex1.gt.0) write(*,'("You have selected to read a",
     1      " src.list file:",A)')catalog_file(1:numcar1)

	if (in_dex2.gt.0) write(*,'("You have selected to calculate",
     1      " Gmult and Gbias at each point")')

	in_dex_tst=0

	if (in_dexc.gt.0) in_dex_tst=in_dex_tst+1
	if (in_dexi.gt.0) in_dex_tst=in_dex_tst+1

	if (in_dexc.ne.0.and.in_dexm.eq.0.and.in_dex_tst.eq.1) then
	   write(*,'(" You have selected to output residual counts",
     1           " maps")')
	   goto 53

	elseif (in_dexi.ne.0.and.in_dexm.eq.0.and.in_dex_tst.eq.1) then
	   write(*,'(" You have selected to output residual intensity",
     1           " maps")')
	   goto 53

	elseif (in_dexm.ne.0.and.in_dexc.ne.0.and.in_dex_tst.eq.1) then
	   write(*,'(" You have selected to output model counts",
     1           " maps")')
	   goto 53

	elseif (in_dexm.ne.0.and.in_dexi.ne.0.and.in_dex_tst.eq.1) then
	   write(*,'(" You have selected to output model intensity",
     1           " maps")')
	   goto 53

	else
	   if (in_dex_tst.eq.2) then
	      write(*,'("You have selected both (C)ounts and",
     1                  " (I)ntensity")')
	      write(*,'("You MUST select EITHER (C) OR (I) NOT BOTH")')
	      write(*,'(" ")')
	   endif

	   write(*,'(" Enter C for residual counts",/," I for residual",
     1           " intensity")')
	   write(*,'("Append an M for model counts or intensity")')
	   write(*,'("Append an L to read a src.list file")')
	   write(*,'("Append an S to calculate parameters at each",
     1               " point:"$)')
	endif

	read(LU(12),'(a)') input
	numcar = index(input, ' ') - 1

	if (numcar.eq.0) goto 101

	tmp(1:1)='O'
	tmp(2:numcar+1)=input(1:numcar)
	goto 11
c
 53	width=20.
	height=20.
	write(*,'("Enter the longitudinal width (<= 360) for all maps",
     1            "(default=20): "$)')
	read(LU(12),'(a)') input
	numcar = index(input, ' ') - 1

	if (input(1:1).eq.'A'.or.input(1:1).eq.'a') goto 101

	if (numcar.eq.0) goto 6

	read(input,*,err=55,end=55)width

	if (int(width).gt.360) goto 55

	goto 6
 55	write(*,'(" Error on input ! <A> to abort or enter width.")')
	goto 53
 6	write(*,'("Enter the latitudinal height (<=180) for all ",
     1        " active maps (default=20): "$)')
	read(LU(12),'(a)') input
	numcar = index(input, ' ') - 1

	if (input(1:1).eq.'A'.or.input(1:1).eq.'a') goto 101
	if (numcar.eq.0) goto 7

	read(input,*,err=66,end=66)height

	if (int(height).gt.180) goto 66

	goto 7
 66	write(*,'(" Error on input ! <A> to abort or enter width.")')
	goto 6
 7	continue

	if (int(width*height*4).gt.(360*720).or.int(height).gt.180.or.
     1      int(width).gt.360) then
	   print *,' '
	   print *,'width*height exceeds 360x180:',INT(width*height+0.1)
	   print *,'or width=',width,' > 360 or height=',height,' > 180'
	   print *,'Please re-enter and stay within these limits'
	   print *,' '
	   goto 53
	endif

	roio1=ROIORG(1)
	roio2=ROIORG(2)
	roie1=ROIEND(1)
	roie2=ROIEND(2)
 74	continue

	if (in_dex1.gt.0.and.in_dexp.ne.0) goto 201
c		
 75	continue

	do j=nsrc1,nsrc2
	   if (in_dex1.eq.0) I=j
	   if (SRC_PARMS(j,8).lt.0.74) goto 99

	   print *,' '
	   print *,'***********************************************'

	   if (in_dex1.eq.0) print *,' PSF: ',j
	   if (in_dex1.ne.0) print *,' Catalog Source: ',k

	   if (coord_sys.eq.'G') then
	      print *,' Long, Lat: ',srcL,',',srcB
	   else
	      print *,' RA, DEC: ',srcL,',',srcB
	   endif

	   print *,' SOURCE NAME: ',SRC_NAMES(j)

	   if (I.ne.0.and.in_dex1.gt.0) then
	      counts_sv=SRC_PARMS(I,3)
	      srcL=SRC_PARMS(I,1)
	      srcB=SRC_PARMS(I,2)
	      gamma=SRC_PARMS(I,4)
	      srcN=SRC_NAMES(I)
	   endif

	   counts=0

	   if (in_dex1.eq.0.and.in_dexp.ne.0) CALL PSFREPLACE(I)
	   if (in_dex1.ne.0.and.I.ne.0.and.in_dexp.eq.0) then
	      srcN=SRC_NAMES(I)
	      CALL PSFREPLACE(I)
	      srcL=SRC_PARMS(I,1)
	      srcB=SRC_PARMS(I,2)
	      Counts=0
	      write(*,'("Removing source[",I3,"]:",A," at ",
     1                 F4.2," degrees")')srcN,aspect
	   endif

	   pL1 = SRC_PARMS(j,1) - width/2
	   pB1 = SRC_PARMS(j,2) - height/2
	   pL2 = SRC_PARMS(j,1) + width/2
	   pB2 = SRC_PARMS(j,2) + height/2
	   CALL ROISET(pL1,pB1,pL2,pB2,ROIORG,ROIEND,ROIPRG,ROIPND)
	   gamma=SRC_PARMS(j,4)
	   srcL=SRC_PARMS(j,1)
	   srcB=SRC_PARMS(j,2)

	   if (.not.(Restrict_Gmult.and.Restrict_Gbias))
     1         call allfit(.false.)

	   counts=0

	   if (tmp(2:2).eq.'C') input='OMRC'
	   if (tmp(2:2).eq.'I') input='OMRF'

	   srcN='GRO'
	   call GETSRCNM()
	   srcN(4:4)='_'

	   if (tmp(2:2).eq.'C') then
	      NEWMAPFILE=srcN(1:12)//'.res-counts'
	   else
	      NEWMAPFILE=srcN(1:12)//'.res-intens'
	   endif

	   if (in_dex2.eq.0) then
	      call residue(input,.false.)
	   else
	      call residuej(tmp,JFALSE,width,height)
	   endif

	   if (I.ne.0.and.in_dexp.ne.0) then
	      srcL=SRC_PARMS(I,1)
	      srcB=SRC_PARMS(I,2)
	      counts=counts_sv
	      srcN=SRC_NAMES(I)
	      SRC_PARMS(I,3)=counts
	   endif

	   print *,'***********************************************'

	   if (in_dexp.ne.0) CALL MAPCPY(bmap,nmap,CTLMSZ1,CTLMSZ2)
 99	   continue
	enddo

 992	goto 201
 101	NEWMAPFILE=sav_NEWMAP
	call ROISET(sv_ROIORG1,sv_ROIORG2,sv_ROIEND1,sv_ROIEND2,
     1       ROIORG,ROIEND,ROIPRG,ROIPND)
	CTLORG(1)=sv_CTLORG1
	CTLORG(2)=sv_CTLORG2
	CTLEND(1)=sv_CTLEND1
	CTLEND(2)=sv_CTLEND2
	CTLMSZ1=sv_CTLMSZ1
	CTLMSZ2=sv_CTLMSZ2
	CALL MAPCPY(bmap,nmap,CTLMSZ1,CTLMSZ2)

	if (in_dex1.gt.0) close(unit=73)

	tttflg=JFALSE
	tttflg2=JFALSE

	return
 201	continue
	nsrc1=503
	nsrc2=503
	read(73,*,end=101,err=201)SRC_NAMES(503),tmp(101:157),
     1       pRA,pDEC,srcL,srcB
	k=k+1

	if (coord_sys.eq.'C') then
	   SRC_PARMS(503,1)=pRA
	   SRC_PARMS(503,2)=pDEC
	else
	   SRC_PARMS(503,1)=srcL
	   SRC_PARMS(503,2)=srcB
	endif

	CALL L_CHECK(SRC_PARMS(503,1))

	if (signal.ne.' ') then
	   CALL ERROR(0,LOC)
	   goto 201
	endif

	SRC_PARMS(503,4)=2.0
	SRC_PARMS(503,3)=0
	i=0
	m=0
	aspjmin=1000.

	do l=1,NSOURCE
	   aspect=gtcirc(SRC_PARMS(503,1),SRC_PARMS(503,2),
     1            SRC_PARMS(l,1),SRC_PARMS(l,2))
	   if (aspect.lt.aspj) then
	      m=m+1
	      print *,'PSF:',l,' is within the cut boundary'
	      if(aspect.lt.aspjmin)then
		 I=l
		 aspjmin=aspect
	      endif
	   endif
	enddo

	if (I.eq.0.and.m.eq.0) goto 75

	if (M.gt.1) then
	   print *,' '
	   print *,'More than one PSF was within the zenith cone'
	   print *,'Only the closest one will be removed'
	   print *,' '
	endif

	print *,' '
	print *,'PSF:',I,' will be removed from the list'
	print *,' '
	goto 75

c	return
	end
c       
