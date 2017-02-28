c
C	SUBROUTINE RESIDUEJ(INPUT,FLAG,width,height)
C
C
C  $Id: residuej.f,v 1.4 2013/05/21 19:08:26 irby Exp $
C
c
c
C  $Log: residuej.f,v $
C  Revision 1.4  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.3  2006/04/17 20:54:28  irby
C  Updates for compliance with g95 and gfortran:
C
C  - Replace call to lnblnk with equivalent call to len_trim.
C    lnblnk (a routine which used to live in libg2c) is not currently
C    available with g95 or gfortran.
C
C  - Change calls to "perror" (also libg2c) to fcerr or c_fcerr.
C
C  - Change calls to IDATE (libg2c) to new libgro routine GIDATE.
C
C  - Fix non-integer loop variables.
C
C  Revision 1.2  2005/08/26 19:36:35  irby
C  Purely cosmetic changes to allow compilation with gfortran/g95, mostly
C  involving fixes to lines longer than 72 chars that were being truncated,
C  but in the case of the CGRO code, also moving misplaced (tabbed) line
C  continuation characters to their appropriate position in column 6.
C
C  Revision 1.1  2002/04/16 20:27:43  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:53:13  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:56:03  jae
c Subroutine Module for like V5.00
c
c
c
	SUBROUTINE RESIDUEJ(INPUT,FLAG,width,height)


	real           width,height
	REAL           MAPVAL
 	character(260)  input
	character(80)   id
	LOGICAL        FLAG

	common /id/id


        INCLUDE  '../COMMON/ctlrep.copy'
        INCLUDE  '../COMMON/cnfrep.copy'
        INCLUDE  '../COMMON/roirep.copy'
        INCLUDE  '../COMMON/likrep.copy'
        INCLUDE  '../COMMON/maprep.copy'
        INCLUDE  '../COMMON/gasrep.copy'
        INCLUDE  '../COMMON/errrep.copy'
        INCLUDE  '../COMMON/emprep.copy'
        INCLUDE  '../COMMON/bmprep.copy'
        INCLUDE  '../COMMON/tmprep.copy'
        INCLUDE  '../COMMON/psfrep.copy'
        INCLUDE  '../COMMON/psmrep.copy'
        INCLUDE  '../COMMON/xrrrep.copy'
        INCLUDE  '../COMMON/fitrep.copy'
        INCLUDE  '../COMMON/locpos.copy'

        save

c
c set array size and check if counts or flux output
c
	id = '$Id: residuej.f,v 1.4 2013/05/21 19:08:26 irby Exp $'


	nwidth=((width/ctlscl)+0.1)
	nheight=((height/ctlscl)+0.1)
	in_dexC=index(input,'C')
	in_dexI=index(input,'I')
	in_dexM=index(input,'M')
	in_dexS=index(input,'S')
	in_dexP=index(input,'+')

	cmodel = 0.

	if (in_dexS.eq.0) CALL ALLFIT(.false.)

	DO 80 JB=ROIPRG(2),ROIPND(2) ! do latitude row
	   JL=ROIPRG(1)		! begin longitude loop
 100	   CONTINUE
	   call mapcor(JL,JB,srcL,srcB)
	   call pixel_select(.false.,'                    ')
	   CALL PSMMAT(0.,0.)
	   if (in_dexS.gt.0) CALL ALLFIT(.false.)
	   cmodel = Gbias*1.e-5*MAPVAL(emap,JL,JB,CTLMSZ1,CTLMSZ2)
	   cmodel = cmodel + MAPVAL(bmap,JL,JB,CTLMSZ1,CTLMSZ2)
	   cmodel = cmodel + Gmult*
     & 		MAPVAL(gasmap,iother,ix,CTLMSZ1,CTLMSZ2)
           IMAP=JL+ nwidth*(JB-1)
	   
	   if (in_dexM.eq.0) then
	      TMPMAP(IMAP)=MAPVAL(map,JL,JB,CTLMSZ1,CTLMSZ2)-cmodel
	   else
	      TMPMAP(IMAP)=cmodel
	   endif

	   if (in_dexc.eq.0) then
c       
c       divide by the exposure
c
	      exposure_val=mapval(emap,JL,JB,CTLMSZ1,CTLMSZ2) / (
     1         (sin(pi180*(srcB+ctlscl/2.))-sin(pi180*(srcB-ctlscl/2.)))
     2         *ctlscl*pi180)
	      TMPMAP(IMAP)=1.e8*TMPMAP(IMAP)/exposure_val
	   endif
c
c       DO ALL COLUMNS IN THE ROI
c
           CALL MAPINC(JL,IEND,ROIPRG(1),ROIPND(1)) 
           IF (SIGNAL.NE.' ') CALL ERROR(1,LOC)
           IF(IEND.NE.1) GOTO 100
 80	CONTINUE		! end latitude loop
 81	CONTINUE
	
c
c       Set up FITS DOC ARRAY and OUTPUT FITS FILE
c
	TMPTYP='RCMP'
	do i=1,10
           TMPDOC(i)=' '
        enddo

	if (in_dexS.gt.0) then
	   write(TMPDOC(1), '("Parameters calculated at each point")')

	else
	   write(TMPDOC(1),
     & 		'("Parameters calculated once at map center")')
	endif

	write(TMPDOC(2),*)'Counts map: ',cmapfile(1:len_trim(cmapfile))
	write(TMPDOC(3),*)'Diffuse map: ',gmapfile(1:len_trim(gmapfile))
	write(TMPDOC(4),'(i3," PSFs in model. ")')NSOURCE
	if (in_dexc.gt.0) then
	   write(moreinput,*)
     &     	'Residual written without normalization.'
	   nchar=38
	   call DOCADD(nchar,TMPDOC,TMPTYP,moreinput)
	   write(6,*)moreinput
        else
	   TMPTYP='RFMP'
	   write(moreinput,*)'Residual flux written.  '
	   nchar=29
	   call DOCADD(nchar,TMPDOC,TMPTYP,moreinput)
	   write(6,*)moreinput
	endif
c       
c       if(nsource.gt.0)then
c       SHOULD call DOCADD for PSF values
c       endif
c
        MAPFILE=NEWMAPFILE
	i = index(MAPFILE, ' ') - 1
	if (i.le.0) i=1
        if (jae_residuej) write(6,*)'Writing ',TMPTYP,
     1                              ' to file:',MAPFILE(1:i)
        CALL MAPRITROI(TMPMAP,TMPTYP,TMPDOC)
        CALL ERROR(0,LOC)
	
	return
	end
