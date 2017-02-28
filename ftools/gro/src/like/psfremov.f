C=======================================================================
C	SUBROUTINE PSFREMOVE(Npsf)
C
C
C  $Id: psfremov.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C  effect: Remove PSF number Npsf in the other PSF map for LIKE program.
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated: by JRM
C=======================================================================
C  $Log: psfremov.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:42  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:52:30  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:49  jae
c Subroutine Module for like V5.00
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	SUBROUTINE PSFREMOVE(Npsf)

C	Common blocks used:
	INCLUDE  '../COMMON/likrep.copy'
	INCLUDE  '../COMMON/cnfrep.copy'
        INCLUDE  '../COMMON/errrep.copy'
	INCLUDE  '../COMMON/psmrep.copy'
cj--->
cj---> added LPxx common block
cj--->
	INCLUDE  '../COMMON/locpos.copy'
cj--->
cj---> End of changed code  /JAE
cj--->^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

        save
c
	character(80) id
	common /id/id

	id = '$Id: psfremov.f,v 1.2 2013/05/21 19:08:26 irby Exp $'
        LOC='PSFREM'


	Counts=0.
	srcN=' '

	call psfreplace(Npsf)
	if (signal.ne.' ') then
	   call error(0,loc)
	   return
	endif

	if (Npsf.eq.NSOURCE) then
	   NSOURCE=NSOURCE-1

	else
	   NSOURCE=NSOURCE-1
	   do ipsf=Npsf,NSOURCE
	      SRC_PARMS(ipsf,1)=SRC_PARMS(ipsf+1,1)
	      SRC_PARMS(ipsf,2)=SRC_PARMS(ipsf+1,2)
	      SRC_PARMS(ipsf,3)=SRC_PARMS(ipsf+1,3)
	      SRC_PARMS(ipsf,4)=SRC_PARMS(ipsf+1,4)
	      SRC_PARMS(ipsf,8)=SRC_PARMS(ipsf+1,8)
	      SRC_NAMES(ipsf)=SRC_NAMES(ipsf+1)
cj--->
cj---> Added lines to move LPxx array contents down from ispf+1 to ispf
cj--->
	      sv_true_x(ipsf)=sv_true_x(ipsf+1)
	      sv_true_y(ipsf)=sv_true_y(ipsf+1)
	      sv_err68(ipsf) =  sv_err68(ipsf+1)
	      sv_err95(ipsf) =  sv_err95(ipsf+1)
	      sv_sigsv(ipsf) =  sv_sigsv(ipsf+1)
	      best_choice(ipsf) = best_choice(ispf+1)
	      best_x(ipsf) =  best_x(ipsf+1)
	      best_y(ipsf) =  best_y(ipsf+1)
	      sv_params(1,ipsf)=sv_params(1,ipsf+1)
	      sv_params(2,ipsf)=sv_params(2,ipsf+1)
	      sv_flx(1,ipsf) = sv_flx(1,ipsf+1)
	      sv_flx(2,ipsf) =  sv_flx(2,ipsf+1)
	      sv_cnts(1,ipsf) =  sv_cnts(1,ipsf+1)
	      sv_cnts(2,ipsf) = sv_cnts(2,ipsf+1)
	      sv_upperlim(ipsf) = sv_upperlim(ipsf+1)
	      sv_expose(ipsf) = sv_expose(ipsf+1)
	      sv_tmp_68_x(ipsf)=sv_tmp_68_x(ipsf+1)
	      sv_tmp_68_y(ipsf)=sv_tmp_68_y(ipsf+1)
	      sv_tmp_95_x(ipsf)=sv_tmp_95_x(ipsf+1)
	      sv_tmp_95_y(ipsf)=sv_tmp_95_y(ipsf+1)
	      svn_TS(ipsf)=svn_TS(ipsf+1)

	      do ll=1,10
		 sv_dstsv(ll,ipsf)=sv_dstsv(ll,ipsf+1)
	      enddo
cj--->
cj---> End of changed code  /JAE
cj--->^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c
	   enddo
	endif

	LikTotaled=.false.

	RETURN
	END
c
