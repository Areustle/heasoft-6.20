C**********************************************************************
C
C       SUBROUTINE  get_flux(Nout,Off_set,expose,org_srcL)
C
C
C  $Id: get_flux.f,v 1.2 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C++     effect: Give warning if source is within 3 degrees (and remove
c	it temporarily if within Off_set degrees). Find flux estimate.
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated: by JRM
C=======================================================================
c	variables:
c	Nout, output device
c	Off_set, maximum distance for source replacement during flux anal
c	expose, exposure
c	org_srcL, catalog x coordinate
C=======================================================================
C  $Log: get_flux.f,v $
C  Revision 1.2  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:31  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/06/06  18:50:12  jae
c Included debug output
c Set report=report2=calc_uncert=.true.
c
c Revision 5.1  1996/02/29  20:48:00  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:37:15  jae
c Subroutine Module for like V5.00
c
C%   Changes:
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      SUBROUTINE  get_flux(Nout,Off_set,expose,org_srcL)

C     Common blocks used:
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/locpos.copy'

      save

      character(80) id
      common /id/id
      LOGICAL in_OTHER
      CHARACTER srcN_temp*18

      LOC='GET_FLUX'

c     look for source confusion (less than 3 degrees)
      id = '$Id: get_flux.f,v 1.2 2013/05/21 19:08:25 irby Exp $'
      in_OTHER=.false.

      do nsrc_replace=1,NSOURCE
         if (abs(SRC_PARMS(nsrc_replace,2)-srcB).lt.3.) then
c     might be close
            theOff_set=gtcirc(SRC_PARMS(nsrc_replace,1),
     &           SRC_PARMS(nsrc_replace,2),srcL,srcB)
            if (theOff_set.lt.3.) then
               if (theOff_set.gt.Off_set) then
		  WRITE(Nout,'(A18," is only ",f4.2,
     &                 " degrees from ",A18,
     &                 " which was left in the OTHER PSF model.")')
     &                 srcN,theOff_set,SRC_NAMES(nsrc_replace)
               else
                  WRITE(Nout,'(A18," is only ",f4.2,
     &                 " degrees from ",A18,
     &                 " which was temporarily removed from the ",
     *                 "OTHER PSF model.")')
     &                 srcN,theOff_set,SRC_NAMES(nsrc_replace)
	          in_OTHER=.true.
	          srcL_temp1=srcL
	          srcB_temp1=srcB
	          Counts=0.
	          call PSFREPLACE(nsrc_replace)
	          srcN_temp=srcN
	          srcN=SRC_NAMES(nsrc_replace)
	          Counts_temp=Counts
	          srcL_temp2=srcL
	          srcB_temp2=srcB
	          srcL=srcL_temp1
	          srcB=srcB_temp1
                  goto 5
               endif
            endif
         endif
      enddo
 5    continue

      report=.true.
      report2=.true.
      calc_uncert=.true.
      call srctest(.false.)
      if (signal.ne.' ') then
c     call error(0,loc)
         WRITE(Nout,'(A18,1x,2f7.2,A7,A81,f6.1,f6.1,f7.2)')
     &        srcN,org_srcL,srcB,
     &        ' Error:',sigmsg,
     &        Ranal,aspect,expose/1.E7
         goto 4
      endif

      if (TS.gt.TS_max) then
c     This is a strong detection.
         WRITE(Nout,'(A18,1x,2f7.2,f7.1,f11.3,f9.3,11x,
     &        2f11.2,10x,f9.3,f9.3,f6.1,f6.1,f7.2)')
     &        srcN,org_srcL,srcB,sqrt(TS),
     &        Flux,1.e8*dCounts/expose,
     &        Counts,dCounts,Gmult,Gbias,Ranal
     &        ,aspect,expose/1.E7
      elseif (TS.le.TS_max.and.TS.gt.TS_min) then
c     This is a weak detection.
         WRITE(Nout,'(A18,1x,2f7.2,f7.1,f11.3,f9.3,"  <",f8.3,
     &        2f11.2,"  <",f7.2,f9.3,f9.3,f6.1,f6.1,f7.2)')
     &        srcN,org_srcL,srcB,sqrt(TS),
     &        Flux,1.e8*dCounts/expose,1.e8*Counts_limit/expose,
     &        Counts,dCounts,Counts_limit,Gmult,Gbias,Ranal
     &        ,aspect,expose/1.E7
      else
c     This is not detected.
         WRITE(Nout,'(A18,1x,2f7.2,f7.1,20x,"  <",f8.3,
     &        22x,"  <",f7.2,f9.3,f9.3,f6.1,f6.1,f7.2)')
     &        srcN,org_srcL,srcB,sqrt(TS),
     &        1.e8*Counts_limit/expose,
     &        Counts_limit,Gmult,Gbias,Ranal
     &        ,aspect,expose/1.E7
      endif

 4    continue

      if (in_OTHER) then
c     put PSF back in other map
         Counts=Counts_temp
         srcL=srcL_temp2
         srcB=srcB_temp2
         call pixel_select(.false.,' ')
         call error(1,loc)
         srcN=srcN_temp
         call PSFREPLACE(nsrc_replace)
      endif

      return
      end
