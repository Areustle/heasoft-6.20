C      SUBROUTINE HOOD(map,bmap,gasmap,emap
C     &                   ,IMAPWIDTH,IMAPHEIGHT)
C
C
C  $Id: hood.f,v 1.3 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C++   Effect: HOOD calculates negative log of  likelihood 
C++           for binned events (map) for the counts model, comprised of
C++           Gmult * background model (gasmap) +
C++           Gbias * exposure + COUNTS * PSF + other sources (bmpmap).
C++	      for extant parameters. Uses poisson probability.
C++	      No derivatives calculated.
c
c-----------------------------------------------------------------------
c     Subroutine Argument Desriptions
c     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c     REAL   map,bmap,gasmap,emap data maps (from common blocks)
c     INTEGER   IMAPWIDTH,IMAPHEIGHT working map size
c
c
C=======================================================================
C  LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C             UPDATED:    by  JRM
c
C=======================================================================
C  $Log: hood.f,v $
C  Revision 1.3  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2002/04/18 19:34:09  irby
C  Bug fixes for OSF build:
C
C  - change "dreal" to acceptable replacement "dble"
C  - changed null string comparisons '' to ' ' - this may not be the best
C    solution.
C  - removed malloc.h from like.h (unnecessary, and not available under Darwin)
C  - Makefile generated using: mkmk version 1.81
C
C  Revision 1.1  2002/04/16 20:27:32  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:48:06  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:37:26  jae
c Subroutine Module for like V5.00
c
C%   Changes:
c 
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      SUBROUTINE HOOD(map,bmap,gasmap,emap,IMAPWIDTH,IMAPHEIGHT)

C     Common blocks used:
      INCLUDE '../COMMON/ctlrep.copy'
      INCLUDE '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE '../COMMON/psfrep.copy'
      INCLUDE '../COMMON/likrep.copy'

      save

      character(80) id
      common /id/id
      REAL MAP(IMAPWIDTH,IMAPHEIGHT)
      REAL BMAP(IMAPWIDTH,IMAPHEIGHT)
      REAL GASMAP(IMAPWIDTH,IMAPHEIGHT)
      REAL EMAP(IMAPWIDTH,IMAPHEIGHT)
      REAL*8 lnL8
      logical psfLok


      id = '$Id: hood.f,v 1.3 2013/05/21 19:08:25 irby Exp $'
      lnL8= 0.D0

      cnts_model_min=1.
      Gbias_actual=Gbias*1.e-5
      cnts_total=
     &     GAST*Gmult+EXPT*Gbias_actual+PSFT*Counts+OTHER_SRCT ! total model counts

C     integrate over the analysis region using the
c     ANALysis table for this pixel calculated in LIKTOT
      do NL=1,Lanal
         if (ANAL(4,NL).GE.1.and.ANAL(4,NL).LE.PSFSZ) then
	    psfLok=.true.
         else
	    psfLok=.false.
         endif
         do jB=ANAL(2,NL),ANAL(3,NL)
	    if (psfLok) then
               jBPSF=jB+IBoffset
               if (jBPSF.GE.1.and.jBPSF.LE.PSFSZ) then
                  psf_val=PSF(ANAL(4,NL),jBPSF)
	       else
                  psf_val=0.
               endif
            else
               psf_val=0.
	    endif
            cnts_model = Gmult *  GASMAP(ANAL(1,NL),jB)
     &           + Gbias_actual * EMAP(ANAL(1,NL),jB)
     &           + Counts * psf_val + BMAP(ANAL(1,NL),jB)
	    if (cnts_model.lt.0.) then
               if (cnts_model.lt.cnts_model_min) then
                  cnts_model_min=cnts_model
                  iL_min=ANAL(1,NL)
                  iB_min=jB
               endif
            else
               if (MAP(ANAL(1,NL),jB).gt.0.) then
c     there are experimental counts here
                  if (cnts_model.eq.0.) then
                     CALL MAPCOR (ANAL(1,NL),jB,pL_min,pB_min)
                     CALL ERROR(1,LOC) 
                     write(sigmsg,'(
     &                    "HOOD:zero cnts_model at ",2f7.2)')
     &                    pL_min,pB_min
                     signal='Z'
                     lnL=0.
                     RETURN
                  endif
                  ILOG=1.E4*cnts_model +1.
                  if (ILOG.LT.29.OR.ILOG.GT.300000) then
                     dlnL=MAP(ANAL(1,NL),jB)*ALOG(cnts_model)
                  else
                     dlnL=MAP(ANAL(1,NL),jB)*XLOG(ILOG)
                  endif
                  lnL8=lnL8-dble(dlnL) 
               endif
	    endif
         enddo
      enddo

      if (cnts_model_min.lt.0.) then
         CALL MAPCOR (iL_min,iB_min,pL_min,pB_min)
         CALL ERROR(1,LOC) 
         write(sigmsg,'(
     &        "HOOD:negative cnts_model=",e11.3," at ",2f7.2)')
     &        cnts_model_min,pL_min,pB_min
         signal='N'
         lnL=0.
         RETURN
      endif

c     model predicts positive counts for all pixels with counts
      lnL=sngl(lnL8)+cnts_total
      
      RETURN
      END
