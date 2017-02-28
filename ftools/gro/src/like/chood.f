c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c        SUBROUTINE CHOOD(map,bmap,gasmap,emap,thelnL,dLdCounts
c     &                   ,IMAPWIDTH,IMAPHEIGHT)
c
c
c  $Id: chood.f,v 1.3 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C++           effect: CHOOD calculates negative log of  likelihood and
C++	       first  derivatives w.r.t  counts 
C=======================================================================
C LIKE Version: 4.6  DELIVERED: June 1993      JMATTOX
C+             UPDATED:    by  JRM
C=======================================================================
C  $Log: chood.f,v $
C  Revision 1.3  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2002/04/18 19:34:08  irby
C  Bug fixes for OSF build:
C
C  - change "dreal" to acceptable replacement "dble"
C  - changed null string comparisons '' to ' ' - this may not be the best
C    solution.
C  - removed malloc.h from like.h (unnecessary, and not available under Darwin)
C  - Makefile generated using: mkmk version 1.81
C
C  Revision 1.1  2002/04/16 20:27:28  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:47:11  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:36:16  jae
c Subroutine Module for like V5.00
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      SUBROUTINE CHOOD(map,bmap,gasmap,emap,thelnL,dLdCounts
     &     ,IMAPWIDTH,IMAPHEIGHT)

C     Common blocks used:
      INCLUDE '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE '../COMMON/cnfrep.copy'
      INCLUDE '../COMMON/psfrep.copy'
      INCLUDE '../COMMON/likrep.copy'

      save

      character(80) id
      common /id/id
      REAL MAP(IMAPWIDTH,IMAPHEIGHT)
      REAL BMAP(IMAPWIDTH,IMAPHEIGHT)
      REAL GASMAP(IMAPWIDTH,IMAPHEIGHT)
      REAL EMAP(IMAPWIDTH,IMAPHEIGHT)
      REAL*8 dC,lnL8
      logical psfLok


      id = '$Id: chood.f,v 1.3 2013/05/21 19:08:25 irby Exp $'
      loc='CHOOD'

      lnL8 = 0.D0
      dC=0.D0
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
c     signal='N'
               dLdCounts=-1.e3
               thelnL=1.e9-1.e3*Counts
               return
	    endif
            if (MAP(ANAL(1,NL),jB).gt.0.) then
c     there are experimental counts here
               if (cnts_model.eq.0.) then
c     signal='Z'
                  dLdCounts=-1.e3
                  thelnL=1.e9-1.e3*Counts
                  return
               endif
               ILOG=1.E4*cnts_model +1.
               if (ILOG.LT.29.OR.ILOG.GT.300000) then
                  dlnL=MAP(ANAL(1,NL),jB)*ALOG(cnts_model)
	       else
                  dlnL=MAP(ANAL(1,NL),jB)*XLOG(ILOG)
               endif
               lnL8=lnL8-dble(dlnL) 
c     1st derivatives
               cntsFrac= MAP(ANAL(1,NL),jB) / cnts_model
               dC=dC + dble(cntsFrac*psf_val)
            endif
         enddo
      enddo

      dLdCounts=-sngl(dC)+PSFT
      thelnL=sngl(lnL8)+cnts_total
      SIGNAL=' '

      RETURN
      END
c
