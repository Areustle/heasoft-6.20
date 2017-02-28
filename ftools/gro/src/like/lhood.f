C        SUBROUTINE LHOOD(map,bmap,gasmap,emap,score,information
c     &                   ,IMAPWIDTH,IMAPHEIGHT)
C
C
C  $Id: lhood.f,v 1.3 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C++           effect: LHOOD calculates first and second derivatives of
C++	      negative log of  likelihood for the counts model, comprised 
C++           of Gmult * background model (gasmap) +
C++           Gbias * exposure + R * PSF + other sources (bmpmap).
c     Subroutine Argument Desriptions
c     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c     REAL   map,bmap,gasmap,emap data maps 
c     REAL   score vector of first derivatives of lnL
c     REAL   information matrix of second derivatives of lnL
c     INTEGER   IMAPWIDTH,IMAPHEIGHT working map size
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED:    by  JRM
C=======================================================================
C  $Log: lhood.f,v $
C  Revision 1.3  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
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
C  Revision 1.1  2002/04/16 20:27:35  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:51:35  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:54:36  jae
c Subroutine Module for like V5.00
c
C%   Changes: 
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE LHOOD(map,bmap,gasmap,emap,score,information
     &     ,IMAPWIDTH,IMAPHEIGHT)

C     Common blocks used:
      INCLUDE '../COMMON/ctlrep.copy'
      INCLUDE '../COMMON/cnfrep.copy'
      INCLUDE '../COMMON/psfrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE '../COMMON/likrep.copy'
      
      save

      character(80) id
      common /id/id
      REAL MAP(IMAPWIDTH,IMAPHEIGHT)
      REAL BMAP(IMAPWIDTH,IMAPHEIGHT)
      REAL GASMAP(IMAPWIDTH,IMAPHEIGHT)
      REAL EMAP(IMAPWIDTH,IMAPHEIGHT)
      REAL*8 dC,dG,dB,dCC,dGG,dBB,dCG,dCB,dGB
      REAL score(3),information(3,3)
      logical psfLok

      id = '$Id: lhood.f,v 1.3 2013/05/21 19:08:26 irby Exp $'
      loc='LHOOD'


      dC=0.D0
      dB=0.D0
      dG=0.D0
      dCC=0.D0
      dGG=0.D0
      dBB=0.D0
      dCG=0.D0
      dCB=0.D0
      dGB=0.D0
      Gbias_actual=Gbias*1.e-5

C     integrate over the analysis region using the
c     ANALysis table for this pixel calculated in LIKTOT
      do NL=1,Lanal
         if(ANAL(4,NL).GE.1.and.ANAL(4,NL).LE.PSFSZ) then
	    psfLok=.true.
         else
	    psfLok=.false.
         endif
         do jB=ANAL(2,NL),ANAL(3,NL)
            if(MAP(ANAL(1,NL),jB).gt.0.) then
               
c     there are counts here - do the analysis
               if(psfLok) then
                  jBPSF=jB+IBoffset
                  if(jBPSF.GE.1.and.jBPSF.LE.PSFSZ) then
                     psf_val=PSF(ANAL(4,NL),jBPSF)
                  else
                     psf_val=0.
                  endif
               else
                  psf_val=0.
               endif
               cnts_model = Gmult *  GASMAP(ANAL(1,NL),jB)
     &              + Gbias_actual * EMAP(ANAL(1,NL),jB)
     &              + Counts * psf_val + BMAP(ANAL(1,NL),jB)
               if(cnts_model.eq.0.)then
c     set to finite number - need to divide by this
                  cnts_model=1.e-9 
               endif
c     1st derivatives
               cntsFrac= MAP(ANAL(1,NL),jB) / cnts_model
               dC=dC + dble(cntsFrac*psf_val)
               dB=dB + dble(cntsFrac*EMAP(ANAL(1,NL),jB))
               dG=dG + dble(cntsFrac*GASMAP(ANAL(1,NL),jB))
c     2nd derivatives
               cntsFrac2= cntsFrac / cnts_model
               dCC=dCC - dble(cntsFrac2*psf_val**2)
               dBB=dBB - dble(cntsFrac2*EMAP(ANAL(1,NL),jB)**2)
               dGG=dGG - dble(cntsFrac2*GASMAP(ANAL(1,NL),jB)**2)
               dCG=dCG - dble(cntsFrac2*psf_val*GASMAP(ANAL(1,NL),jB))
               dCB=dCB - dble(cntsFrac2*psf_val*EMAP(ANAL(1,NL),jB))
               dGB=dGB - dble(cntsFrac2*GASMAP(ANAL(1,NL),jB)*
     &              EMAP(ANAL(1,NL),jB))
	    endif
         enddo
      enddo
      
      if(Nopt.eq.3)then
         score(1)=sngl(dC)-PSFT
         score(2)=sngl(dG)-GAST
         score(3)=(sngl(dB)-EXPT)*1.e-5
         information(1,1)=-sngl(dCC)
         information(2,2)=-sngl(dGG)
         information(3,3)=-sngl(dBB)*1.e-10
         information(1,2)=-sngl(dCG)
         information(1,3)=-sngl(dCB)*1.e-5
         information(2,3)=-sngl(dGB)*1.e-5
         information(2,1)=information(1,2)
         information(3,1)=information(1,3)
         information(3,2)=information(2,3)
      elseif(Nopt.eq.2)then
         if(.not.OptC)then
            score(1)=sngl(dG)-GAST
            score(2)=(sngl(dB)-EXPT)*1.e-5
            information(1,1)=-sngl(dGG)
            information(2,2)=-sngl(dBB)*1.e-10
            information(1,2)=-sngl(dGB)*1.e-5
            information(2,1)=information(1,2)
         elseif(.not.OptB)then
            score(1)=sngl(dC)-PSFT
            score(2)=sngl(dG)-GAST
            information(1,1)=-sngl(dCC)
            information(2,2)=-sngl(dGG)
            information(1,2)=-sngl(dCG)
            information(2,1)=information(1,2)
         else
            score(1)=sngl(dC)-PSFT
            score(2)=(sngl(dB)-EXPT)*1.e-5
            information(1,1)=-sngl(dCC)
            information(2,2)=-sngl(dBB)*1.e-10
            information(1,2)=-sngl(dCB)*1.e-5
            information(2,1)=information(1,2)
         endif
      else
         if(OptC)then
            score(1)=sngl(dC)-PSFT
            information(1,1)=-sngl(dCC)
         elseif(OptB)then
            score(1)=(sngl(dB)-EXPT)*1.e-5
            information(1,1)=-sngl(dBB)*1.e-10
         else
            score(1)=sngl(dG)-GAST
            information(1,1)=-sngl(dGG)
         endif
      endif
      RETURN
      END
