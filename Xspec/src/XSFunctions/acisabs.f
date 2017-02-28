      SUBROUTINE acisabs(ear, ne, param, ifl, photar, photer)

      INTEGER ne, ifl
      REAL ear(0:ne), param(8), photar(ne), photer(ne)       

c***       Ver 1.1 - Modified the previous ver. of acisabs.f
c***       to use exponential decay instead of linear

c      FORTRAN version of xcont.pro, written by George Chartas
c      Conversion performed by Konstantin Getman 07/17/02
c
c                  THE CONTENT OF xcont.pro      
c       (1) Use observed decay with time in the ratio of observed 
c       X-ray transmissions at 0.67keV and 5.895keV.
c        Observed decay in S3 based on Catherine Grants' analysis:
c        Fit parameters provided by Allyn Tennant
c
c        R(t) = (norm*exp(-tauinf*(1.0-exp(-t/tefold))))       eq(1)
c          norm=0.00722+/-0.00007
c          tefold=620.+/-66.
c          tauinf=0.582+/-0.024
c        
c       
c       (2) Model decay
c
c       Trans(E1,t) = TOBF(E1) * Tcont(E1,t)   eq(2)
c
c       where  TOBF(E1) is the transmission of the OBF at energy E1
c        Tcont(E1,t) is the transmission of the contamination
c        layer at energy E1 and time t
c
c       The modeled decay is:
c
c        R(t)=Trans(E1,t)/Trans(E2,t)=const*exp{-rho*d(t)*[mac(E1)-mac(E2)]} eq(3)
c
c
c        where : rho is the density of the contaminant
c        d(t) is the thickness of the contaminant
c        mac(E) is the mass absorption coefficient of the 
c        contaminant at energy E.
c        mac(E) calculated from atomic scattering factor files
c        provided at http://www-cxro.lbl.gov/optical_constants/asf.html
c
c      The plot of the thickness of the contaminant vs. time is 
c       derived form eq(1) and eq(3):
c
c       d(t)*rho = ln(R(t)/const) / [mac(E2) - mac(E1)] 
c
c
c
c      (3) Suggested modification of ACIS arf files to account for contamination
c
c       Tcont(E,t) = exp(-mac(E)*rho*d(t))
c
c------------------------------------------------------------------------      
c                  THE CONTENT OF acisabs xspec model
c      Number of model parameters: 7
c      1      Tdays      Days between launch and observation
c       2       norm    Normalization factor
c      3      tauinf      Optical depth at infinite time
c      4      tefold      e-folding time for build-up of contaminant 
c      5      nC      Number of carbon atoms in hydrocarbon molecule
c      6      nH      Number of hydrogen atoms in hydrocarbon molecule
c      7      nO      Number of oxygen atoms in hydrocarbon molecule
c      8      nN      Number of nitrogen atoms in hydrocarbon molecule
c      
c      THIS PROGRAM REQUIRES the following data files to be present in the 
c       xspec/manager directory :
c      c.nff, h.nff, o.nff, n.nff
c

c Declaration      

      integer nmax, numaxC, numaxO, numaxH,numaxN, i
      real awC, awO, awH, awN, Mac, MacE1, MacE2, thRho, decay, log
      real Tdays, nC, nH, nO, nN 
      parameter (nmax=1000, Ecal1=0.67, Ecal2=5.895, awC=12.01115,
     + awO=15.9994, awH=1.00797, awN=14.0067)
      real tauinf, tefold, norm,  muO(nmax), muH(nmax),
     + muC(nmax), muN(nmax), enO(nmax), enN(nmax), enH(nmax), enC(nmax),
     + xO, xN, xH, xC, elnorm, eninpt
      logical qfirst

      save qfirst, numaxO, enO, muO, numaxH, enH, muH
      save numaxN, enN, muN, numaxC, enC, muC

      data qfirst / .true. /

c Suppress a warning message about not using ifl
      i = ifl

c This model does not have errors

      DO i = 1, ne
         photer(i) = 0.0
      ENDDO

c Initialization      
      Tdays = param(1)
      norm = param(2)
      tauinf = param(3)
      tefold = param(4)      
      nC = param(5)
      nH = param(6)
      nO = param(7)
      nN = param(8)
      
c Read data
      IF ( qfirst ) THEN
         CALL READata ('o',awO,numaxO,enO,muO)
         CALL READata ('h',awH,numaxH,enH,muH)
         CALL READata ('n',awN,numaxN,enN,muN)
         CALL READata ('c',awC,numaxC,enC,muC)
         qfirst = .FALSE.
      ENDIF
                 
c Calculate the relative abundance of each element
      xO = nO * awO
      xH = nH * awH
      xN = nN * awN
      xC = nC * awC
      elnorm = xO + xH + xN + xC
      xO = xO/elnorm
      xH = xH/elnorm
      xN = xN/elnorm
      xC = xC/elnorm

c Calculate mass absorption coefficient at Ecal1 and Ecal2
      MacE1 = xO*Mac(numaxO,enO,muO,Ecal1)+xH*Mac(numaxH,enH,muH,Ecal1)
     ++xN*Mac(numaxN,enN,muN,Ecal1)+xC*Mac(numaxC,enC,muC,Ecal1)
      MacE2 = xO*Mac(numaxO,enO,muO,Ecal2)+xH*Mac(numaxH,enH,muH,Ecal2)
     ++xN*Mac(numaxN,enN,muN,Ecal2)+xC*Mac(numaxC,enC,muC,Ecal2)

c Estimate thickness of contaminate during observation 
      decay = norm*exp(-tauinf*(1.0 - exp(-Tdays/tefold)))
      thRho = log(decay/norm)/(MacE2 - MacE1)

c Calculate transmission of contamination layer for input energies
      DO 105 i = 1, ne
      eninpt = (ear(i-1) + ear(i))/2.        
        photar(i) = exp (-(
     + xO*Mac(numaxO,enO,muO,eninpt)+xH*Mac(numaxH,enH,muH,eninpt)
     + +xN*Mac(numaxN,enN,muN,eninpt)+xC*Mac(numaxC,enC,muC,eninpt
     + ))*thRho)
  105 CONTINUE

      RETURN
      END

c Additional Subroutines and Functions
c-----------------------------------------
c     This one reads f2 and E from nff file
c     and calculates mu(energy) array for the specified element

      SUBROUTINE READata (elem,aw,numax,engrid,mu)

       implicit none
       integer numax, nnmax, ilun, lenn, ios
       character(1) elem
       CHARACTER(300) contxt, fl_name
       PARAMETER (nnmax=1000)
       real aw, engrid(nnmax), mu(nnmax)

       INTEGER lenact
       CHARACTER(128) fgmodf
       EXTERNAL lenact, fgmodf

       CALL getlun(ilun)
       fl_name = fgmodf()
       lenn = lenact(fl_name)
       fl_name = fl_name(:lenn) // elem // '.nff'
       ios = 0
       numax=0

       CALL openwr(ilun, fl_name, 'old', ' ', ' ', 0, 0, ios)
       IF ( ios .NE. 0 ) THEN
          contxt = 'Failed to open '//fl_name(:lenact(fl_name))
          CALL xwrite(contxt, 10)
          CALL frelun(ilun)
          RETURN
       ENDIF

  15   numax=numax+1
       READ(ilun,'(E12.6, 1X, E12.6)',END=25) engrid(numax),mu(numax)      
        mu(numax) = 4.208e7 * mu(numax) / engrid(numax) / aw 
       GOTO 15
  25   CONTINUE
       numax=numax-1

       CLOSE (ilun)
       CALL frelun(ilun)

      RETURN
      END
c---------------------------------------

c---------------------------------------
c Function performs linear interpolation, calculates 
c mass absorption coefficient for specified energy enkeV      
      FUNCTION Mac(N,engrid,mu,enkeV)
        integer N
        REAL engrid(N),mu(N),enkeV, eneV, Mac
        eneV = 1000. * enkeV
        DO 100 i = 1, N-1
          if ((eneV.GE.engrid(i)) .AND. (eneV.LE.engrid(i+1))) GOTO 101 
  100   CONTINUE
  101   Mac = mu(i+1) - (engrid(i+1)-eneV)*(mu(i+1)-mu(i))/
     +  (engrid(i+1)-engrid(i))
      RETURN
      END
c---------------------------------------      
