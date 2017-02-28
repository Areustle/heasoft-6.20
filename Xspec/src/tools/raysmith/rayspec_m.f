**==rayspec_m.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
      SUBROUTINE RAYSPEC_M(T0,Dene0,Frac0,Nbin0,Binmin0,Binsyz0,Idens0,
     &                     Icx0,Bin0)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL ABIn , ABUnd , ABUnj , be , BIN , BINmin , Binmin0 , BINsyz , 
     &     Binsyz0 , CION , cionh , CNC , CONce , crec , dene , Dene0 , 
     &     DNE , GNDrec , HENeut , HEPlus
      INTEGER i , icx , Icx0 , idens , Idens0 , iprint , 
     &        jprint , NBIn , Nbin0 , NJ , num
      REAL PCOol , PM , POT , POU , POWer , PTOt , RE , RF , 
     &     RHY , t , T0 , TAU , TAUhe , TPLus , TU
C*** End of declarations inserted by SPAG
C          Subroutine to compute cooling from a hot diffuse gas.
C
c *****************************************************************
c *                                                               *
c *  To call this subroutine use the following :                  *
c *     t is the log10 of the temp. (K)                           *
c *     dene is the log10 electron density (cm^-3)                *
c *     frac is the fraction of solar abundance for the metals    *
c *     Nbin is the number of energy bins required                *
c *     binmin is the energy of the low edge of the lowest bin (eV)*
c *     binsyz is the size of the bins (eV)                       *
c *     Idens is 0 (or 1 for density dependent dielectric recomb  *
c *     Icx is 0 (or 1 for charge transfer)                       *
c *     bin is the name of the array in which the spectrum is to  *
c * be returned. It should have size Nbin. The quantity returned  *
c * is the total emissivity in that bin in units of               *
c * 10^-23 n_e n_H erg cm^3 s^-1 bin^-1                           *
c *                                                               *
c *****************************************************************
 
      INCLUDE 'rayspec.inc'
 
c	First a few common blocks.
      COMMON /PARAMS/ NJ(12) , ABUnj(12) , ABUnd , BINmin , BINsyz , 
     &                NBIn
c	For conversion to wavelength bins.
c	Output?
      COMMON /RESULT/ CONce(30) , GNDrec(30) , POWer(220) , RHY , 
     &                HENeut , HEPlus , DNE , PCOol , POU , POT , RE , 
     &                TU , PM(4)
c	Photoionisation.
      COMMON /PT    / RF(500) , TAU(150) , TAUhe(150) , TPLus(150)
      COMMON /COM   / CNC(12,30) , PTOt(12,220) , ABIn(MXBINS) , 
     &                BIN(MXBINS)
      REAL frac(12)
      REAL Bin0(Nbin0) , Frac0(12)

      INTEGER ilun

      CHARACTER(255) atfile

      INTEGER lenact
      CHARACTER(255) trfnam
      EXTERNAL lenact, trfnam

      DATA RF/500*1.0/

c Element Abundances from Anders & Grevesse (with H defined to be 12.00)
 
      ABUnj(1) = 11.00
      ABUnj(2) = 8.56
      ABUnj(3) = 8.05
      ABUnj(4) = 8.93
      ABUnj(5) = 8.09
      ABUnj(6) = 7.58
      ABUnj(7) = 7.55
      ABUnj(8) = 7.21
      ABUnj(9) = 6.56
      ABUnj(10) = 6.36
      ABUnj(11) = 7.67
      ABUnj(12) = 6.25
c
c     Initialise all variables
      t = T0
      dene = Dene0
      DO 100 i = 1 , 12
         frac(i) = Frac0(i)
 100  CONTINUE
      NBIn = Nbin0
      BINmin = Binmin0
      BINsyz = Binsyz0
      idens = Idens0
      icx = Icx0
c
      num = 12
      iprint = 0
      jprint = 0
c

      atfile = trfnam('$HEADAS,../spectral/modelData,atomic.data')
      CALL getlun(ilun)
      OPEN (UNIT=ilun,FILE=atfile(:lenact(atfile)),STATUS='old')
      CALL ATREAD(ilun,num)
      CLOSE (UNIT=ilun)
      CALL frelun(ilun)

C
c              Modify Abundances
      DO 200 i = 1 , 12
         frac(i) = ALOG10(frac(i))
         ABUnj(i) = ABUnj(i) + frac(i)
 200  CONTINUE
c
      t = 10.**t
      dene = 10.**dene
C COMPUTE RATIO OF IONIZED TO NEUTRAL HYDROGEN
      cionh = CION(1,1,13.6,t)
      be = 157890./t
      crec = 2.06E-11*(.4288+.5*ALOG(be)+.469*be**(-.33333))/SQRT(t)
      RHY = cionh/crec
C
C
      DO 300 i = 1 , NBIn
         BIN(i) = 0.
 300  CONTINUE
c
      CALL FELINE(t,dene,num,iprint,jprint,0,0,idens,icx)
c
      DO 400 i = 1 , NBIn
         Bin0(i) = BIN(i)
 400  CONTINUE
c
      RETURN
      END
