
      FUNCTION JETX_PSF(energy,Eps,r,Ierr)
C
CC  brightness of point spread function (not normalized)
C
CG
C   call_var.          type I/O description
CP  energy              R   I   Energy [keV]
CP  EPS			R   I	off-axis angle [arc min]
CP  r                   R   I   from target position [pixel]
C
CP  IERR                I   O   = 0 no error
CP                              = 1 energy outside bounds 0.07-3 keV
CP                              = 3
C
C     OUTPUT:
C             PSFOFF      brightness of point spread function
C
CX
C
C***********************************************************************
C
C   variables   meaning
C
cc      implicit none
      INTEGER ierr
      REAL*4 Eps,JETX_PSF
      REAL*4 r
      REAL*4 a_s1,b_s1,sigmag1,a_n1,b_n1,c_n1,normg1
      REAL*4 a_s2,c_s2,sigmag2,a_n2,normg2
      REAL*4 a_r,b_r,r_c,a_i,b_i,i_k,a_n,c_n,n_k
      REAL*4 energy,twopi,pixel_size
C
C
C      r = (sqrt(float(i-size/2)**2.+float(j-size/2)**2.)+0.5)
C
C      DATA          RTNAME /'PSFOFF'/
C
      Ierr = 0
C
C
C     pixel_size=1./8.
      pixel_size=2.
      twopi = 6.283185
cc
      a_s1=1.538926
      b_s1=-2.6256487e-2
      sigmag1=(a_s1+b_s1*energy)*pixel_size
c
      a_n1=1.1151133e-2
      b_n1=2.2278619e-3
      c_n1=-1.1809723e-4
      normg1=a_n1+b_n1*energy+c_n1*energy*energy
cc
      a_s2=4.678278
      c_s2=1.3267608e-2
      sigmag2=(a_s2+c_s2*energy*energy)*pixel_size
c
      a_n2=3.7314447e-3
      normg2=a_n2 
cc
      a_r=5.414726
      b_r=-0.2387667
      r_c=(a_r+b_r*energy)*pixel_size
c
      a_i=1.492605
      b_i=-2.0868517e-2
      i_k=a_i+b_i*energy
c
      a_n=5.6556705e-4
      c_n=2.960038e-4
      n_k=a_n+c_n*energy*energy
c
      JETX_PSF=normg1*exp(-R*R/(2.*sigmag1*sigmag1))
     &        +normg2*exp(-R*R/(2.*sigmag2*sigmag2))
     &        +n_k*(1.+(R/r_c)**2.)**(-i_k)
c
c      JETX_PSF = JETX_PSF/16.
      RETURN
      END





