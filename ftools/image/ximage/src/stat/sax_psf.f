      FUNCTION SAX_PSF(energy,Eps,r,Ierr)
C
CC  brightness of point spread function (normalized for original pixels)
C
CG
C   call_var.          type I/O description
CP  energy              R   I   Energy [keV]
CP  EPS			R   I	off-axis angle [arc min]
CP  r                   R   I   angle from target position [pixel]
C
CP  IERR                I   O   = 0 no error
CP                              = 1 energy outside bounds 0.07-3 keV
CP                              = 3
C
C     OUTPUT:
C             PSFOFF      brightness of point spread function
C
C   
C    19/6/1998 FT - normalization for original pixels (8 arcsec)
CX
C
C***********************************************************************
C
C   variables   meaning
C
      INTEGER ierr
      REAL*4 Eps, SAX_PSF
      REAL*4 r
      REAL*4 a_s,b_s,c_s,d_s,e_s,f_s
      REAL*4 a_m,b_m,c_m,d_m,e_m,f_m
      REAL*4 a_r,b_r,c_r,d_r,e_r,m_e,r_e,a_r1,b_r1
      REAL*4 sigma,r_l,d2,energy,twopi,pixel_size
C
C
C      r = (sqrt(float(i-size/2)**2.+float(j-size/2)**2.)+0.5)
C
C      DATA          RTNAME /'PSFOFF'/
C
      Ierr = 0
C
C
      pixel_size=20./8.
      twopi = 6.283185
      a_s=0.9544370
      b_s=0.8834696
      c_s=1.675199
      d_s=3.592193
      e_s=-0.1747078
      f_s=4.9475040E-03
      sigma=(c_s*exp((a_s-energy)/b_s)+
     &     d_s+e_s*energy+f_s*energy**2)*pixel_size
      a_r=0.2996073
      b_r=1.801236
      c_r=3.801108
      d_r=2.415506
      e_r=0.1802952
      r_l=(c_r*exp( (a_r-energy)/b_r )+
     &     d_r+e_r*energy)*pixel_size
      a_m=-1.822006
      b_m=3.988300
      c_m=1.480808
      d_m=0.7515454
      e_m=0.1139848
      f_m=-2.1994077E-03
      m_e=c_m*exp( (a_m-energy)/b_m ) +d_m +e_m*energy +f_m*energy**2.
      a_r1=-0.8180979
      b_r1=8.013765
      r_e=b_r1*energy**a_r1
      d2=r_l*r_l/(2.*(m_e-1.))
      SAX_PSF = 1./twopi/(r_e*sigma*sigma+d2)
     &     *( r_e*exp(-r**2./(2.*sigma**2.)) 
     &     +(1.+(r/r_l)**2.)**(-m_e) ) 
c normalization for original pixels (8 arcsec)
      SAX_PSF = SAX_PSF/64.
      RETURN
      END









