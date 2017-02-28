C----------------------------------------------------------------------
C This subroutine transforms galactic coordinates into 1950 celestial
C  coordinates.  It is based on the Fortran routine TRANSRAD by the
C  same author.  
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Vers 1.0  ?? ??? ????  Keith Scollick's TRANSRAD IDL routine for SkyView
C       2.0   5 Jul 1994  JSA's conversion from IDL to FORTRAN
C       3.0   8 Dec 1994  Stripped down to galactic -> celestial only,
C                          external calculation of xa, ya, za elements

       subroutine gal2cel (xa, ya, za, xb, yb, zb)

       double precision xa, ya, za, xb, yb, zb
       double precision phi, theta, psi
       double precision lambda(3,3)

       phi = 4.9261918136D0
       theta = 1.09257611D0
       psi = 5.707226433D0
       call matrix(phi, theta, psi, lambda)
       
       xb = (xa*lambda(1,1)) + (ya*lambda(2,1)) + (za*lambda(3,1))
       yb = (xa*lambda(1,2)) + (ya*lambda(2,2)) + (za*lambda(3,2))
       zb = (xa*lambda(1,3)) + (ya*lambda(2,3)) + (za*lambda(3,3))

       return

       end

C----------------------------------------------------------------------
C This subroutine performs coordinate precession.  As with gal2cel,
C  it has been stripped from TRANSRAD to suit this FTOOL.

       subroutine precs(xa, ya, za, yrin, yrout, xb, yb, zb)

       double precision xa, ya, za, xb, yb, zb
       double precision yrin, yrout
       double precision plambda(3,3)

       call pmatrix(yrin, yrout, plambda)

       xb = (xa*plambda(1,1)) + (ya*plambda(2,1)) + (za*plambda(3,1))
       yb = (xa*plambda(1,2)) + (ya*plambda(2,2)) + (za*plambda(3,2))
       zb = (xa*plambda(1,3)) + (ya*plambda(2,3)) + (za*plambda(3,3))

       return

       end

C---------------------------------------------------------------------
C   Eq. 4-46 in "4-4 The Eulerian Angles" from pg. 109 of Goldstein's
C 'Classical Mechanics' (1950).

       subroutine matrix(phi, theta, psi, lambda)

       double precision psi, phi, theta
       double precision lambda(3,3)

       lambda(1,1) = dcos(psi)*dcos(phi) - 
     +                dcos(theta)*dsin(phi)*dsin(psi)
       lambda(1,2) = dcos(psi)*dsin(phi) + 
     +                dcos(theta)*dcos(phi)*dsin(psi)
       lambda(1,3) = dsin(psi)*dsin(theta)

       lambda(2,1) = -dsin(psi)*dcos(phi) - 
     +                 dcos(theta)*dsin(phi)*dcos(psi) 
       lambda(2,2) = -dsin(psi)*dsin(phi) + 
     +                 dcos(theta)*dcos(phi)*dcos(psi)
       lambda(2,3) = dcos(psi)*dsin(theta)

       lambda(3,1) = dsin(theta)*dsin(phi)
       lambda(3,2) = -dsin(theta)*dcos(phi)
       lambda(3,3) = dcos(theta)

       return

       end

C----------------------------------------------------------------------
C   "The Rigorous Method for General Precession" from pg. 23-25 of 
C Taff's 'Computational Spherical Astronomy' (1978).
C
C Taff's precession quantities are represented by:
C       zeta_0 -> a, z -> b, theta -> c
C
C Author: Jesse Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Vers. 1.0  05 Jul 1994  Based on Keith Scollick's IDL routine, checked
C                           with Taff (using Taff's constants)
C        2.0  19 Dec 1994  Reversed epochs to correct direction of precession
C        2.1  20 Dec 1994  Matrix transposed: Taff precesses by
C                           P x C rather than C x P (epochs restored)

       subroutine pmatrix(epoch1, epoch2, plambda)

       double precision cdr, csr
       parameter (cdr = 0.1745329519943D-1)
       parameter (csr = cdr / 3600.D0)

       double precision epoch1, epoch2
       double precision t_i, t_f, a, b, c
       double precision plambda(3,3)

       t_f = 0.01D0 * (epoch2 - epoch1)
       t_i = 0.01D0 * (epoch2 - 1900.D0)

        a = csr*(2304.253D0+(1.3975D0*t_i)+(0.00006D0*(t_i**2)))*t_f
     +      + csr*(0.3023D0 - (0.0027D0*t_i))*(t_f**2) 
     +      + csr*(0.01800D0*(t_f**3))
       b = a + csr*(0.7927D0 + (0.00066D0*t_i))*(t_f**2)
     +       + csr*(0.00032D0*(t_f**3))
       c = csr*(2004.685D0-(0.8533D0*t_i)-(0.00037D0*(t_i**2)))*t_f
     +      + csr*(-0.4267D0 + (0.00037D0*t_i))*(t_f**2)
     +      - csr*(0.04180D0*(t_f**3))

       plambda(1,1) = dcos(a)*dcos(b)*dcos(c) - dsin(a)*dsin(b)
       plambda(2,1) = -dsin(a)*dcos(b)*dcos(c) - dcos(a)*dsin(b) 
       plambda(3,1) = -dcos(b)*dsin(c)

       plambda(1,2) = dcos(a)*dsin(b)*dcos(c) + dsin(a)*dcos(b)
       plambda(2,2) = -dsin(a)*dsin(b)*dcos(c) + dcos(a)*dcos(b)
       plambda(3,2) = -dsin(b)*dsin(c)

       plambda(1,3) = dcos(a)*dsin(c)
       plambda(2,3) = -dsin(a)*dsin(c)
       plambda(3,3) = dcos(c)

       return

       end
