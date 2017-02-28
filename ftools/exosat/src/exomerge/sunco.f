      SUBROUTINE SUNCO(ATT, SUN, SRA, SDE, ROLL, ICOUNT)
      IMPLICIT CHARACTER (A-Z)
C
C Taken from Interactive Analysis under /LE/lib/att.F
C

*
* Subroutine to recompute the exosat attitude matrix as function of
* star reference position and sun effective position
*
* ATT(3,3)  R*8  old attitude matrix
*                ATT(1,1),ATT(2,1),ATT(3,1) = star tracker X-axis
*                ATT(1,2),ATT(2,2),ATT(3,2) = star tracker Y-axis
*                ATT(1,3),ATT(2,3),ATT(3,3) = star tracker Z-axis
* SUN(3)    R*8  s/c to sun unit vector
* SRA       R*8  right ascension of reference star (radians)
* SDE       R*8  declination     of reference star (radians)
* ROLL      R*8  actual roll error measured by FSS (radians)
*                (positive when sun is below x-y plane)
* ATT(3,3)  R*8  updated attitude matrix  (two-way parameter!)
*
* ICOUNT     I   iteration count (for testing)
*
*
* SUBROUTINES CALLED
* DROTR  rotates a set of reference axes around an arbitrary axis
* DROTX  rotates a set of reference axes around one of them
* CMVHW1  move N 16-Bit words from 2nd argument to 3rd argument
*
*
* LOCAL VARIABLES
* FSSDAT(3) R*8  fine sun sensor data
*                (1) = misalignement in roll       (radians)
*                (2) = misalignement in pitch      (radians)
*                (3) = scale factor
* RLIM  minimum roll error correction on radians ( = 10 arcsec )
* STAR  star unit vector
* FSSR  fine sun sensor reference axes in inertial reference system
* SUNB  sun unit vector in FSS reference system
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION ROLL, SDE, SRA
      INTEGER*2 ICOUNT
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION ATT(3, 3), SUN(3)
*     ..
*     .. Local Scalars ..
      SAVE
      DOUBLE PRECISION DROLL, PROLL, RLIM
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION FSSDAT(3), FSSR(3, 3), STAR(3), SUNB(3)
*     ..
*     .. External Subroutines ..
      EXTERNAL DROTR, DROTX
c      external  CMVHW1
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC DABS, DATAN, DCOS, DSIN
*     ..
*     .. Data statements ..
*         = 9.8 & 1.7 ARC MINS
      DATA FSSDAT/.00285D0, .00049D0, 1.0D0/
      DATA RLIM/4.848D-5/
*     ..
*
*     if present convert star position into a unit vector (use SUNB)
*     if not assume star is on X-axis
*
      IF (SRA.EQ.0.D0 .AND. SDE.EQ.0.D0) THEN
c*
c*         this occurs during certain contingencies, but SUNCO
c*         will still behave correctly.
c*
c          CALL CMVHW1(12, ATT(1,1), SUNB)
         SUNB(1)=ATT(1,1)
         SUNB(2)=ATT(2,1)
         SUNB(3)=ATT(3,1)
      ELSE
*
*         normal case - valid star coordinates - convert
*
          SUNB(1) = DCOS(SDE)*DCOS(SRA)
          SUNB(2) = DCOS(SDE)*DSIN(SRA)
          SUNB(3) = DSIN(SDE)
      END IF
*
*     express star position in star tracker reference matrix
*
      STAR(1) = ATT(1, 1)*SUNB(1) + ATT(2, 1)*SUNB(2) +
     +          ATT(3, 1)*SUNB(3)
      STAR(2) = ATT(1, 2)*SUNB(1) + ATT(2, 2)*SUNB(2) +
     +          ATT(3, 2)*SUNB(3)
      STAR(3) = ATT(1, 3)*SUNB(1) + ATT(2, 3)*SUNB(2) +
     +          ATT(3, 3)*SUNB(3)
*
      ICOUNT = 0
*
   10 CONTINUE
      ICOUNT = ICOUNT + 1
*
*     initialise FSS reference matrix to star tracker reference matrix
*
c      CALL CMVHW1(36, ATT, FSSR)
      FSSR(1,1)=ATT(1,1)
      FSSR(2,1)=ATT(2,1)
      FSSR(3,1)=ATT(3,1)
      FSSR(1,2)=ATT(1,2)
      FSSR(2,2)=ATT(2,2)
      FSSR(3,2)=ATT(3,2)
      FSSR(1,3)=ATT(1,3)
      FSSR(2,3)=ATT(2,3)
      FSSR(3,3)=ATT(3,3)
*
*     apply misalignements in roll and pitch
*
      CALL DROTX(FSSR, FSSDAT(1), 1)
      CALL DROTX(FSSR, FSSDAT(2), 2)
*
*     compute predicted sun position in FSS reference
*
      SUNB(1) = FSSR(1, 1)*SUN(1) + FSSR(2, 1)*SUN(2) +
     +          FSSR(3, 1)*SUN(3)
      SUNB(2) = FSSR(1, 2)*SUN(1) + FSSR(2, 2)*SUN(2) +
     +          FSSR(3, 2)*SUN(3)
      SUNB(3) = FSSR(1, 3)*SUN(1) + FSSR(2, 3)*SUN(2) +
     +          FSSR(3, 3)*SUN(3)
*
*     derive predicted roll angle error ( = minus roll angle)
*
      PROLL = -DATAN(SUNB(3)/SUNB(2))
*
*     residual roll error = actual - predicted
*
      DROLL = ROLL*FSSDAT(3) - PROLL
*
*     if residual roll error is small enough return
*
      IF (DABS(DROLL).LT.RLIM .OR. ICOUNT.GE.10) RETURN
*
*     apply that correction to the initial attitude matrix with the
*     axe of rotation being along the star vector and iterate
*
      CALL DROTR(ATT, DROLL, STAR)
      GO TO 10
      END


      SUBROUTINE DROTR(XYZ, PHI, A)
      IMPLICIT CHARACTER (A-Z)
*
* Rotate around arbitrary axis by angle phi
*
* CALLING SEQUENCE
*
*    INPUT  XYZ     R8   (3,3)  XYZ(1,1),XYZ(2,1),XYZ(3,1) for X-axis
*                               XYZ(1,2),XYZ(2,2),XYZ(3,2) for Y-axis
*                               XYZ(1,3),XYZ(2,3),XYZ(3,3) for Z-axis
*           PHI     R8   (1)    rotation angle (rad)
*           A       R8   (3)    axis of rotation in s/c system
*                               of coordinates
*    OUTPUT XYZ     R8   (3,3)  X,Y,Z-axes after rotation
*

*     .. Scalar Arguments ..
      DOUBLE PRECISION PHI
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION A(3), XYZ(3, 3)
*     ..
*     .. Local Scalars ..
      SAVE
      DOUBLE PRECISION C, S
      INTEGER*2 I, J
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION W(3, 3), XYZN(3, 3)
*     ..
*     .. External Subroutines ..
      EXTERNAL MUL1
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC DCOS, DSIN
*     ..
      C = DCOS(PHI)
      S = DSIN(PHI)
      W(1, 1) = C + (1.-C)*A(1)*A(1)
      W(2, 1) = (1.-C)*A(2)*A(1) + S*A(3)
      W(3, 1) = (1.-C)*A(3)*A(1) - S*A(2)
      W(1, 2) = (1.-C)*A(1)*A(2) - S*A(3)
      W(2, 2) = C + (1.-C)*A(2)*A(2)
      W(3, 2) = (1.-C)*A(3)*A(2) + S*A(1)
      W(1, 3) = (1.-C)*A(1)*A(3) + S*A(2)
      W(2, 3) = (1.-C)*A(2)*A(3) - S*A(1)
      W(3, 3) = C + (1.-C)*A(3)*A(3)
      CALL MUL1(XYZ, W, XYZN)
      DO 20 I = 1, 3
          DO 10 J = 1, 3
              XYZ(I, J) = XYZN(I, J)
   10     CONTINUE
   20 CONTINUE
      RETURN
      END



      SUBROUTINE DROTX(A, FI, I)
      IMPLICIT CHARACTER (A-Z)
*
* ROTATES A SET OF AXES X,Y,Z AROUND ONE OF THEM BY AN ANGLE FI
*
* INPUT
* A      R8    3*3 A(1,1),A(2,1),A(3,1) for X-axis
*                  A(1,2),A(2,2),A(3,2) for Y-axis
*                  A(1,3),A(2,3),A(3,3) for Z-axis
* FI     R8      3 rotation angle in radians
* I      I4        rotation axis number (1 for axis ox)
*
* OUTPUT
* A      R8    3*3 X,Y,Z-axes after rotation
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION FI
      INTEGER I
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION A(3, 3)
*     ..
*     .. Local Scalars ..
      SAVE
      DOUBLE PRECISION CSF, SNF, U
      INTEGER*2 K, L, N
*     ..
*     .. Local Arrays ..
      INTEGER*2 J(4)
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC DCOS, DSIN
*     ..
*     .. Save statement ..
C     SAVE J
*     ..
*     .. Data statements ..
      DATA J/2, 3, 1, 2/
*     ..
      K = J(I)
      L = J(I+1)
      CSF = DCOS(FI)
      SNF = DSIN(FI)
      DO 10 N = 1, 3
          U = A(N, K)*CSF + A(N, L)*SNF
          A(N, L) = -A(N, K)*SNF + A(N, L)*CSF
          A(N, K) = U
   10 CONTINUE
      RETURN
      END


*
* Calculates the product OF TWO 3*3 matrices, i.e XYZN=XYZ*W
*
      SUBROUTINE MUL1(XYZ, W, XYZN)
      IMPLICIT CHARACTER (A-Z)
*
*     .. Array Arguments ..
      DOUBLE PRECISION W(3, 3), XYZ(3, 3), XYZN(3, 3)
*     ..
*     .. Local Scalars ..
      SAVE
      INTEGER*2 I, J, K
*     ..
      DO 30 I = 1, 3
          DO 20 J = 1, 3
              XYZN(I, J) = 0.D0
              DO 10 K = 1, 3
                  XYZN(I, J) = XYZ(I, K)*W(K, J) + XYZN(I, J)
   10         CONTINUE
   20     CONTINUE
   30 CONTINUE
      RETURN
      END


