 
 
 
      SUBROUTINE VDOT(Product,Vector1,Istart1,Incr1,Vector2,Istart2,
     &                Incr2,Nelements)
      IMPLICIT NONE
c
c   VAX version of VDOT subroutine on HP 1000.
c   (see: VIS user manual, p.2-19)
c   VDOT calculates the inner product between VECTOR1 and
c   VECTOR2 and stores it in variable PRODUCT.
c   This version only supports one-dimensional arrays
c   with INCR1=+/-1 and INCR2=+/-1.
c   If INCRx = -1 the multiplication is done in reversed order of elements.
c   ISTART1 and ISTART2 are starting positions for VECTOR1 and
c   VECTOR2, respectively.
c
      INCLUDE '../include/io.inc'
      REAL*4 Product , Vector1(512) , Vector2(512)
      INTEGER*4 Istart1 , Incr1 , Istart2 , Incr2 , Nelements , i
c
c  check values for INCRx
c
      IF ( Incr1.EQ.1 .OR. Incr1.EQ.-1 ) THEN
         IF ( Incr2.EQ.1 .OR. Incr2.EQ.-1 ) THEN
c
            Product = 0.0
c
            IF ( Incr1.EQ.1 .AND. Incr2.EQ.1 ) THEN
               DO 10 i = 1 , Nelements
                  Product = Product + Vector1(Istart1+i-1)
     &                      *Vector2(Istart2+i-1)
 10            CONTINUE
            ENDIF
c
            IF ( Incr1.EQ.1 .AND. Incr2.EQ.-1 ) THEN
               DO 20 i = 1 , Nelements
                  Product = Product + Vector1(Istart1+i-1)
     &                      *Vector2(Istart2+1-i)
 20            CONTINUE
            ENDIF
c
            IF ( Incr1.EQ.-1 .AND. Incr2.EQ.1 ) THEN
               DO 30 i = 1 , Nelements
                  Product = Product + Vector1(Istart1+1-i)
     &                      *Vector2(Istart2+i-1)
 30            CONTINUE
            ENDIF
c
            IF ( Incr1.EQ.-1 .AND. Incr2.EQ.-1 ) THEN
               DO 40 i = 1 , Nelements
                  Product = Product + Vector1(Istart1+1-i)
     &                      *Vector2(Istart2+1-i)
 40            CONTINUE
            ENDIF
c
            RETURN
         ENDIF
      ENDIF
c
c   error condition
c
      WRITE (ZWRite,99001)
      CALL XWRITE(ZWRite,10)
      STOP
99001 FORMAT (t5,' >>> VDOT: wrong increment')
      END
