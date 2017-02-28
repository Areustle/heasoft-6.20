**==view.spg  processed by SPAG 4.50J  at 16:32 on 17 Oct 1998
c VIEW.FOR
c
      SUBROUTINE VIEW(Day0,Ivw,Ra50,Dc50,Day,Angl)
c
c
c  This routine for a given scan (Ivw), day (Day0)
c  and sky position (RA50,DC50) finds day and scan angle 
c  when that position is in FOV.
c
c Variable  
c Day0   I r*4  Initial day number day since 1977 input paramater
c Ivw    I i    Which scan to use 1st or 2nd  input paramater  
c Ra50   I r*8  RA  in 1950 of object 
c Dc50   I r*8  Dec in 1950 of object
c Day    o r*4  Day in the FOV since 1977    
c Angl   o r*4  Scan angle 
c
      INTEGER Ivw
      REAL*4 Day , Day0, angl
      REAL*8 Ra50 , Dc50
c
c
      INTEGER i, icnt 
      REAL a1, a2, angl0, angl1, b1, b2, rtd, sunlon, tim0, tim1
      REAL*8 ecln, eclt, ra78, dc78
      LOGICAL*1 qp
      DIMENSION tim0(3) , tim1(3)
      DATA rtd/57.296/
c
      Day = 10000.
      Angl = 0.
c
c    Convert day and scan angle
c
      CALL UP5078(Ra50,Dc50,ra78,dc78)
      CALL RATOEC(ra78,dc78,ecln,eclt)
c
c  Try sunlon -90 
c
      sunlon = ecln - 90.
      angl0 = eclt
      CALL SUNDAY(sunlon/rtd,tim0)
c
c  Try the other sunlon +90 
c
      sunlon = ecln + 90.
      CALL SUNDAY(sunlon/rtd,tim1)
      angl1 = 180. - eclt
      qp = tim0(1).LT.tim1(1)
      icnt = 0
      DO i = 1 , 3
         IF ( qp ) THEN
            a1 = tim0(i)
            a2 = tim1(i)
            b1 = angl0
            b2 = angl1
         ELSE
            a1 = tim1(i)
            a2 = tim0(i)
            b1 = angl1
            b2 = angl0
         ENDIF
         IF ( a1.GE.Day0 ) THEN
            icnt = icnt + 1
            IF ( icnt.GE.Ivw ) THEN
               Day = a1
               Angl = b1
               GOTO 100
            ENDIF
         ENDIF
         IF ( a2.GE.Day0 ) THEN
            icnt = icnt + 1
            IF ( icnt.GE.Ivw ) THEN
               Day = a2
               Angl = b2
               GOTO 100
            ENDIF
         ENDIF
      ENDDO
 100  IF ( Angl.LT.0. ) Angl = Angl + 360.
      RETURN
      END
