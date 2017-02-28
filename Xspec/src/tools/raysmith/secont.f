**==secont.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
      FUNCTION SECONT(B,Q)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL a , B , RS_GAMMA , Q , r , SECONT
C*** End of declarations inserted by SPAG
      a = B/(Q*Q)
      r = Q**.6666667
      IF ( a.LE.50.0 ) THEN
         SECONT = 1.0 + (.1728/r)*((.33333-2.*a)*RS_GAMMA(.33333,a)+1.0)
     &            - (.0496/(r*r))
     &            *(.66667*(1.-a-3.*a*a)*RS_GAMMA(.66667,a)+(1.+2.*a))
      ELSE
         SECONT = (1.0-.1728/r) - 0.0496/(r*r)
      ENDIF
      RETURN
      END
 
