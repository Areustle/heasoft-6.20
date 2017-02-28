      SUBROUTINE compls(ear,ne,param,ifl, photar, photer)

      INTEGER ne, ifl
      REAL ear(0:ne),param(2),photar(ne), photer(ne)

c
c **      ** subroutine modified for XSPEC
c **      ** fwj haberl 28 july 1986
c
c **      ** CALCULATES A COMPTONIZATION SPECTRUM AFTER LAMB ANS SANFORD 1983
c **      ** see ADDMOD for parameter descriptions
c         number of parameters: 2
c            1      temperature
c            2      optical depth
C---
      REAL tkt, tau, at, emid, ek2, ek, ek3, pk, qk, eat
      REAL fac, bre, therm, acompls, edelt
      INTEGER i

c suppress a warning message from the compiler
      i = ifl

c this model does not calculate errors

      DO i = 1, ne
         photer(i) = 0.0
      ENDDO

      tkt=param(1)
      tau=param(2)

      AT=2*TKT/511.*TAU*TAU

      DO i=1,ne
         EMID=(ear(i-1)+ear(i))/2.
         EK=EMID/TKT
         EK2=EK*EK
         EK3=EK2*EK
         PK=1.5*(EK3-EK2-EK)/AT
         QK=1.5*EK2/AT-0.75*EK3/AT
         EAT=0
         IF(AT.LT.170.)EAT=EXP(-AT)
         FAC=(1.+0.375*EK3*AT-PK*(EAT-1.+AT)+QK*(EAT*(AT+1.)-1.))
         BRE=THERM(EMID,TKT)
         aCOMPLS=FAC*BRE
         edelt=ear(i)-ear(i-1)
         photar(i)=edelt*acompls
      ENDDO

      RETURN
      END
