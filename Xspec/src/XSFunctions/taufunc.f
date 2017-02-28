      REAL FUNCTION Taufunc(energy)
      DOUBLE PRECISION energy, EineV, enrgeV, sigmalog, sigma10
      DOUBLE PRECISION fit1(3), fit2(5), fit3(3), fit4(2), fit5(2)
      DOUBLE PRECISION fit6(3), fit7(4), fit8(3), fit9(4), fit10(3)
      DOUBLE PRECISION fit11(3), fit12(3), fit13(4), fit14(3), fit15(4) 
      DOUBLE PRECISION fit16(3), fit17(3), fit18(4)
      DOUBLE PRECISION fit19(3), fit20(3), fit21(4)

      DATA fit1/ - 0.21928968D04, 0.87726511D04, -0.87911559D04/
      DATA fit2/ - 0.32539665D02, 0.28441119D03, -0.9327074D03,
     &     0.13615747D04, -0.7644055D03/
      DATA fit3/0.4533806D04, -0.22190636D05, 0.27135973D05/
      DATA fit4/ - 33.8527, 66.2578/
      DATA fit5/16.19258, -56.5496/
      DATA fit6/0.21215426D04, -0.10427483D05, 0.12796081D05/
      DATA fit7/ - 0.43122084D01, 0.28679637D02, -0.60724145D02,
     &     0.23208073D02/
      DATA fit8/ - 0.75251758D03, 0.40846885D04, -0.55595149D04/
      DATA fit9/0.31471058D03, -0.26448742D04, 0.74097558D04,
     &     -0.6936511D04/
      DATA fit10/ - 0.20596737D03, 0.11694643D04, -0.16764801D04/
      DATA fit11/ - 0.56210932D02, 0.32646566D03, -0.49040269D03/
      DATA fit12/0.13275885D02, -0.76195773D02, 0.92925391D02/
      DATA fit13/ - 0.48526061D01, 0.41041298D02, -0.11466505D03,
     &     0.89339906D02/
      DATA fit14/ - 0.35969152D04, 0.22386128D05, -0.34847439D05/
      DATA fit15/0.34490094D02, -0.33430332D03, 0.10800713D04,
     &     -0.11793778D04/
      DATA fit16/ - 0.9679292D03, 0.63110749D04, -0.10303588D05/
      DATA fit17/ - 0.70948446D02, 0.46641703D03, -0.78279389D03/
      DATA fit18/0.35179443, -0.40310504D01, 0.15307437D02,
     &     -0.35484683D02/
      DATA fit19/ - 0.3233016D03, 0.24855057D04, -0.47933023D04/
      DATA fit20/ - 0.62333109D02, 0.4830931D03, -0.9522382D03/
      DATA fit21/0.89462024D01, -0.10645717D03, 0.42234393D03,
     &     -0.57482295D03/

c       These fits are polynomial fits to scattering cross section data
c       provided by P. Martin. The x-axis is in units of log Energy (in eV)
c       and the y-axis is in terms of log (sigma * (E**2)). I used 21
c       segments, which I know might be overkill, but I wanted smooth regions
c       to fit. The fits turned out quite well, with the percentage error
c       between data and fits never being greater than 0.5%.

      EineV = energy*1000
      enrgeV = dlog10(EineV)
      taufunc = 0.0

c       Fit for Segment 1
      IF ((enrgeV.GT.1.999) .AND. (enrgeV.LT.2.002906)) THEN
         sigmalog = (fit1(1)*(enrgeV**2)) + (fit1(2)*enrgeV) + fit1(3)
         sigma10 = 10**(sigmalog)
         taufunc = SNGL(sigma10/(EineV**2))

c       Fit for Segment 2
      ELSEIF ((enrgeV.GT.2.002906) .AND. (enrgeV.LT.2.444922)) THEN
         sigmalog = (fit2(1)*(enrgeV**4)) + (fit2(2)*(enrgeV**3))
     &               + (fit2(3)*(enrgeV**2)) + (fit2(4)*enrgeV)
     &               + fit2(5)
         sigma10 = 10**(sigmalog)
         taufunc = SNGL(sigma10/(EineV**2))

c       Fit for Segment 3
      ELSEIF ((enrgeV.GT.2.444922) .AND. (enrgeV.LT.2.452919)) THEN
         sigmalog = (fit3(1)*(enrgeV**2)) + (fit3(2)*enrgeV) + fit3(3)
         sigma10 = 10**(sigmalog)
         taufunc = SNGL(sigma10/(EineV**2))

c       Fit for Segment 4
      ELSEIF ((enrgeV.GT.2.452919) .AND. (enrgeV.LT.2.453924)) THEN
         sigmalog = (fit4(1)*enrgeV) + fit4(2)
         sigma10 = 10**(sigmalog)
         taufunc = SNGL(sigma10/(EineV**2))

c       Fit for Segment 5
      ELSEIF ((enrgeV.GT.2.453924) .AND. (enrgeV.LT.2.454921)) THEN
         sigmalog = (fit5(1)*enrgeV) + fit5(2)
         sigma10 = 10**(sigmalog)
         taufunc = SNGL(sigma10/(EineV**2))

c       Fit for Segment 6
      ELSEIF ((enrgeV.GT.2.454921) .AND. (enrgeV.LT.2.457917)) THEN
         sigmalog = (fit6(1)*(enrgeV**2)) + (fit6(2)*enrgeV) + fit6(3)
         sigma10 = 10**(sigmalog)
         taufunc = SNGL(sigma10/(EineV**2))

c       Fit for Segment 7
      ELSEIF ((enrgeV.GT.2.457917) .AND. (enrgeV.LT.2.710927)) THEN
         sigmalog = (fit7(1)*(enrgeV**3)) + (fit7(2)*(enrgeV**2))
     &               + (fit7(3)*enrgeV) + fit7(4)
         sigma10 = 10**(sigmalog)
         taufunc = SNGL(sigma10/(EineV**2))

c       Fit for Segment 8
      ELSEIF ((enrgeV.GT.2.710927) .AND. (enrgeV.LT.2.72591)) THEN
         sigmalog = (fit8(1)*(enrgeV**2)) + (fit8(2)*enrgeV) + fit8(3)
         sigma10 = 10**(sigmalog)
         taufunc = SNGL(sigma10/(EineV**2))

c       Fit for Segment 9
      ELSEIF ((enrgeV.GT.2.72591) .AND. (enrgeV.LT.2.831911)) THEN
         sigmalog = (fit9(1)*(enrgeV**3)) + (fit9(2)*(enrgeV**2))
     &               + (fit9(3)*enrgeV) + fit9(4)
         sigma10 = 10**(sigmalog)
         taufunc = SNGL(sigma10/(EineV**2))

c       Fit for Segment 10
      ELSEIF ((enrgeV.GT.2.831911) .AND. (enrgeV.LT.2.857913)) THEN
         sigmalog = (fit10(1)*(enrgeV**2)) + (fit10(2)*enrgeV)
     &               + fit10(3)
         sigma10 = 10**(sigmalog)
         taufunc = SNGL(sigma10/(EineV**2))

c       Fit fot Segment 11
      ELSEIF ((enrgeV.GT.2.857913) .AND. (enrgeV.LT.2.910926)) THEN
         sigmalog = (fit11(1)*(enrgeV**2)) + (fit11(2)*enrgeV)
     &               + fit11(3)
         sigma10 = 10**(sigmalog)
         taufunc = SNGL(sigma10/(EineV**2))

c       Fit for Segment 12
      ELSEIF ((enrgeV.GT.2.910926) .AND. (enrgeV.LT.2.931912)) THEN
         sigmalog = (fit12(1)*(enrgeV**2)) + (fit12(2)*enrgeV)
     &               + fit12(3)
         sigma10 = 10**(sigmalog)
         taufunc = SNGL(sigma10/(EineV**2))

c       Fit for Segment 13
      ELSEIF ((enrgeV.GT.2.931912) .AND. (enrgeV.LT.3.11092)) THEN
         sigmalog = (fit13(1)*(enrgeV**3)) + (fit13(2)*(enrgeV**2))
     &               + (fit13(3)*enrgeV) + fit13(4)
         sigma10 = 10**(sigmalog)
         taufunc = SNGL(sigma10/(EineV**2))

c       Fit for Segment 14
      ELSEIF ((enrgeV.GT.3.11092) .AND. (enrgeV.LT.3.114918)) THEN
         sigmalog = (fit14(1)*(enrgeV**2)) + (fit14(2)*enrgeV)
     &               + fit14(3)
         sigma10 = 10**(sigmalog)
         taufunc = SNGL(sigma10/(EineV**2))

c       Fit for Segment 15
      ELSEIF ((enrgeV.GT.3.114918) .AND. (enrgeV.LT.3.25892)) THEN
         sigmalog = (fit15(1)*(enrgeV**3)) + (fit15(2)*(enrgeV**2))
     &               + (fit15(3)*enrgeV) + fit15(4)
         sigma10 = 10**(sigmalog)
         taufunc = SNGL(sigma10/(EineV**2))

c       Fit for Segment 16
      ELSEIF ((enrgeV.GT.3.25892) .AND. (enrgeV.LT.3.264919)) THEN
         sigmalog = (fit16(1)*(enrgeV**2)) + (fit16(2)*enrgeV)
     &               + fit16(3)
         sigma10 = 10**(sigmalog)
         taufunc = SNGL(sigma10/(EineV**2))

c       Fit for Segment 17
      ELSEIF ((enrgeV.GT.3.264919) .AND. (enrgeV.LT.3.290916)) THEN
         sigmalog = (fit17(1)*(enrgeV**2)) + (fit17(2)*enrgeV)
     &               + fit17(3)
         sigma10 = 10**(sigmalog)
         taufunc = SNGL(sigma10/(EineV**2))

c       Fit for Segment 18
      ELSEIF ((enrgeV.GT.3.290916) .AND. (enrgeV.LT.3.841921)) THEN
         sigmalog = (fit18(1)*(enrgeV**3)) + (fit18(2)*(enrgeV**2))
     &               + (fit18(3)*enrgeV) + fit18(4)
         sigma10 = 10**(sigmalog)
         taufunc = SNGL(sigma10/(EineV**2))

c       Fit for Segment 19
      ELSEIF ((enrgeV.GT.3.841921) .AND. (enrgeV.LT.3.852916)) THEN
         sigmalog = (fit19(1)*(enrgeV**2)) + (fit19(2)*enrgeV)
     &               + fit19(3)
         sigma10 = 10**(sigmalog)
         taufunc = SNGL(sigma10/(EineV**2))

c       Fit for Segment 20
      ELSEIF ((enrgeV.GT.3.852916) .AND. (enrgeV.LT.3.880913)) THEN
         sigmalog = (fit20(1)*(enrgeV**2)) + (fit20(2)*enrgeV)
     &               + fit20(3)
         sigma10 = 10**(sigmalog)
         taufunc = SNGL(sigma10/(EineV**2))

c       Fit for Segment 21
      ELSEIF ((enrgeV.GT.3.880913) .AND. (enrgeV.LT.3.99991)) THEN
         sigmalog = (fit21(1)*(enrgeV**3)) + (fit21(2)*(enrgeV**2))
     &               + (fit21(3)*enrgeV) + fit21(4)
         sigma10 = 10**(sigmalog)
         taufunc = SNGL(sigma10/(EineV**2))

c       If enrgeV is greater that 4, the highest energy in our data set,
c       we will assume sigmalog at that enrgeV is the same as what it would
c       be at enrgeV = 4, because sigmalog is quite flat at these energies.
c       The numerator (5.454965e-17) is what sigma10 would be for
c       enrgeV = 3.99991

      ELSEIF (enrgeV.GT.3.99991) THEN
         taufunc = SNGL((5.454965E-17)/(EineV**2))
      ENDIF
      RETURN
      END
