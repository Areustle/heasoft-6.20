      SUBROUTINE CALFIL
C----------------------------------------------------------------------
C#    G-CALFIL   VERSION 3  MOD 0   85/07/01   H.MAYER-HASSELWANDER
C                                   85/08/30   A.W.STRONG
C			    93/09/30   B.H. Perry
C-------------------------------------------------------------------
C     THIS ROUTINE PROVIDES THE ENERGY DISPERSION CORRECTED
C     'EFFECTIVE SENSITIVE AREA' FOR THE SELECTED ENERGY RANGE AND
C     FOR THE SPECIFIED SPECTRA OF CELESTIAL AND OF INSTRUMENTAL
C     GAMMA INTENSITIES FOR ALL INCIDENCE ANGLES.
C     FIRST SUBROUTINE ENDISP IS CALLED ,WICH PROVIDES THE ENERGY-
C     DISPERSION FOR THE SELECTED ENERGY INTERVAL AND SPECTRUM.
C     THEN SUBROUTINE SENSAR CALCULATES THE EFFECTIVE SENSITIVE AREA
C     FOR ALL INCIDENCE ANGLES.
C     AN VECTOR IS PREPARED WHICH DESCRIBES THE INCLINATION DEPENDANCE
C     OF THE INSTRUMENTAL BACKGROUND COUNTRATE.
C     MODIF. MARCH 85: SELECT APPRORIATE SENSITIVE AREA FILE DEPENDING
C                      ON PAIR OPENING ANGLE SELECTION; IF AN INTERMED-
C                      IATE VALUE IS SPECIFIED, THE NEXT LARGER
C                      SELECTABLE LIMIT IS USED.
C----------------------------------------------------------------------
C
      INCLUDE 'COMMON.INC'
      REAL phi
      INTEGER i, status, enprob_ptr, effarea_ptr, nelem
      logical enprobok, effareaok

C init
      effareaok=.false.
C
C get the correct ebounds matrix. BHP
C
      CALL READ_EBOUNDS
C
C interpolate the array escal by sending ebounds_ehi and
C ebounds_elo to the subroutine get_escal. escal is later
C passed to sensar for calculation
C
      CALL GET_ESCAL
C
C call endisp to integrate the energy dispersion (REDIST) over
C a measured energy interval. This is done for each of the seven
C inclination angles, then interpolated for all inclinations to
C return the probabilities (ENPROB(n_chan,n_theta)).
C
C allocate needed dynamic arrays
C allocate enprob array
      enprobok = .false.
      nelem = n_chan*n_theta
      enprob_ptr = 0
      call udmget (nelem, 6, enprob_ptr, status)
      if (status .ne. 0) then 
         context = ' Error allocating enprob memory'
         call fcerr (context)
         goto 1000
      else
         enprobok = .true.
         call initrealarray(memr(enprob_ptr), nelem)
      endif
 
C allocate effarea array
      effareaok = .false.
      nelem = n_chan*n_theta
      effarea_ptr = 0
      call udmget (nelem, 6, effarea_ptr, status)
      if (status .ne. 0) then 
         context = ' Error allocating effarea memory'
         call fcerr (context)
         goto 1000
      else
         effareaok = .true.
         call initrealarray(memr(effarea_ptr), nelem)
      endif
 
      CALL ENDISP (memr(enprob_ptr))

C
C CALL READ_EFFAREA TO GET APPROPRIATE SENSITIVE AREA FILE. THIS
C ROUTINE RETURNS THE CORRECT SA ARRAY BASED ON PAIRA
C
      CALL READ_EFFAREA (memr(effarea_ptr))
C
C call sensar to calculate the effective sensitive area function
C as a function of inclination, using sensarea, energy disp, and
C spectral index.
C
      CALL SENSAR(1, memr(enprob_ptr), memr(effarea_ptr))
      CALL SENSAR(2, memr(enprob_ptr), memr(effarea_ptr))
C
C DERIVE VECTOR INDICATING THE INCLINATION DEPENDENCE OF THE
C BACKGROUND COUNT RATE FOR THE ENERGY RANGE
C
C n_theta is the number of inclination angle bins, 60 for COS-B
C
      DO 100 i = 1 , N_theta
         phi = Inclsiz*(i-1)
         IF ( Bglint.LT.-1.E-06 ) THEN
C
C INSTRUMENTAL BACKGROUND INTENSITY IS INCLINATION DEPENDANT
C
            Bgincl(i) = Effsa(1,2)*(1.+Bglint*phi+Bgquat*phi**2)
         ELSE
C
C INSTRUMENTAL BACKGROUND INTENSITY IS 'FLAT'
C
            Bgincl(i) = Effsa(i,2)
         ENDIF
 100  CONTINUE
      WRITE (context,99001) N_theta , (Bgincl(i),i=1,N_theta,10)
      call fcecho (context)
      call fcecho (' ')

 1000 if (enprobok) call udmfre (enprob_ptr, 6, status)
      if (effareaok) call udmfre (effarea_ptr, 6, status)
      RETURN
99001 FORMAT ('0BACKGROUND, 0 TO ',i5,' DEG: ',10E12.3)
      END
C
C
C
C
      SUBROUTINE ENDISP (enprob)
C----------------------------------------------------------------------
C#    G-ENDISP   VERSION 3  MOD 0   85/07/01   H.MAYER-HASSELWANDER
C----------------------------------------------------------------------
C     THIS ROUTINE INTEGRATES THE ENERGY DISPERSION OVER A MEASURED
C     ENERGY INTERVAL. THIS IS DONE FIRST FOR THE 7 AVAILABLE
C     INCLINATIONS, THEN INTERPOLATION IS PERFORMED TO GIVE THE
C     PROBABILITY FOR ALL INCLINATIONS BETWEN 0 AND 30 DEGREES IN
C     1/2 DEGREE INTERVALS: ENPROB(n_chan,n_theta).
C----------------------------------------------------------------------
C
C modified by bhp, summer 1993
C
      INCLUDE 'COMMON.INC'
      REAL edpv , elogv , FINTER , ric1 , ric2 , rincl
      INTEGER ic1 , ic2 , incl , lv , mentru , row, nincl
      DIMENSION elogv(1000) , edpv(1000)   ! n_chan = 115 for cos-b
      real enprob (n_chan, n_theta)
C
C     DERIVE PROBABILITY, THAT A PHOTON IS OBSERVED IN ENERGY INTERVAL
C
C     FOR ALL TRUE ENERGIES AND INCIDENCE ANGLES
C
C     LOOP FOR INCLINATION ANGLES:
C
C n_incl = 7 for cos-b. There are 7 redistribution matrix files
C n_incl = 1 for sas-2. There are 1 redistribution matrix files
C nincl = nint(delta-degrees of redist files/delta-degrees of redist bins       
C
      nincl = nint(d_theta/inclsiz)
      DO 100 incl = 1 , N_incl
         DO 50 row = 1 , N_chan
C
C  EDP IS NORMALISED ALONG THE 'MEASURED ENERGY' AXIS SUCH THAT THE SUM
C  OVER THE LOGARITHMIC BINS FROM 20 MEV TO 100 GEV IS EQUAL TO 1.
C
            CALL READ_REDIST(incl,row)

            Enlgl = ALOG10(Enerl)
            Enlgh = ALOG10(Enerh)
 
            CALL TABSEL(Enlgl,Enlgh,Elog,Redist_array,N_chan,elogv,edpv,
     &                  lv)
C
C PERFORM INTEGRATION
C
            CALL QTFG(elogv,edpv,edpv,lv)
            Enprob(row,nincl*(incl-1)+1) = edpv(lv)/0.02033
 50      CONTINUE
 100  CONTINUE
C
C  THE FACTOR .02033 (LOGAR. ENERGY BINWIDTH) COMPENSATES FOR THE SAME
C  FACTOR IMPLICITLY APPLIED IN THE ROUTINE QTFG
C
      DO 200 incl = 1 , N_theta
         rincl = incl
         ic1 = (incl/(nincl+1))*nincl + 1
         ic2 = ic1 + nincl
         ric1 = ic1
         ric2 = ic2
         DO 120 mentru = 1 , N_chan
            Enprob(mentru,incl) = FINTER(rincl,ric1,ric2,
     &		   Enprob(mentru,ic1),Enprob(mentru,ic2))
 120        CONTINUE
 200  CONTINUE
      call fcecho 
     &('*** Energy Dispersion calibration data entered into FADMAP ***')
      RETURN
      END
C
C
C
C
      SUBROUTINE EXPBKG(mapp, sarmat, bkgmat)
C---------------------------------------------------------------------
C#    G-EXPBKG     VERSION 3   MOD 0   85/07/30  H.MAYER-HASSELWANDER
C                                      85/08/30  A.W.STRONG
C                                      85/10/02  H.MAYER-HASSELWANDER
C---------------------------------------------------------------------
C     THIS ROUTINE UPDATES THE EXPOSURE AND BACKGROUND WORK ARRAYS
C     ACCORDING TO THE CONTRIBUTION OF ONE OUR DATASET. THE EXPOSURE IS
C     CALCULATED AND STORED FOR CELESTIAL AND BACKGROUND FLUX SEPARATE.
C     THE UNITS OF EXPOSURE AT THIS LEVEL ARE CM**2 * S * SR (OF BIN)
C     THE BACKGROUND WORK ARRAY CONTAINS THE PREDICTED BKG-COUNTS/BIN
C---------------------------------------------------------------------
C
      INCLUDE 'COMMON.INC'
      REAL bkgcnt
      INTEGER la , lo
      real mapp (mappx, mappy, 4), sarmat (mappx, mappy, 2)
      real bkgmat(mappx, mappy)
C
      DO 100 lo = 1 , INT(Nlo/Dlo)
         DO 50 la = 1 , INT(Nla/Dla)
 
C
C     CHECK FOR LIMIT OF FIELD OF VIEW
C
            IF ( Sarmat(la,lo,1).LT.-0.1 ) goto 50
C
C  INCLINATION DEPENDANT OR FLAT BACKGROUND
C
            IF ( Bglint.LT.-1.E-06 ) THEN
C
C  BACKGROUND INTENSITY IS INCLINATION DEPENDANT
C
               bkgcnt = ((Bkg0+Bkg1*Dels3)*Bkgmat(la,lo)
     &                  -Bkg0*Sarmat(la,lo,2))*Ruseft*Sr(la)
            ELSE
C
C  BACKGROUND INTENSITY IS FLAT
C
               bkgcnt = Bkg1*Dels3*Sarmat(la,lo,2)*Ruseft*Sr(la)
 
            ENDIF
C
C 1 = counts map
C 2 = background map
C 3 = source exposure map
C 4 = background exposure map
C
            Mapp(la,lo,2) = Mapp(la,lo,2) + bkgcnt
            Mapp(la,lo,3) = Mapp(la,lo,3) + Sarmat(la,lo,1)
     &                      *Ruseft*Sr(la)
            Mapp(la,lo,4) = Mapp(la,lo,4) + Sarmat(la,lo,2)
     &                      *Ruseft*Sr(la)
 
 50      CONTINUE
 100  CONTINUE
 
      RETURN
      END
C
C
C
C
      FUNCTION FINTER(X,X1,X2,Y1,Y2)
C--------------------------------------------------------------------
C#    G-FINTER     VERSION 3   MOD 0   85/07/01   H.MAYER-HASSELWANDER
C-------------------------------------------------------------------
C     INTERPOLATION AT X BETWEEN TWO PAIRS OF ARG/FUNCT VAL (LINEAR)
C--------------------------------------------------------------------
      REAL FINTER , X , X1 , X2 , Y1 , Y2
      FINTER = (Y1*(X-X2)-Y2*(X-X1))/(X1-X2)
      RETURN
      END
C
C
C
C
C
C
C
C
      FUNCTION GTCIRC(Al1,B1,Al2,B2)
C----------------------------------------------------------------------
C#    GTCIRC       VERSION3   MOD 0     85/09/05   A.W.STRONG
C----------------------------------------------------------------------
C     GIVES  SHORTEST GREAT CIRCLE DISTANCE BETWEEN TWO POINTS
C      (AL1,B1) AND (AL2,B2) ON THE CELESTIAL SPHERE
C     USING THE SPHERICAL TRIG. COSINE RULE
C     UNITS: DEGREES
C----------------------------------------------------------------------
      REAL Al1 , Al2 , B1 , b1r , B2 , b2r , cb1 , cb2 , cdal , dtr , 
     &     GTCIRC , sb1 , sb2
C
C
      dtr = 3.14159/180.
      b1r = B1*dtr
      b2r = B2*dtr
      sb1 = SIN(b1r)
      sb2 = SIN(b2r)
      cb1 = COS(b1r)
      cb2 = COS(b2r)
      cdal = COS((Al1-Al2)*dtr)
      GTCIRC = ACOS(sb1*sb2+cb1*cb2*cdal)
      GTCIRC = GTCIRC/dtr
      RETURN
      END
C
C
C
C
      SUBROUTINE QTFG(X,Y,Z,Ndim)
C--------------------------------------------------------------------
C#    G-QTFG     VERSION 3   MOD 0   85/07/01   H.MAYER-HASSELWANDER
C-------------------------------------------------------------------
C     INTEGRATION ROUTINE FROM IBM SCIENTIFIC SUBROUTINE PACKAGE
C--------------------------------------------------------------------
C
C
      INTEGER i , Ndim
      REAL sum1 , sum2 , X , Y
      REAL Z
      DIMENSION X(*) , Y(*) , Z(*)
C
      sum2 = 0.
      IF ( Ndim.GE.1 ) THEN
         IF ( Ndim.NE.1 ) THEN
C
C     INTEGRATION LOOP
C
            DO 20 i = 2 , Ndim
               sum1 = sum2
               sum2 = sum2 + .5*(X(i)-X(i-1))*(Y(i)+Y(i-1))
               Z(i-1) = sum1
 20         CONTINUE
         ENDIF
         Z(Ndim) = sum2
      ENDIF
      RETURN
      END
C
C
C
C
      SUBROUTINE SENMAT (sarmat, bkgmat)
 
      INCLUDE 'COMMON.INC'
      REAL al1 , al2 , b , latcor , loncor
      REAL GTCIRC , pi1 , pi2
      INTEGER i , iphi1 , iphi2 , k , l
      real sarmat(mappx, mappy, 2), bkgmat(mappx, mappy)
 
      pi1 = 3.141593/180.
      pi2 = (pi1*Dla)**2
 
      loncor = Clo - Nlo/2.
      latcor = Cla - Nla/2.
 
      DO 100 i = 1 , INT(Nlo/Dlo)
         b = pi1*(latcor+(i-0.5)*Dla)
         Sr(i) = pi2*COS(b)
 100  CONTINUE
 
      DO 200 k = 1 , INT(Nlo/Dlo)/2
         DO 150 i = 1 , INT(Nla/Dla)
            b = latcor + (i-0.5)*Dla
            al1 = loncor + (k-0.5)*Dlo
            al2 = loncor + Nlo - (k-0.5)*Dlo
            iphi1 = 1 + GTCIRC(al1,b,Xpcoord1(X_row),Xpcoord2(X_row))
     &              /Inclsiz
            iphi2 = 1 + GTCIRC(al2,b,Xpcoord1(X_row),Xpcoord2(X_row))
     &              /Inclsiz
            IF ( iphi1.GT.N_theta ) iphi1 = N_theta
            IF ( iphi2.GT.N_theta ) iphi2 = N_theta
            Bkgmat(i,k) = Bgincl(iphi1)
            Bkgmat(i,INT(Nlo/Dlo)+1-k) = Bgincl(iphi2)
            DO 120 l = 1 , 2
               Sarmat(i,k,l) = Effsa(iphi1,l)
               Sarmat(i,INT(Nlo/Dlo)+1-k,l) = Effsa(iphi2,l)
 120        CONTINUE
 150     CONTINUE
 200  CONTINUE
      RETURN
      END
C
C
C
C
      SUBROUTINE SENSAR(Ind, enprob, effarea_array)
C----------------------------------------------------------------------
C#    G-SENSAR   VERSION 3   MOD 0   85/07/01     MAYER HASSELWANDER
C----------------------------------------------------------------------
C     THIS ROUTINE CALCULATES THE EFFECTIVE SENSITIVE AREA FUNCTION AS
C     A FUNCTION OF INCLINATION, USING SENSITIVE AREA, ENERGY DISPERSION
C     AND SPECTRAL INDEX. ARGUMENT IND DETERMINES INTO WHICH PART OF THE
C     OUTPUT ARRAY THE RESULT IS WRITTEN.
C     MODIF. MARCH 85: SELECT APPRORIATE SENSITIVE AREA FILE DEPENDING
C                      ON PAIR OPENING ANGLE SELECTION; IF AN INTERMED-
C                      IATE VALUE IS SPECIFIED, THE NEXT LARGER
C                      SELECTABLE LIMIT IS USED.
C***** CALIBRATION DATA ARE AVAILABLE IN THE ENERGY RANGE 37-7800 ******
C----------------------------------------------------------------------
C
      REAL escalv , sa2 , spec , specv , speind
      INTEGER i , i1 , incl , inclmx , Ind , k , nv
      INCLUDE 'COMMON.INC'
 
      DIMENSION spec(1000) , specv(1000) , escalv(1000) , sa2(1000)

      real enprob (n_chan, n_theta), effarea_array (n_chan, n_theta)
C
C DIM should be 115 for COS-B
C
C DEFINE THE INDEX OF THE RELEVANT SENSITIVE AREA DATASET
C DEPENDING ON SELECTED OPENING ANGLE LIMIT
C
C DEFINE ACCEPTED FIELD OF VIEW (SCINA, MAX INCLIN TO POINTING)
C
      inclmx = INT(Scina/Inclsiz) + 1
      IF ( inclmx.GT.N_theta ) inclmx = N_theta
C
C CALCULATE ENERGY SCALE AND SPECTRUM
C
C ESCAL RETURNS THE INTERPOLATED ESCAL ARRAY, USED TO CALCULATE SPEC
C
      IF ( Ind.EQ.1 ) THEN
         speind = Sg
      ELSE
         speind = Sb
      ENDIF
 
      DO 100 k = 1 , N_chan
         spec(k) = Escal(k)**speind
 100  CONTINUE
C
C INTEGRATE SPECTRUM OVER ENERGY INTERVAL (DENOMINATOR)
C
      CALL TABSEL(Enerl,Enerh,Escal,spec,N_chan,escalv,specv,nv)
      CALL QTFG(escalv,specv,specv,nv)
C
C INTEGRATE PRODUKT OF SA,ENPROB,SPEC OVER ALL ENERGIES
C
      DO 200 incl = 1 , inclmx
 
         DO 150 i = 1 , N_chan
            sa2(i) = Effarea_array(i,incl)*Enprob(i,incl)*spec(i)
 150     CONTINUE
 
         CALL QTFG(Escal,sa2,sa2,N_chan)
         Effsa(incl,Ind) = sa2(N_chan)/specv(nv)
 
 200  CONTINUE
 
C
C BEYOND FIELD OF VIEW, INITIALIZE EFFSA ELEMENTS TO -1
C
c      i1 = inclmx + 1
      i1 = inclmx 
      DO 300 incl = i1 , N_theta
         Effsa(incl,Ind) = -1.
 300  CONTINUE
 
      IF ( Ind.EQ.1 ) THEN
         Call fcecho (' ')
         Call fcecho
     &        ('*** Effective Area calibration array (EFFSA) for ')
         Call fcecho
     &        ('    ''celestial intensity'' entered into FADMAP ***')
      ELSE
         call fcecho (' ')
         call fcecho
     &        ('*** Effective Area calibration array (EFFSA) for ')
         call fcecho 
     &('   ''instrumental background intensity'' entered in FADMAP ***')
      ENDIF
 
      RETURN
      END
C
C
C
C
      SUBROUTINE TABSEL(Xl,Xh,X,Y,N,X1,Y1,Lv)
C----------------------------------------------------------------------
C#    G-TABSEL    VERSION 3   MOD 0   85/07/01    H.MAYER-HASSELWANDER
C----------------------------------------------------------------------
C     THIS ROUTINE DRAWS A MONOTONIC TABLE OUT OF AN EXISTING MONOTONIC
C     TABLE, BUT WITH NEW LEFT XL AND RIGHT XH BORDERS. IT USES FUNCTION
C     FINTER, DEFINED WITHIN THIS PROGRAM PACKAGE.
C----------------------------------------------------------------------
C
C modified by bhp, summer 1993
C
      REAL FINTER , X , X1 , Xh , Xl , Y , Y1
      INTEGER l , l1 , l2 , ll , Lv , N
      DIMENSION X(1000) , Y(1000) , X1(1000) , Y1(1000)
      l1=0
      l2=0
      IF ( Xl.LT.Xh ) THEN
         IF ( Xl.GE.X(1) .AND. Xh.LE.X(N) ) THEN
            DO 20 l = 1 , N
               l1 = l
               IF ( X(l).GT.Xl ) GOTO 40
 20         CONTINUE
 40         X1(1) = Xl
            DO 60 l = l1 , N
               l2 = l
               IF ( X(l).GE.Xh ) GOTO 80
 60         CONTINUE
 80         Lv = l2 - l1 + 2
            X1(Lv) = Xh
            Y1(1) = FINTER(X1(1),X(l1-1),X(l1),Y(l1-1),Y(l1))
            Y1(Lv) = FINTER(X1(Lv),X(l2-1),X(l2),Y(l2-1),Y(l2))
            ll = 1
            DO 100 l = l1 , l2 - 1
               ll = ll + 1
               X1(ll) = X(l)
               Y1(ll) = Y(l)
 100        CONTINUE
            RETURN
         ENDIF
      ENDIF
      call fcerr ( '***    ERROR IN TABSEL, WRONG RANGE    ***')
      call fcerr ('*** FADMAP EXITED IN SUBROUTINE TABSEL ***')
      STOP
      END
