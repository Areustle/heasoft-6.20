        SUBROUTINE MQM_RATE (TIME,R,S,SSX,NDATA,A,MA,
     +      LISTA,MFIT,COVAR,ALPHA,NCA,CHISQ,FUNCS,ALAMDA,IERR)
C
CC  Maximum likelyhood fitting routine
C
C************************ FFORM VERSION 1.0 ************  2-MAR-88 09:16
C
CA  author : J. Bloch          date:  4-SEP-1987
CU  update : J. Bloch          date:  6-SEP-1987
CU  update : SLS               date:  2-MAR-1988 09:25  
C   update: FITS calls (Srilal Weera Dec 96)
C
CT  status: not tested
C
C   general description 
CG  MQMLMN was derived from MRQMIN in the book "Numerical Recipes" by
CG  Press et al. It finds the best parameter fit using the marquardt
CG  method applied to the Poisson Maximum Likelihood Estimator, as
CG  described by Cash in the 1979 March 15 Astrophysical Journal. The
CG  SIG array has been removed because it is not needed.
C
C   call_var.          type I/O description 
CP  IERR                I4  I/O = 0 no error
CP                              = 3
CP  X                   R4  ?/? independent variable data values.
CP  Y                   R4  ?/? dependent variable data values.
CP  NDATA               I4      number of data points to fit.
CP  A                   R4  I/O model parameter values.
CP  MA                  I4  I/? number of model parameter values.
CP  LISTA               I4  I/O list of parameters
CP                                  in array A actually being varied.
CP  MFIT                I4  I/? number of parameters actually being fit.
CP  COVAR               R8  I/O covariance matrix and work
CP                                  array used durint the fit.
CP  ALPHA               R8  I/O matrix ALPHA used in the Marquard method
CP                                  (See Press et al.,pp. 521-528)
CP  NCA                 I4  I/? Actual dimensions of ALPHA and COVAR.
CP  CHISQ               R4  I/O Output Maximum Likelihood Estimator.
CP  FUNCS               R4  ?/? Input name of model function, with call
CP                                  sequence:
CP                                  CALL FUNCS(X1,A,YMOD,DYDA,DY2DA2,MA)
CP                                      With:
CP                                      X1 = Independent variable at
CP                                          which to evaluate the model.
CP                                      A(MA) = Array of model params.
CP                                      YMOD = Output model value at X1.
CP                                      DYDA(MA) = Vector of first
CP                                           derivatives of the model
CP                                           w.r.t. the parameters.
CP                                      DY2DA2(20,20)= Matrix of partial
CP                                           second derivatives of the
CP                                           model w.r.t the parameters.
CP                                      MA = Input INTEGER number of
CP                                           parameters in the model.
CP  ALAMDA              R4  I/O Updated REAL iteration control variable.
CP                              Must be set to  < 0.0 on first call
CP                              of MQMLMN. If a step fails, ALMADA grows
CP                              by a factor of 10. On final call, set
CP                              ALMADA = 0.0 so curvature matrix is 
CP                              returned.
C
C   include_block name          description 
CI  R$COMMON:CGENL.CMN          general parameter common block
C
C   routines_called    type     description 
CR  HFLAG               R4      output flag handling routine
CR  WRFLAG              SR      writes to FLAG output stream
CR  DCVSRT              SR      
CR  DGAUSJ              SR      
CR  MRQ_CB              SR      
C
C   extensions/system calls     description 
CX 
C
C***********************************************************************
C
C   variables   meaning
C
        IMPLICIT NONE
C
C  Set MMAX to the largest expected number of parameters
C
C        PARAMETER MMAX=10
C
        INTEGER*4 IERR, IERR1, IHIT, J, K, KK, MA, MFIT
        INTEGER*4 LISTA(MA), NCA, NDATA
C
        REAL*4 A(MA), ALAMDA, ATRY(10), CHISQ, FUNCS, OCHISQ, 
     +      R(NDATA), S(NDATA), SSX(NDATA), TIME(NDATA)
C
        REAL*8 ALPHA(NCA,NCA), COVAR(NCA,NCA), BETA(10), DA(10)
	character(80) context
C
        EXTERNAL FUNCS
C
	OCHISQ =0.0
        IERR = 0
C
C  Do some initializations
C
        IF(ALAMDA .LT. 0.) THEN
            KK = MFIT + 1
C
C  Check to see if LISTA contains a proper permutation of the coeffs.
C
            J = 0
            DO WHILE ((J .LT. MA) .AND. (IERR .EQ. 0))
                J = J + 1
                IHIT = 0
                DO 11 K=1,MFIT
                    IF(LISTA(K) .EQ. J) IHIT = IHIT + 1
   11           ENDDO
                IF(IHIT .EQ. 0) THEN
                    LISTA(KK) = J
                    KK = KK + 1
                ELSE IF (IHIT .GT. 1) THEN
                    IERR = 3
            context= 'MQM_RATE: Improper permutation in LISTA, 1'
	    call fcecho(context)
                ENDIF
            ENDDO
C
            IF(KK .NE. (MA+1)) THEN
                 IERR = 3
            context= 'MQM_RATE: Improper permutation in LISTA, 2'
	    call fcecho(context)
            ENDIF
C
            IF(IERR .EQ. 0) THEN
                ALAMDA = 0.001
                CALL MRQ_RATE (TIME,R,S,SSX,NDATA,A,MA,
     +              LISTA,MFIT,ALPHA,BETA,NCA,CHISQ,FUNCS,IERR1)
                IF(IERR1 .EQ. 3) THEN
                    IERR = 3
            context= 'MQM_RATE ==> Bad return from MRQ_RATE #1'
	    call fcecho(context)
                ELSE
                    OCHISQ = CHISQ
                    DO 13 J=1,MA
                        ATRY(J) = A(J)
   13               ENDDO
                ENDIF
            ENDIF
        ENDIF
C
C  Alter linearized fitting matrix by augmenting diagonal elements
C
        IF(IERR .EQ. 0) THEN
            DO 15 J=1,MFIT
                DO 14 K=1,MFIT
                    COVAR(J,K) = ALPHA(J,K)
   14           ENDDO
                COVAR(J,J) = ALPHA(J,J)*(1.0D0 + DBLE(ALAMDA))
                DA(J) = BETA(J)
   15       ENDDO
C
C  Get the matrix solution
C
            CALL DGAUSJ(COVAR,MFIT,NCA,DA,1,1,IERR1)
            IF(IERR1 .EQ. 3) THEN
                IERR = 3
            context= 'MQM_RATE ==> Bad return from DGAUSJ'
	    call fcecho(context)
            ELSE
C
C  If converged evaluate covarience matrix with ALAMDA = 0
C
                IF(ALAMDA .EQ. 0.) THEN
                    CALL DCVSRT(COVAR,NCA,MA,LISTA,MFIT)
                ELSE
C
C  Check to see if the trial succeeded
C
                    DO 16 J=1,MFIT
                        ATRY(LISTA(J)) = A(LISTA(J)) + SNGL(DA(J))
   16               ENDDO
C
                    CALL MRQ_RATE (TIME,R,S,SSX,NDATA,
     +                  ATRY,MA,LISTA,MFIT,COVAR,DA,NCA,CHISQ,
     +                  FUNCS,IERR1)
                    IF(IERR1 .EQ. 3) THEN
                        IERR = 3
            context= 'MQM_RATE ==> Bad return from MRQ_RATE #2'
	    call fcecho(context)
                    ELSE
                        IF(CHISQ .LT. OCHISQ) THEN
C
C  Success, accept the new solution
C
                            ALAMDA = 0.1*ALAMDA
                            OCHISQ = CHISQ
                            DO 18 J=1,MFIT
                                DO 17 K=1,MFIT
                                    ALPHA(J,K) = COVAR(J,K)
   17                           ENDDO
                                BETA(J) = DA(J)
                                A(LISTA(J)) = ATRY(LISTA(J))
   18                       ENDDO
                        ELSE
C
C  Failure, increase ALAMDA and return
C
                            ALAMDA = 10.*ALAMDA
                            CHISQ = OCHISQ
                        ENDIF
                    ENDIF
                ENDIF
            ENDIF
        ENDIF
C
        RETURN
        END
