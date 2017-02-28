         SUBROUTINE MRQ_RATE (TIME,R,S,SSX,NDATA,A,MA,
     +       LISTA,MFIT,ALPHA,BETA,NALP,CHISQ,FUNCS,IERR)
C
CC  Computes matrix elements for MQM_CB
C
C************************ FFORM VERSION 1.0 ************ DD-MMM-YY HH:MM
C
CA  author : J. Bloch          date:  4-SEP-1987
CU  update : J. Bloch          date:  6-SEP-1987
CU  update : SLS               date:  2-MAR-1988 09:23  
C
CT  status: not tested
C
C   general description:
CG  MRQMLC was derived from MRQCOF in the book "Numerical Recipes"
CG  by Press et al. It computes the matrix elements needed for
CG  the marquardt method applied to the Poisson Maximum Likelihood
CG  Estimator, as described by Cash in the 1979 March 15
CG  Astrophysical Journal. The SIG array has been removed because
CG  it is not needed.
C
C   call_var.          type I/O description 
CP  IERR                I4    O = 0 no error
CP                              = 3 illegal model value (model < 0.)
CP  X                   R4  ?/? array of independent var. data values.
CP  Y                   R4  ?/? array of dependent variable data values.
CP  NDATA               I4      number of data points to fit.
CP  A                   R4  I/O array of model parameter values.
CP  MA                  I4  I/? number of model parameter values.
CP  LISTA               I4  I/O array list of parameters
CP                                  in array A actually being varied.
CP  MFIT                I4  I/? number of parameters actually being fit.
CP  COVAR               R8  I/O covariance matrix and work
CP                                  array used durint the fit.
CP  ALPHA               R8  I/O matrix ALPHA used in the Marquard 
CP                              method.  (See Press et al., pp. 521-528)
CP  BETA                        vector beta used in the Marquardt 
CP                              method.  (See Press et al. pp. 521-528)
CP  NALP                        actual dimensions of ALPHA and BETA.
CP  CHISQ               R4  I/O Output Maximum Likelihood Estimator.
CP  FUNCS               SR      input name of model function
C
C   include_block_name          description
CI  R$COMMON:CGENL.CMN          general parameter common block
C
C   routines_called    type     description
CR  HFLAG               R       output flag handling routine
CR  WRFLAG              SR      writes to FLAG output stream
CR  FUNCS               SR      input name of model function, with call
CR                                  sequence:
CR                              CALL FUNCS(X1,A,YMOD,DYDA,DY2DA2,LISTA,
CR                                          MFIT,MA)
CR                                  With:
CR                                    X1 = Independent variable at
CR                                        which to evaluate the model.
CR                                    A(MA) = Array of model parameters.
CR                                    YMOD = Output model value at X1.
CR                                    DYDA(MA) = Vector of first
CR                                         derivatives of the model
CR                                         w.r.t. the parameters.
CR                                    DY2DA2(20,20) = Matrix of partial
CR                                         second derivatives of the
CR                                         model w.r.t the parameters.
CR                                    LISTA(MFIT) = Same as above.
CR                                    MFIT = Same as above.
CR                                    MA = Input INTEGER number of
CR                                         parameters in the model.
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
C       PARAMETER MMAX=10
C
        INTEGER*4 I, IERR, J, K, KK, MA, MFIT, NALP, NDATA
        INTEGER*4 LISTA(MFIT)
C
        REAL*4 A(MA), CHISQ, DY, DYDA(10), D2YDA2(10,10), 
     +          R(NDATA), S(NDATA), SIG2I, SSX(NDATA), TIME(NDATA), 
     +          WT, YMOD
C
        REAL*8 ALPHA(NALP,NALP), BETA(MA)
	character(80) context
C
        EXTERNAL FUNCS
C
        IERR = 0
C
C  Initialize (symmetric) ALPHA and BETA
C
        DO 12 J=1,MFIT
            DO 11 K=1,J
                ALPHA(J,K) = 0.
   11       ENDDO
            BETA(J) = 0.
   12   ENDDO
C
        CHISQ = 0.0
C
C  Summation loop over all data
C
        I = 0
        DO WHILE ((I .LT. NDATA) .AND. (IERR .EQ. 0))
            I = I + 1
            CALL FUNCS(TIME(I),SSX(I),A,YMOD,DYDA,
     +          D2YDA2,LISTA,MFIT,MA)
C
            IF(YMOD .LT. 0.0D0) THEN
		write(context,
     &	 '('' MRQ_RATE: Illegal model value = '',f10.5)')YMOD
		call fcecho(context)
		write(context,
     &	 '(''                       TIME(I) = '',f10.5)')TIME(I)
		call fcecho(context)
		write(context,
     &	 '(''                          R(I) = '',f10.5)')R(I)
		call fcecho(context)
		write(context,
     &	 '(''                          S(I) = '',f10.5)')S(I)
		call fcecho(context)
		write(context,
     &	 '(''                        SSX(I) = '',f10.5)')SSX(I)
		call fcecho(context)
C
           DO KK=1,MA
 		write(context,
     &	 '(''                             A = '',f10.5)') A(KK)
		call fcecho(context)
           ENDDO
		IERR = 3
            ELSE
                SIG2I = 1./(S(I)*S(I))
                DY = R(I) - YMOD
C
                DO 14 J=1,MFIT
                    WT = SIG2I*DYDA(LISTA(J))
                    DO 13 K=1,J
                        ALPHA(J,K) = ALPHA(J,K) + WT*DYDA(LISTA(K))
   13               ENDDO
                    BETA(J) = BETA(J) + DY*WT
   14           ENDDO
                CHISQ = CHISQ + DY*DY*SIG2I
            ENDIF
        ENDDO
C
        IF(IERR .EQ. 0) THEN
C
C  Fill in the symmetric side
C
            DO 17 J=2,MFIT
                DO 16 K=1,J-1
                    ALPHA(K,J) = ALPHA(J,K)
   16           ENDDO
   17       ENDDO
        ENDIF
C
        RETURN
        END
