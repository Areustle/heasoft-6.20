      SUBROUTINE DGECO(A,LDA,N,IPVT,RCOND,Z)                            
C***BEGIN PROLOGUE  DGECO                                               
C***DATE WRITTEN   780814   (YYMMDD)                                    
C***REVISION DATE  861211   (YYMMDD)                                    
C***CATEGORY NO.  D2A1                                                  
C***KEYWORDS  LIBRARY=SLATEC(LINPACK),                                  
C             TYPE=DOUBLE PRECISION(SGECO-S DGECO-D CGECO-C),           
C             CONDITION NUMBER,GENERAL MATRIX,LINEAR ALGEBRA,MATRIX,    
C             MATRIX FACTORIZATION                                      
C***AUTHOR  MOLER, C. B., (U. OF NEW MEXICO)                            
C***PURPOSE  Factors a double precision matrix by Gaussian elimination  
C            and estimates the condition of the matrix.                 
C***DESCRIPTION                                                         
C                                                                       
C     DGECO factors a double precision matrix by Gaussian elimination   
C     and estimates the condition of the matrix.                        
C                                                                       
C     If  RCOND  is not needed, DGEFA is slightly faster.               
C     To solve  A*X = B , follow DGECO by DGESL.                        
C     To compute  INVERSE(A)*C , follow DGECO by DGESL.                 
C     To compute  DETERMINANT(A) , follow DGECO by DGEDI.               
C     To compute  INVERSE(A) , follow DGECO by DGEDI.                   
C                                                                       
C     On Entry                                                          
C                                                                       
C        A       DOUBLE PRECISION(LDA, N)                               
C                the matrix to be factored.                             
C                                                                       
C        LDA     INTEGER                                                
C                the leading dimension of the array  A .                
C                                                                       
C        N       INTEGER                                                
C                the order of the matrix  A .                           
C                                                                       
C     On Return                                                         
C                                                                       
C        A       an upper triangular matrix and the multipliers         
C                which were used to obtain it.                          
C                The factorization can be written  A = L*U  where       
C                L  is a product of permutation and unit lower          
C                triangular matrices and  U  is upper triangular.       
C                                                                       
C        IPVT    INTEGER(N)                                             
C                an INTEGER vector of pivot indices.                    
C                                                                       
C        RCOND   DOUBLE PRECISION                                       
C                an estimate of the reciprocal condition of  A .        
C                For the system  A*X = B , relative perturbations       
C                in  A  and  B  of size  EPSILON  may cause             
C                relative perturbations in  X  of size  EPSILON/RCOND . 
C                If  RCOND  is so small that the logical expression     
C                           1.0 + RCOND .EQ. 1.0                        
C                is true, then  A  may be singular to working           
C                precision.  In particular,  RCOND  is zero  if         
C                exact singularity is detected or the estimate          
C                underflows.                                            
C                                                                       
C        Z       DOUBLE PRECISION(N)                                    
C                a work vector whose contents are usually unimportant.  
C                If  A  is close to a singular matrix, then  Z  is      
C                an approximate null vector in the sense that           
C                NORM(A*Z) = RCOND*NORM(A)*NORM(Z) .                    
C                                                                       
C     LINPACK.  This version dated 08/14/78 .                           
C     Cleve Moler, University of New Mexico, Argonne National Lab.      
C                                                                       
C     Subroutines and Functions                                         
C                                                                       
C     LINPACK DGEFA                                                     
C     BLAS DAXPY,DDOT,DSCAL,DASUM                                       
C     Fortran DABS,DMAX1,DSIGN                                          
C***REFERENCES  DONGARRA J.J., BUNCH J.R., MOLER C.B., STEWART G.W.,    
C                 *LINPACK USERS  GUIDE*, SIAM, 1979.                   
C***ROUTINES CALLED  DASUM,DAXPY,DDOT,DGEFA,DSCAL                       
C***END PROLOGUE  DGECO                                                 
      INTEGER LDA,N,IPVT(1)                                             
      DOUBLE PRECISION A(LDA,1),Z(1)                                    
      DOUBLE PRECISION RCOND                                            
C                                                                       
      DOUBLE PRECISION DDOT,EK,T,WK,WKM                                 
      DOUBLE PRECISION ANORM,S,DASUM,SM,YNORM                           
      INTEGER INFO,J,K,KB,KP1,L                                         
C                                                                       
C     COMPUTE 1-NORM OF A                                               
C                                                                       
C***FIRST EXECUTABLE STATEMENT  DGECO                                   
      ANORM = 0.0D0                                                     
      DO 10 J = 1, N                                                    
         ANORM = DMAX1(ANORM,DASUM(N,A(1,J),1))                         
   10 CONTINUE                                                          
C                                                                       
C     FACTOR                                                            
C                                                                       
      CALL DGEFA(A,LDA,N,IPVT,INFO)                                     
C                                                                       
C     RCOND = 1/(NORM(A)*(ESTIMATE OF NORM(INVERSE(A)))) .              
C     ESTIMATE = NORM(Z)/NORM(Y) WHERE  A*Z = Y  AND  TRANS(A)*Y = E .  
C     TRANS(A)  IS THE TRANSPOSE OF A .  THE COMPONENTS OF  E  ARE      
C     CHOSEN TO CAUSE MAXIMUM LOCAL GROWTH IN THE ELEMENTS OF W  WHERE  
C     TRANS(U)*W = E .  THE VECTORS ARE FREQUENTLY RESCALED TO AVOID    
C     OVERFLOW.                                                         
C                                                                       
C     SOLVE TRANS(U)*W = E                                              
C                                                                       
      EK = 1.0D0                                                        
      DO 20 J = 1, N                                                    
         Z(J) = 0.0D0                                                   
   20 CONTINUE                                                          
      DO 100 K = 1, N                                                   
         IF (Z(K) .NE. 0.0D0) EK = DSIGN(EK,-Z(K))                      
         IF (DABS(EK-Z(K)) .LE. DABS(A(K,K))) GO TO 30                  
            S = DABS(A(K,K))/DABS(EK-Z(K))                              
            CALL DSCAL(N,S,Z,1)                                         
            EK = S*EK                                                   
   30    CONTINUE                                                       
         WK = EK - Z(K)                                                 
         WKM = -EK - Z(K)                                               
         S = DABS(WK)                                                   
         SM = DABS(WKM)                                                 
         IF (A(K,K) .EQ. 0.0D0) GO TO 40                                
            WK = WK/A(K,K)                                              
            WKM = WKM/A(K,K)                                            
         GO TO 50                                                       
   40    CONTINUE                                                       
            WK = 1.0D0                                                  
            WKM = 1.0D0                                                 
   50    CONTINUE                                                       
         KP1 = K + 1                                                    
         IF (KP1 .GT. N) GO TO 90                                       
            DO 60 J = KP1, N                                            
               SM = SM + DABS(Z(J)+WKM*A(K,J))                          
               Z(J) = Z(J) + WK*A(K,J)                                  
               S = S + DABS(Z(J))                                       
   60       CONTINUE                                                    
            IF (S .GE. SM) GO TO 80                                     
               T = WKM - WK                                             
               WK = WKM                                                 
               DO 70 J = KP1, N                                         
                  Z(J) = Z(J) + T*A(K,J)                                
   70          CONTINUE                                                 
   80       CONTINUE                                                    
   90    CONTINUE                                                       
         Z(K) = WK                                                      
  100 CONTINUE                                                          
      S = 1.0D0/DASUM(N,Z,1)                                            
      CALL DSCAL(N,S,Z,1)                                               
C                                                                       
C     SOLVE TRANS(L)*Y = W                                              
C                                                                       
      DO 120 KB = 1, N                                                  
         K = N + 1 - KB                                                 
         IF (K .LT. N) Z(K) = Z(K) + DDOT(N-K,A(K+1,K),1,Z(K+1),1)      
         IF (DABS(Z(K)) .LE. 1.0D0) GO TO 110                           
            S = 1.0D0/DABS(Z(K))                                        
            CALL DSCAL(N,S,Z,1)                                         
  110    CONTINUE                                                       
         L = IPVT(K)                                                    
         T = Z(L)                                                       
         Z(L) = Z(K)                                                    
         Z(K) = T                                                       
  120 CONTINUE                                                          
      S = 1.0D0/DASUM(N,Z,1)                                            
      CALL DSCAL(N,S,Z,1)                                               
C                                                                       
      YNORM = 1.0D0                                                     
C                                                                       
C     SOLVE L*V = Y                                                     
C                                                                       
      DO 140 K = 1, N                                                   
         L = IPVT(K)                                                    
         T = Z(L)                                                       
         Z(L) = Z(K)                                                    
         Z(K) = T                                                       
         IF (K .LT. N) CALL DAXPY(N-K,T,A(K+1,K),1,Z(K+1),1)            
         IF (DABS(Z(K)) .LE. 1.0D0) GO TO 130                           
            S = 1.0D0/DABS(Z(K))                                        
            CALL DSCAL(N,S,Z,1)                                         
            YNORM = S*YNORM                                             
  130    CONTINUE                                                       
  140 CONTINUE                                                          
      S = 1.0D0/DASUM(N,Z,1)                                            
      CALL DSCAL(N,S,Z,1)                                               
      YNORM = S*YNORM                                                   
C                                                                       
C     SOLVE  U*Z = V                                                    
C                                                                       
      DO 160 KB = 1, N                                                  
         K = N + 1 - KB                                                 
         IF (DABS(Z(K)) .LE. DABS(A(K,K))) GO TO 150                    
            S = DABS(A(K,K))/DABS(Z(K))                                 
            CALL DSCAL(N,S,Z,1)                                         
            YNORM = S*YNORM                                             
  150    CONTINUE                                                       
         IF (A(K,K) .NE. 0.0D0) Z(K) = Z(K)/A(K,K)                      
         IF (A(K,K) .EQ. 0.0D0) Z(K) = 1.0D0                            
         T = -Z(K)                                                      
         CALL DAXPY(K-1,T,A(1,K),1,Z(1),1)                              
  160 CONTINUE                                                          
C     MAKE ZNORM = 1.0                                                  
      S = 1.0D0/DASUM(N,Z,1)                                            
      CALL DSCAL(N,S,Z,1)                                               
      YNORM = S*YNORM                                                   
C                                                                       
      IF (ANORM .NE. 0.0D0) RCOND = YNORM/ANORM                         
      IF (ANORM .EQ. 0.0D0) RCOND = 0.0D0                               
      RETURN                                                            
      END                                                               
      SUBROUTINE DGEDI(A,LDA,N,IPVT,DET,WORK,JOB)                       
C***BEGIN PROLOGUE  DGEDI                                               
C***DATE WRITTEN   780814   (YYMMDD)                                    
C***REVISION DATE  861211   (YYMMDD)                                    
C***CATEGORY NO.  D3A1,D2A1                                             
C***KEYWORDS  LIBRARY=SLATEC(LINPACK),                                  
C             TYPE=DOUBLE PRECISION(SGEDI-S DGEDI-D CGEDI-C),           
C             DETERMINANT,INVERSE,LINEAR ALGEBRA,MATRIX,                
C             MATRIX FACTORIZATION                                      
C***AUTHOR  MOLER, C. B., (U. OF NEW MEXICO)                            
C***PURPOSE  Computes the determinant and inverse of a matrix using     
C            factors computed by DGECO or DGEFA.                        
C***DESCRIPTION                                                         
C                                                                       
C     DGEDI computes the determinant and inverse of a matrix            
C     using the factors computed by DGECO or DGEFA.                     
C                                                                       
C     On Entry                                                          
C                                                                       
C        A       DOUBLE PRECISION(LDA, N)                               
C                the output from DGECO or DGEFA.                        
C                                                                       
C        LDA     INTEGER                                                
C                the leading dimension of the array  A .                
C                                                                       
C        N       INTEGER                                                
C                the order of the matrix  A .                           
C                                                                       
C        IPVT    INTEGER(N)                                             
C                the pivot vector from DGECO or DGEFA.                  
C                                                                       
C        WORK    DOUBLE PRECISION(N)                                    
C                work vector.  Contents destroyed.                      
C                                                                       
C        JOB     INTEGER                                                
C                = 11   both determinant and inverse.                   
C                = 01   inverse only.                                   
C                = 10   determinant only.                               
C                                                                       
C     On Return                                                         
C                                                                       
C        A       inverse of original matrix if requested.               
C                Otherwise unchanged.                                   
C                                                                       
C        DET     DOUBLE PRECISION(2)                                    
C                determinant of original matrix if requested.           
C                Otherwise not referenced.                              
C                Determinant = DET(1) * 10.0**DET(2)                    
C                with  1.0 .LE. DABS(DET(1)) .LT. 10.0                  
C                or  DET(1) .EQ. 0.0 .                                  
C                                                                       
C     Error Condition                                                   
C                                                                       
C        A division by zero will occur if the input factor contains     
C        a zero on the diagonal and the inverse is requested.           
C        It will not occur if the subroutines are called correctly      
C        and if DGECO has set RCOND .GT. 0.0 or DGEFA has set           
C        INFO .EQ. 0 .                                                  
C                                                                       
C     LINPACK.  This version dated 08/14/78 .                           
C     Cleve Moler, University of New Mexico, Argonne National Lab.      
C                                                                       
C     Subroutines and Functions                                         
C                                                                       
C     BLAS DAXPY,DSCAL,DSWAP                                            
C     Fortran DABS,MOD                                                  
C***REFERENCES  DONGARRA J.J., BUNCH J.R., MOLER C.B., STEWART G.W.,    
C                 *LINPACK USERS  GUIDE*, SIAM, 1979.                   
C***ROUTINES CALLED  DAXPY,DSCAL,DSWAP                                  
C***END PROLOGUE  DGEDI                                                 
      INTEGER LDA,N,IPVT(1),JOB                                         
      DOUBLE PRECISION A(LDA,1),DET(2),WORK(1)                          
C                                                                       
      DOUBLE PRECISION T                                                
      DOUBLE PRECISION TEN                                              
      INTEGER I,J,K,KB,KP1,L,NM1                                        
C                                                                       
C     COMPUTE DETERMINANT                                               
C                                                                       
C***FIRST EXECUTABLE STATEMENT  DGEDI                                   
      IF (JOB/10 .EQ. 0) GO TO 70                                       
         DET(1) = 1.0D0                                                 
         DET(2) = 0.0D0                                                 
         TEN = 10.0D0                                                   
         DO 50 I = 1, N                                                 
            IF (IPVT(I) .NE. I) DET(1) = -DET(1)                        
            DET(1) = A(I,I)*DET(1)                                      
C        ...EXIT                                                        
            IF (DET(1) .EQ. 0.0D0) GO TO 60                             
   10       IF (DABS(DET(1)) .GE. 1.0D0) GO TO 20                       
               DET(1) = TEN*DET(1)                                      
               DET(2) = DET(2) - 1.0D0                                  
            GO TO 10                                                    
   20       CONTINUE                                                    
   30       IF (DABS(DET(1)) .LT. TEN) GO TO 40                         
               DET(1) = DET(1)/TEN                                      
               DET(2) = DET(2) + 1.0D0                                  
            GO TO 30                                                    
   40       CONTINUE                                                    
   50    CONTINUE                                                       
   60    CONTINUE                                                       
   70 CONTINUE                                                          
C                                                                       
C     COMPUTE INVERSE(U)                                                
C                                                                       
      IF (MOD(JOB,10) .EQ. 0) GO TO 150                                 
         DO 100 K = 1, N                                                
            A(K,K) = 1.0D0/A(K,K)                                       
            T = -A(K,K)                                                 
            CALL DSCAL(K-1,T,A(1,K),1)                                  
            KP1 = K + 1                                                 
            IF (N .LT. KP1) GO TO 90                                    
            DO 80 J = KP1, N                                            
               T = A(K,J)                                               
               A(K,J) = 0.0D0                                           
               CALL DAXPY(K,T,A(1,K),1,A(1,J),1)                        
   80       CONTINUE                                                    
   90       CONTINUE                                                    
  100    CONTINUE                                                       
C                                                                       
C        FORM INVERSE(U)*INVERSE(L)                                     
C                                                                       
         NM1 = N - 1                                                    
         IF (NM1 .LT. 1) GO TO 140                                      
         DO 130 KB = 1, NM1                                             
            K = N - KB                                                  
            KP1 = K + 1                                                 
            DO 110 I = KP1, N                                           
               WORK(I) = A(I,K)                                         
               A(I,K) = 0.0D0                                           
  110       CONTINUE                                                    
            DO 120 J = KP1, N                                           
               T = WORK(J)                                              
               CALL DAXPY(N,T,A(1,J),1,A(1,K),1)                        
  120       CONTINUE                                                    
            L = IPVT(K)                                                 
            IF (L .NE. K) CALL DSWAP(N,A(1,K),1,A(1,L),1)               
  130    CONTINUE                                                       
  140    CONTINUE                                                       
  150 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE DGEFA(A,LDA,N,IPVT,INFO)                               
C***BEGIN PROLOGUE  DGEFA                                               
C***DATE WRITTEN   780814   (YYMMDD)                                    
C***REVISION DATE  861211   (YYMMDD)                                    
C***CATEGORY NO.  D2A1                                                  
C***KEYWORDS  LIBRARY=SLATEC(LINPACK),                                  
C             TYPE=DOUBLE PRECISION(SGEFA-S DGEFA-D CGEFA-C),           
C             GENERAL MATRIX,LINEAR ALGEBRA,MATRIX,MATRIX FACTORIZATION 
C***AUTHOR  MOLER, C. B., (U. OF NEW MEXICO)                            
C***PURPOSE  Factors a double precision matrix by Gaussian elimination. 
C***DESCRIPTION                                                         
C                                                                       
C     DGEFA factors a double precision matrix by Gaussian elimination.  
C                                                                       
C     DGEFA is usually called by DGECO, but it can be called            
C     directly with a saving in time if  RCOND  is not needed.          
C     (Time for DGECO) = (1 + 9/N)*(Time for DGEFA) .                   
C                                                                       
C     On Entry                                                          
C                                                                       
C        A       DOUBLE PRECISION(LDA, N)                               
C                the matrix to be factored.                             
C                                                                       
C        LDA     INTEGER                                                
C                the leading dimension of the array  A .                
C                                                                       
C        N       INTEGER                                                
C                the order of the matrix  A .                           
C                                                                       
C     On Return                                                         
C                                                                       
C        A       an upper triangular matrix and the multipliers         
C                which were used to obtain it.                          
C                The factorization can be written  A = L*U  where       
C                L  is a product of permutation and unit lower          
C                triangular matrices and  U  is upper triangular.       
C                                                                       
C        IPVT    INTEGER(N)                                             
C                an integer vector of pivot indices.                    
C                                                                       
C        INFO    INTEGER                                                
C                = 0  normal value.                                     
C                = K  if  U(K,K) .EQ. 0.0 .  This is not an error       
C                     condition for this subroutine, but it does        
C                     indicate that DGESL or DGEDI will divide by zero  
C                     if called.  Use  RCOND  in DGECO for a reliable   
C                     indication of singularity.                        
C                                                                       
C     LINPACK.  This version dated 08/14/78 .                           
C     Cleve Moler, University of New Mexico, Argonne National Lab.      
C                                                                       
C     Subroutines and Functions                                         
C                                                                       
C     BLAS DAXPY,DSCAL,IDAMAX                                           
C***REFERENCES  DONGARRA J.J., BUNCH J.R., MOLER C.B., STEWART G.W.,    
C                 *LINPACK USERS  GUIDE*, SIAM, 1979.                   
C***ROUTINES CALLED  DAXPY,DSCAL,IDAMAX                                 
C***END PROLOGUE  DGEFA                                                 
      INTEGER LDA,N,IPVT(1),INFO                                        
      DOUBLE PRECISION A(LDA,1)                                         
C                                                                       
      DOUBLE PRECISION T                                                
      INTEGER IDAMAX,J,K,KP1,L,NM1                                      
C                                                                       
C     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING                        
C                                                                       
C***FIRST EXECUTABLE STATEMENT  DGEFA                                   
      INFO = 0                                                          
      NM1 = N - 1                                                       
      IF (NM1 .LT. 1) GO TO 70                                          
      DO 60 K = 1, NM1                                                  
         KP1 = K + 1                                                    
C                                                                       
C        FIND L = PIVOT INDEX                                           
C                                                                       
         L = IDAMAX(N-K+1,A(K,K),1) + K - 1                             
         IPVT(K) = L                                                    
C                                                                       
C        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED          
C                                                                       
         IF (A(L,K) .EQ. 0.0D0) GO TO 40                                
C                                                                       
C           INTERCHANGE IF NECESSARY                                    
C                                                                       
            IF (L .EQ. K) GO TO 10                                      
               T = A(L,K)                                               
               A(L,K) = A(K,K)                                          
               A(K,K) = T                                               
   10       CONTINUE                                                    
C                                                                       
C           COMPUTE MULTIPLIERS                                         
C                                                                       
            T = -1.0D0/A(K,K)                                           
            CALL DSCAL(N-K,T,A(K+1,K),1)                                
C                                                                       
C           ROW ELIMINATION WITH COLUMN INDEXING                        
C                                                                       
            DO 30 J = KP1, N                                            
               T = A(L,J)                                               
               IF (L .EQ. K) GO TO 20                                   
                  A(L,J) = A(K,J)                                       
                  A(K,J) = T                                            
   20          CONTINUE                                                 
               CALL DAXPY(N-K,T,A(K+1,K),1,A(K+1,J),1)                  
   30       CONTINUE                                                    
         GO TO 50                                                       
   40    CONTINUE                                                       
            INFO = K                                                    
   50    CONTINUE                                                       
   60 CONTINUE                                                          
   70 CONTINUE                                                          
      IPVT(N) = N                                                       
      IF (A(N,N) .EQ. 0.0D0) INFO = N                                   
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DASUM(N,DX,INCX)                        
C***BEGIN PROLOGUE  DASUM                                               
C***DATE WRITTEN   791001   (YYMMDD)                                    
C***REVISION DATE  861211   (YYMMDD)                                    
C***CATEGORY NO.  D1A3A                                                 
C***KEYWORDS  LIBRARY=SLATEC(BLAS),                                     
C             TYPE=DOUBLE PRECISION(SASUM-S DASUM-D SCASUM-C),ADD,      
C             LINEAR ALGEBRA,MAGNITUDE,SUM,VECTOR                       
C***AUTHOR  LAWSON, C. L., (JPL)                                        
C           HANSON, R. J., (SNLA)                                       
C           KINCAID, D. R., (U. OF TEXAS)                               
C           KROGH, F. T., (JPL)                                         
C***PURPOSE  Sum of magnitudes of d.p. vector components                
C***DESCRIPTION                                                         
C                                                                       
C                B L A S  Subprogram                                    
C    Description of Parameters                                          
C                                                                       
C     --Input--                                                         
C        N  number of elements in input vector(s)                       
C       DX  double precision vector with N elements                     
C     INCX  storage spacing between elements of DX                      
C                                                                       
C     --Output--                                                        
C    DASUM  double precision result (zero if N .LE. 0)                  
C                                                                       
C     Returns sum of magnitudes of double precision DX.                 
C     DASUM = sum from 0 to N-1 of DABS(DX(1+I*INCX))                   
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,     
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*, 
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL       
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323 
C***ROUTINES CALLED  (NONE)                                             
C***END PROLOGUE  DASUM                                                 
C                                                                       
      DOUBLE PRECISION DX(1)                                            
C***FIRST EXECUTABLE STATEMENT  DASUM                                   
      DASUM = 0.D0                                                      
      IF(N.LE.0)RETURN                                                  
      IF(INCX.EQ.1)GOTO 20                                              
C                                                                       
C        CODE FOR INCREMENTS NOT EQUAL TO 1.                            
C                                                                       
      NS = N*INCX                                                       
          DO 10 I=1,NS,INCX                                             
          DASUM = DASUM + DABS(DX(I))                                   
   10     CONTINUE                                                      
      RETURN                                                            
C                                                                       
C        CODE FOR INCREMENTS EQUAL TO 1.                                
C                                                                       
C                                                                       
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 6.   
C                                                                       
   20 M = MOD(N,6)                                                      
      IF( M .EQ. 0 ) GO TO 40                                           
      DO 30 I = 1,M                                                     
         DASUM = DASUM + DABS(DX(I))                                    
   30 CONTINUE                                                          
      IF( N .LT. 6 ) RETURN                                             
   40 MP1 = M + 1                                                       
      DO 50 I = MP1,N,6                                                 
         DASUM = DASUM + DABS(DX(I)) + DABS(DX(I+1)) + DABS(DX(I+2))    
     1   + DABS(DX(I+3)) + DABS(DX(I+4)) + DABS(DX(I+5))                
   50 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE DAXPY(N,DA,DX,INCX,DY,INCY)                            
C***BEGIN PROLOGUE  DAXPY                                               
C***DATE WRITTEN   791001   (YYMMDD)                                    
C***REVISION DATE  861211   (YYMMDD)                                    
C***CATEGORY NO.  D1A7                                                  
C***KEYWORDS  LIBRARY=SLATEC(BLAS),                                     
C             TYPE=DOUBLE PRECISION(SAXPY-S DAXPY-D CAXPY-C),           
C             LINEAR ALGEBRA,TRIAD,VECTOR                               
C***AUTHOR  LAWSON, C. L., (JPL)                                        
C           HANSON, R. J., (SNLA)                                       
C           KINCAID, D. R., (U. OF TEXAS)                               
C           KROGH, F. T., (JPL)                                         
C***PURPOSE  D.P computation y = a*x + y                                
C***DESCRIPTION                                                         
C                                                                       
C                B L A S  Subprogram                                    
C    Description of Parameters                                          
C                                                                       
C     --Input--                                                         
C        N  number of elements in input vector(s)                       
C       DA  double precision scalar multiplier                          
C       DX  double precision vector with N elements                     
C     INCX  storage spacing between elements of DX                      
C       DY  double precision vector with N elements                     
C     INCY  storage spacing between elements of DY                      
C                                                                       
C     --Output--                                                        
C       DY  double precision result (unchanged if N .LE. 0)             
C                                                                       
C     Overwrite double precision DY with double precision DA*DX + DY.   
C     For I = 0 to N-1, replace  DY(LY+I*INCY) with DA*DX(LX+I*INCX) +  
C       DY(LY+I*INCY), where LX = 1 if INCX .GE. 0, else LX = (-INCX)*N 
C       and LY is defined in a similar way using INCY.                  
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,     
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*, 
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL       
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323 
C***ROUTINES CALLED  (NONE)                                             
C***END PROLOGUE  DAXPY                                                 
C                                                                       
      DOUBLE PRECISION DX(1),DY(1),DA                                   
C***FIRST EXECUTABLE STATEMENT  DAXPY                                   
      IF(N.LE.0.OR.DA.EQ.0.D0) RETURN                                   
      IF(INCX.EQ.INCY) IF(INCX-1) 5,20,60                               
    5 CONTINUE                                                          
C                                                                       
C        CODE FOR NONEQUAL OR NONPOSITIVE INCREMENTS.                   
C                                                                       
      IX = 1                                                            
      IY = 1                                                            
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1                                 
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1                                 
      DO 10 I = 1,N                                                     
        DY(IY) = DY(IY) + DA*DX(IX)                                     
        IX = IX + INCX                                                  
        IY = IY + INCY                                                  
   10 CONTINUE                                                          
      RETURN                                                            
C                                                                       
C        CODE FOR BOTH INCREMENTS EQUAL TO 1                            
C                                                                       
C                                                                       
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 4.   
C                                                                       
   20 M = MOD(N,4)                                                      
      IF( M .EQ. 0 ) GO TO 40                                           
      DO 30 I = 1,M                                                     
        DY(I) = DY(I) + DA*DX(I)                                        
   30 CONTINUE                                                          
      IF( N .LT. 4 ) RETURN                                             
   40 MP1 = M + 1                                                       
      DO 50 I = MP1,N,4                                                 
        DY(I) = DY(I) + DA*DX(I)                                        
        DY(I + 1) = DY(I + 1) + DA*DX(I + 1)                            
        DY(I + 2) = DY(I + 2) + DA*DX(I + 2)                            
        DY(I + 3) = DY(I + 3) + DA*DX(I + 3)                            
   50 CONTINUE                                                          
      RETURN                                                            
C                                                                       
C        CODE FOR EQUAL, POSITIVE, NONUNIT INCREMENTS.                  
C                                                                       
   60 CONTINUE                                                          
      NS = N*INCX                                                       
          DO 70 I=1,NS,INCX                                             
          DY(I) = DA*DX(I) + DY(I)                                      
   70     CONTINUE                                                      
      RETURN                                                            
      END                                                               
      DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)                 
C***BEGIN PROLOGUE  DDOT                                                
C***DATE WRITTEN   791001   (YYMMDD)                                    
C***REVISION DATE  861211   (YYMMDD)                                    
C***CATEGORY NO.  D1A4                                                  
C***KEYWORDS  LIBRARY=SLATEC(BLAS),                                     
C             TYPE=DOUBLE PRECISION(SDOT-S DDOT-D CDOTU-C),             
C             INNER PRODUCT,LINEAR ALGEBRA,VECTOR                       
C***AUTHOR  LAWSON, C. L., (JPL)                                        
C           HANSON, R. J., (SNLA)                                       
C           KINCAID, D. R., (U. OF TEXAS)                               
C           KROGH, F. T., (JPL)                                         
C***PURPOSE  D.P. inner product of d.p. vectors                         
C***DESCRIPTION                                                         
C                                                                       
C                B L A S  Subprogram                                    
C    Description of Parameters                                          
C                                                                       
C     --Input--                                                         
C        N  number of elements in input vector(s)                       
C       DX  double precision vector with N elements                     
C     INCX  storage spacing between elements of DX                      
C       DY  double precision vector with N elements                     
C     INCY  storage spacing between elements of DY                      
C                                                                       
C     --Output--                                                        
C     DDOT  double precision dot product (zero if N .LE. 0)             
C                                                                       
C     Returns the dot product of double precision DX and DY.            
C     DDOT = sum for I = 0 to N-1 of  DX(LX+I*INCX) * DY(LY+I*INCY)     
C     where LX = 1 if INCX .GE. 0, else LX = (-INCX)*N, and LY is       
C     defined in a similar way using INCY.                              
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,     
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*, 
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL       
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323 
C***ROUTINES CALLED  (NONE)                                             
C***END PROLOGUE  DDOT                                                  
C                                                                       
      DOUBLE PRECISION DX(1),DY(1)                                      
C***FIRST EXECUTABLE STATEMENT  DDOT                                    
      DDOT = 0.D0                                                       
      IF(N.LE.0)RETURN                                                  
      IF(INCX.EQ.INCY) IF(INCX-1) 5,20,60                               
    5 CONTINUE                                                          
C                                                                       
C         CODE FOR UNEQUAL OR NONPOSITIVE INCREMENTS.                   
C                                                                       
      IX = 1                                                            
      IY = 1                                                            
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1                                 
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1                                 
      DO 10 I = 1,N                                                     
         DDOT = DDOT + DX(IX)*DY(IY)                                    
        IX = IX + INCX                                                  
        IY = IY + INCY                                                  
   10 CONTINUE                                                          
      RETURN                                                            
C                                                                       
C        CODE FOR BOTH INCREMENTS EQUAL TO 1.                           
C                                                                       
C                                                                       
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 5.   
C                                                                       
   20 M = MOD(N,5)                                                      
      IF( M .EQ. 0 ) GO TO 40                                           
      DO 30 I = 1,M                                                     
         DDOT = DDOT + DX(I)*DY(I)                                      
   30 CONTINUE                                                          
      IF( N .LT. 5 ) RETURN                                             
   40 MP1 = M + 1                                                       
      DO 50 I = MP1,N,5                                                 
         DDOT = DDOT + DX(I)*DY(I) + DX(I+1)*DY(I+1) +                  
     1   DX(I + 2)*DY(I + 2) + DX(I + 3)*DY(I + 3) + DX(I + 4)*DY(I + 4)
   50 CONTINUE                                                          
      RETURN                                                            
C                                                                       
C         CODE FOR POSITIVE EQUAL INCREMENTS .NE.1.                     
C                                                                       
   60 CONTINUE                                                          
      NS = N*INCX                                                       
          DO 70 I=1,NS,INCX                                             
          DDOT = DDOT + DX(I)*DY(I)                                     
   70     CONTINUE                                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE DSCAL(N,DA,DX,INCX)                                    
C***BEGIN PROLOGUE  DSCAL                                               
C***DATE WRITTEN   791001   (YYMMDD)                                    
C***REVISION DATE  861211   (YYMMDD)                                    
C***CATEGORY NO.  D1A6                                                  
C***KEYWORDS  LIBRARY=SLATEC(BLAS),                                     
C             TYPE=DOUBLE PRECISION(SSCAL-S DSCAL-D CSCAL-C),           
C             LINEAR ALGEBRA,SCALE,VECTOR                               
C***AUTHOR  LAWSON, C. L., (JPL)                                        
C           HANSON, R. J., (SNLA)                                       
C           KINCAID, D. R., (U. OF TEXAS)                               
C           KROGH, F. T., (JPL)                                         
C***PURPOSE  D.P. vector scale x = a*x                                  
C***DESCRIPTION                                                         
C                                                                       
C                B L A S  Subprogram                                    
C    Description of Parameters                                          
C                                                                       
C     --Input--                                                         
C        N  number of elements in input vector(s)                       
C       DA  double precision scale factor                               
C       DX  double precision vector with N elements                     
C     INCX  storage spacing between elements of DX                      
C                                                                       
C     --Output--                                                        
C       DX  double precision result (unchanged if N.LE.0)               
C                                                                       
C     Replace double precision DX by double precision DA*DX.            
C     For I = 0 to N-1, replace DX(1+I*INCX) with  DA * DX(1+I*INCX)    
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,     
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*, 
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL       
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323 
C***ROUTINES CALLED  (NONE)                                             
C***END PROLOGUE  DSCAL                                                 
C                                                                       
      DOUBLE PRECISION DA,DX(1)                                         
C***FIRST EXECUTABLE STATEMENT  DSCAL                                   
      IF(N.LE.0)RETURN                                                  
      IF(INCX.EQ.1)GOTO 20                                              
C                                                                       
C        CODE FOR INCREMENTS NOT EQUAL TO 1.                            
C                                                                       
      NS = N*INCX                                                       
          DO 10 I = 1,NS,INCX                                           
          DX(I) = DA*DX(I)                                              
   10     CONTINUE                                                      
      RETURN                                                            
C                                                                       
C        CODE FOR INCREMENTS EQUAL TO 1.                                
C                                                                       
C                                                                       
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 5.   
C                                                                       
   20 M = MOD(N,5)                                                      
      IF( M .EQ. 0 ) GO TO 40                                           
      DO 30 I = 1,M                                                     
        DX(I) = DA*DX(I)                                                
   30 CONTINUE                                                          
      IF( N .LT. 5 ) RETURN                                             
   40 MP1 = M + 1                                                       
      DO 50 I = MP1,N,5                                                 
        DX(I) = DA*DX(I)                                                
        DX(I + 1) = DA*DX(I + 1)                                        
        DX(I + 2) = DA*DX(I + 2)                                        
        DX(I + 3) = DA*DX(I + 3)                                        
        DX(I + 4) = DA*DX(I + 4)                                        
   50 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE DSWAP(N,DX,INCX,DY,INCY)                               
C***BEGIN PROLOGUE  DSWAP                                               
C***DATE WRITTEN   791001   (YYMMDD)                                    
C***REVISION DATE  861211   (YYMMDD)                                    
C***CATEGORY NO.  D1A5                                                  
C***KEYWORDS  LIBRARY=SLATEC(BLAS),                                     
C             TYPE=DOUBLE PRECISION(SSWAP-S DSWAP-D CSWAP-C ISWAP-I),   
C             INTERCHANGE,LINEAR ALGEBRA,VECTOR                         
C***AUTHOR  LAWSON, C. L., (JPL)                                        
C           HANSON, R. J., (SNLA)                                       
C           KINCAID, D. R., (U. OF TEXAS)                               
C           KROGH, F. T., (JPL)                                         
C***PURPOSE  Interchange d.p. vectors                                   
C***DESCRIPTION                                                         
C                                                                       
C                B L A S  Subprogram                                    
C    Description of Parameters                                          
C                                                                       
C     --Input--                                                         
C        N  number of elements in input vector(s)                       
C       DX  double precision vector with N elements                     
C     INCX  storage spacing between elements of DX                      
C       DY  double precision vector with N elements                     
C     INCY  storage spacing between elements of DY                      
C                                                                       
C     --Output--                                                        
C       DX  input vector DY (unchanged if N .LE. 0)                     
C       DY  input vector DX (unchanged if N .LE. 0)                     
C                                                                       
C     Interchange double precision DX and double precision DY.          
C     For I = 0 to N-1, interchange  DX(LX+I*INCX) and DY(LY+I*INCY),   
C     where LX = 1 if INCX .GE. 0, else LX = (-INCX)*N, and LY is       
C     defined in a similar way using INCY.                              
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,     
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*, 
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL       
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323 
C***ROUTINES CALLED  (NONE)                                             
C***END PROLOGUE  DSWAP                                                 
C                                                                       
      DOUBLE PRECISION DX(1),DY(1),DTEMP1,DTEMP2,DTEMP3                 
C***FIRST EXECUTABLE STATEMENT  DSWAP                                   
      IF(N.LE.0)RETURN                                                  
      IF(INCX.EQ.INCY) IF(INCX-1) 5,20,60                               
    5 CONTINUE                                                          
C                                                                       
C       CODE FOR UNEQUAL OR NONPOSITIVE INCREMENTS.                     
C                                                                       
      IX = 1                                                            
      IY = 1                                                            
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1                                 
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1                                 
      DO 10 I = 1,N                                                     
        DTEMP1 = DX(IX)                                                 
        DX(IX) = DY(IY)                                                 
        DY(IY) = DTEMP1                                                 
        IX = IX + INCX                                                  
        IY = IY + INCY                                                  
   10 CONTINUE                                                          
      RETURN                                                            
C                                                                       
C       CODE FOR BOTH INCREMENTS EQUAL TO 1                             
C                                                                       
C                                                                       
C       CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 3.    
C                                                                       
   20 M = MOD(N,3)                                                      
      IF( M .EQ. 0 ) GO TO 40                                           
      DO 30 I = 1,M                                                     
        DTEMP1 = DX(I)                                                  
        DX(I) = DY(I)                                                   
        DY(I) = DTEMP1                                                  
   30 CONTINUE                                                          
      IF( N .LT. 3 ) RETURN                                             
   40 MP1 = M + 1                                                       
      DO 50 I = MP1,N,3                                                 
        DTEMP1 = DX(I)                                                  
        DTEMP2 = DX(I+1)                                                
        DTEMP3 = DX(I+2)                                                
        DX(I) = DY(I)                                                   
        DX(I+1) = DY(I+1)                                               
        DX(I+2) = DY(I+2)                                               
        DY(I) = DTEMP1                                                  
        DY(I+1) = DTEMP2                                                
        DY(I+2) = DTEMP3                                                
   50 CONTINUE                                                          
      RETURN                                                            
   60 CONTINUE                                                          
C                                                                       
C     CODE FOR EQUAL, POSITIVE, NONUNIT INCREMENTS.                     
C                                                                       
      NS = N*INCX                                                       
        DO 70 I=1,NS,INCX                                               
        DTEMP1 = DX(I)                                                  
        DX(I) = DY(I)                                                   
        DY(I) = DTEMP1                                                  
   70   CONTINUE                                                        
      RETURN                                                            
      END                                                               
      INTEGER FUNCTION IDAMAX(N,DX,INCX)                                
C***BEGIN PROLOGUE  IDAMAX                                              
C***DATE WRITTEN   791001   (YYMMDD)                                    
C***REVISION DATE  861211   (YYMMDD)                                    
C***CATEGORY NO.  D1A2                                                  
C***KEYWORDS  LIBRARY=SLATEC(BLAS),                                     
C             TYPE=DOUBLE PRECISION(ISAMAX-S IDAMAX-D ICAMAX-C),        
C             LINEAR ALGEBRA,MAXIMUM COMPONENT,VECTOR                   
C***AUTHOR  LAWSON, C. L., (JPL)                                        
C           HANSON, R. J., (SNLA)                                       
C           KINCAID, D. R., (U. OF TEXAS)                               
C           KROGH, F. T., (JPL)                                         
C***PURPOSE  Find the smallest index of that component of a d.p. vector 
C            having the maximum magnitude.                              
C***DESCRIPTION                                                         
C                                                                       
C                B L A S  Subprogram                                    
C    Description of Parameters                                          
C                                                                       
C     --Input--                                                         
C        N  number of elements in input vector(s)                       
C       DX  double precision vector with N elements                     
C     INCX  storage spacing between elements of DX                      
C                                                                       
C     --Output--                                                        
C   IDAMAX  smallest index (zero if N .LE. 0)                           
C                                                                       
C     Find smallest index of maximum magnitude of double precision DX.  
C     IDAMAX =  first I, I = 1 to N, to minimize  ABS(DX(1-INCX+I*INCX) 
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,     
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*, 
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL       
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323 
C***ROUTINES CALLED  (NONE)                                             
C***END PROLOGUE  IDAMAX                                                
C                                                                       
      DOUBLE PRECISION DX(*),DMAX,XMAG                                  
C***FIRST EXECUTABLE STATEMENT  IDAMAX                                  
      IDAMAX = 0                                                        
      IF(N.LE.0) RETURN                                                 
      IDAMAX = 1                                                        
      IF(N.LE.1)RETURN                                                  
      IF(INCX.EQ.1)GOTO 20                                              
C                                                                       
C        CODE FOR INCREMENTS NOT EQUAL TO 1.                            
C                                                                       
      DMAX = DABS(DX(1))                                                
      NS = N*INCX                                                       
      II = 1                                                            
          DO 10 I = 1,NS,INCX                                           
          XMAG = DABS(DX(I))                                            
          IF(XMAG.LE.DMAX) GO TO 5                                      
          IDAMAX = II                                                   
          DMAX = XMAG                                                   
    5     II = II + 1                                                   
   10     CONTINUE                                                      
      RETURN                                                            
C                                                                       
C        CODE FOR INCREMENTS EQUAL TO 1.                                
C                                                                       
   20 DMAX = DABS(DX(1))                                                
      DO 30 I = 2,N                                                     
          XMAG = DABS(DX(I))                                            
          IF(XMAG.LE.DMAX) GO TO 30                                     
          IDAMAX = I                                                    
          DMAX = XMAG                                                   
   30 CONTINUE                                                          
      RETURN                                                            
      END                                                               

