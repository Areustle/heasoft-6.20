        SUBROUTINE DGAUSJ(A,N,NP,B,M,MP,IERR)
C
CC  Numerical Routines linear equation solution routine
C
C************************ FFORM VERSION 1.0 ************  1-MAR-88 15:03
C
CA  author : Press et al.      date:
CU  update : J. Bloch          date:  6-SEP-1987
CU  update : SLS               date:  1-MAR-1988 14:42
C   update: FITS calls (Srilal Weera Dec 96)
C
CT  status: not tested
C
C   general description 
CG  Linear equation solution by Gauss-Jordan elimination.  A is an
CG  input matrix of N x N elements, stored in an array of physical
CG  dimensions NP x NP.  B is an input matrix of N x M containing the
CG  M right-hand side vectors, stored in an array of physical
CG  dimensions NP x MP.  On output, A is replaced by its matrix
CG  inverse, and B is replaced by the corresponding set of solution
CG  vectors.
CG  Modified by J. Bloch to be a double precision matrix version of the
CG  subroutine GAUSSJ in the book NUMERICAL RECIPES by Press et al.
C
C   call_var.          type I/O description 
CP  A                   R8  I/O Updated REAL*8 Matrix which will be
CP                              replaced by it's inverse.
CP  N                   I4  I   actual order of matrix A
CP  NP                  I4      actual dimenstions of array A.
CP  B                   R8  I/O Updated array containing M right hand
CP                                  side vectors. Upon return, B
CP                                  contains the solution vectors.
CP  M                   I4  I   number of right hand side vectors.
CP  MP                  I4  I   actual second dimension size of B.
CP  IERR                I4  I/O = 0 no errors
CP                              = 3 singular matrix
C
C   include_block name          description 
C
C   routines_called    type     description 
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
C       PARAMETER NMAX=200
C
        INTEGER*4 I, ICOL, IERR, IROW, J, K, L, LL
        INTEGER*4 M, MP, N, NP
        INTEGER*4 IPIV(200), INDXR(200), INDXC(200)
C
        REAL*8 A(NP,NP), B(NP,MP), BIG, DUM, PIVINV
C
	ICOL =0
	IROW =0
        IERR = 0
        DO 11 J=1,N
            IPIV(J) = 0
   11   ENDDO
C
C  Start the main loop over the columns to be reduced
C
        I = 0
        DO WHILE ((I .LT. N) .AND. (IERR .EQ. 0))
            I = I + 1
            BIG = 0.D0
C
C  Start the outer loop of the search for the pivot element
C
            J = 0
            DO WHILE ((J .LT. N) .AND. (IERR .EQ. 0))
                J = J + 1
                IF(IPIV(J) .NE. 1) THEN
                    K = 0
                    DO WHILE ((K .LT. N) .AND. (IERR .EQ. 0))
                        K = K + 1
                        IF (IPIV(K) .EQ. 0) THEN
                            IF (ABS(A(J,K)) .GE. BIG) THEN
                                BIG = DABS(A(J,K))
                                IROW = J
                                ICOL = K
                            ENDIF
                        ELSE IF (IPIV(K) .GT. 1) THEN
                            IERR = 3
                          call fcecho('DGAUSJ, singular matrix, pos 1')
                        ENDIF
                    ENDDO
                ENDIF
            ENDDO
            IF(IERR .EQ. 0) THEN
                IPIV(ICOL) = IPIV(ICOL) + 1
C
C  We now have the pivot element, so we interchange rows, if needed,
C  to put the pivot element on the diagonal.  The columns are not
C  physically interchanged, only relabeled: INDX(I), the column of the
C  Ith pivot element, is the Ith column that is reduced, while INDXR(I)
C  is the row in which that pivot element was origially located.  If
C  INDXR(I) =/ INDXC(I) there is an implied column interchange.  With
C  this form of bookkeeping, the solution B's will end up in the correct
C  order, and the inverse matrix will be scrambled by columns.
C
                IF (IROW .NE. ICOL) THEN
                    DO 14 L=1,N
                        DUM = A(IROW,L)
                        A(IROW,L) = A(ICOL,L)
                        A(ICOL,L) = DUM
   14               ENDDO
C
                    DO 15 L=1,M
                        DUM = B(IROW,L)
                        B(IROW,L) = B(ICOL,L)
                        B(ICOL,L) = DUM
   15               ENDDO
                ENDIF
C
C  We are now ready to divide the pivot row by the pivot element
C  located at IROW and ICOL
C
                INDXR(I) = IROW
                INDXC(I) = ICOL
                IF (A(ICOL,ICOL) .EQ. 0.) THEN
                    IERR = 3
                    call fcecho('DGAUSJ, singular matrix, pos 2')
                ELSE
                    PIVINV = 1.D0/A(ICOL,ICOL)
                    A(ICOL,ICOL) = 1.D0
                    DO 16 L=1,N
                        A(ICOL,L) = A(ICOL,L)*PIVINV
   16               ENDDO
C
                    DO 17 L=1,M
                        B(ICOL,L) = B(ICOL,L)*PIVINV
   17               ENDDO
C
C  Next we reduce the rows, except for the pivot row
C
                    DO 21 LL=1,N
                        IF(LL .NE. ICOL)THEN
                            DUM = A(LL,ICOL)
                            A(LL,ICOL) = 0.
                            DO 18 L=1,N
                                A(LL,L) = A(LL,L) - A(ICOL,L)*DUM
   18                       ENDDO
                            DO 19 L=1,M
                                B(LL,L) = B(LL,L) - B(ICOL,L)*DUM
   19                       ENDDO
                        ENDIF
   21               ENDDO
                ENDIF
            ENDIF
   22   ENDDO
C
C  This is the end of the main loop over the columns of the reduction.
C  Now unscramble the solution in view of the column interchanges.
C  We do this by interchanging pairs of columns in the order that the
C  permutation was built up.
C
        IF(IERR .EQ. 0) THEN
            DO 24 L=N,1,-1
                IF(INDXR(L).NE.INDXC(L))THEN
                    DO 23 K=1,N
                        DUM = A(K,INDXR(L))
                        A(K,INDXR(L)) = A(K,INDXC(L))
                        A(K,INDXC(L)) = DUM
   23               ENDDO
                ENDIF
   24       ENDDO
        ENDIF
C
        RETURN
        END
