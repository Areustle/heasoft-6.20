        SUBROUTINE DCVSRT(COVAR,NCVM,MA,LISTA,MFIT)
C
CC  Repacks the covarience matrix for MQMLMN
C
C************************ FFORM VERSION 1.0 ************  1-MAR-88 14:40
C
CA  author : Press et al.      date:
CU  update : J. Bloch          date:
CU  update : SLS               date:  1-MAR-1988 14:39
C
CT  status: not tested
C
C   general description 
CG  DCVSRT is the REAL*8 version of routine COVSRT found in "Numerical
CG  Recipes" by Press et al. See p 515 for details. The only difference
CG  is that array COVAR is REAL*8 instead of single precision.  Given
CG  the covarience matrix COVAR of a fit MFIT of MA total parameters,
CG  and there ordering LISTA, repack the covarience matrix to the true
CG  order of the parameters.  Elements associated with fixed parameters
CG  will be zero.  NCVM is the physical dimension of COVAR
C
C   call_var.          type I/O description 
CP  MA                  I4  I   total number of parameters
CP  MFIT                I4  I   number of active parameters
CP  NCVM                I4      dimension of COVAR array
CP  COVAR               R8  I/O covarience matrix
CP  LISTA               I4  I   position vector
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
        INTEGER*4 I, J, MA, MFIT, LISTA(MFIT), NCVM
C
        REAL*8 COVAR(NCVM,NCVM), SWAP
C
C  Zero all elements below the diagonal
C
        DO 12 J=1,MA-1
            DO 11 I=J+1,MA
                COVAR(I,J) = 0.
   11       ENDDO
   12   ENDDO
C
C  Repack off-diagonal elements of fit into correct locations
C  below the diagonal
C
        DO 14 I=1,MFIT-1
            DO 13 J=I+1,MFIT
                IF(LISTA(J) .GT. LISTA(I)) THEN
                    COVAR(LISTA(J),LISTA(I)) = COVAR(I,J)
                ELSE
                    COVAR(LISTA(I),LISTA(J)) = COVAR(I,J)
                ENDIF
   13       ENDDO
   14   ENDDO
C
C  Temporarily store origial elements in top row, and zero the diagonal
C
        SWAP = COVAR(1,1)
C
        DO 15 J=1,MA
            COVAR(1,J) = COVAR(J,J)
            COVAR(J,J) = 0.
   15   ENDDO
C
        COVAR(LISTA(1),LISTA(1)) = SWAP
C
C  Now sort the elements into proper order on diagonal
C
        DO 16 J=2,MFIT
            COVAR(LISTA(J),LISTA(J)) = COVAR(1,J)
   16   ENDDO
C
C  Finally, fill in above the diagonal by symmetry
C
        DO 18 J=2,MA
            DO 17 I=1,J-1
                COVAR(I,J) = COVAR(J,I)
   17       ENDDO
   18   ENDDO
C
        RETURN
        END
