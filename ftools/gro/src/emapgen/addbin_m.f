CCCCCCCCCCCCCCCCCCCCCCCC MAPGEN.SOURCE(ADDBIN_M) CCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  ROUTINE NAME:  ADDBIN_M
CH1
CH1  $Id: addbin_m.f,v 1.3 2013/05/21 19:08:24 irby Exp $
CH1
CH1  PROGRAMMER(S) AND COMPLETION DATE:
CH1     ALBERT ETIENNE - S.T.X. - 02/13/89
CH1
CH1  FUNCTION: INCREMENTS THE CURRENT MAP BIN BY 1. ALLOWS THE ARRAY
CH1            SIZES TO BE VARIABLE.
CH1
CH1  SOFTWARE SYSTEM AND SPACECRAFT:  EGRET PROJECT
CH1
CH1  COMPUTER AND LANGUAGE:  IBM 3081 - VS FORTRAN
CH1
CH2  CALLING SEQUENCE:  CALL ADDBIN_M(MAP,N1,N2,N3,I,J,K,ENFLAG)
CH2     ARGUMENT    TYPE   I/O                 DESCRIPTION
CH2     --------    ----   ---  ----------------------------------------
CH2	MAP	    I*2    I/O  MAP ARRAY OF THE BIN COUNTS
CH2	N1	    I*4	    I   MAP FIRST DIMENSION
CH2	N2	    I*4	    I   MAP SECOND DIMENSION
CH2	N3	    I*4	    I   MAP THIRD DIMENSION
CH2	I	    I*4     I   BIN INDEX INTO MAP ON FIRST DIMENSION
CH2	J	    I*4     I   BIN INDEX INTO MAP ON SECOND DIMENSION 
CH2	I	    I*4     I   BIN INDEX INTO MAP ON THIRD DIMENSION
CH2     ENFLAG      L*4     O   DETERMINES IF EACH ENERGY LEVEL HAS DATA
CH2
CH2  CALLED BY:  BATOF, BRECTN, BPOLAR
CH2
CH2  CALLS: NONE
CH2
CH3 COMMON USE: NONE
CH3
CH3 SIGNIFICANT LOCAL VARIABLES:  NONE
CH3   VARIABLE   TYPE   INI. VAL.               DESCRIPTION
CH3   --------   ----   ---------  -----------------------------------
CH3
CH4  LOGICAL UNITS USED:   UNIT #                DESCRIPTION
CH4                        ------    -----------------------------------
CH4                        NONE
CH4
CH4  METHOD:
CH4     INCREMENT THE CURRENT MAP BIN BY 1
CH4     RECORD THAT THERE IS AT LEAST 1 NON-ZERO BIN FOR THAT ENERGY
CH4  END ADDBIN_M
CH4
CH5 $Log: addbin_m.f,v $
CH5 Revision 1.3  2013/05/21 19:08:24  irby
CH5 Change character*n to character(n) to silence warnings: "Obsolescent
CH5 feature: Old-style character length".
CH5
CH5 Revision 1.2  1998/09/30 18:24:16  peachey
CH5 Changed addbin to addbin_m and celgal to celgal_m
CH5
c Revision 1.1  1998/09/30  17:01:01  peachey
c New tool, delivered by Jeff Silvis
c
C Revision 1.1  1992/03/27  15:28:19  albert
C Initial revision
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE ADDBIN_M(MAP,N1,N2,N3,I,J,K,ENFLAG)
      implicit none 

       INTEGER N1,N2,N3,I,J,K
      INTEGER*2 MAP(N1,N2,N3)
      LOGICAL   ENFLAG(10)

      character(80)	id
      common	/id/	id
      id = '$Id: addbin_m.f,v 1.3 2013/05/21 19:08:24 irby Exp $'

C---> INCREMENT THE BIN ARRAY FOR CURRENT BIN
      MAP(I,J,K) = MAP(I,J,K) + 1
      ENFLAG(K) = .TRUE.

      RETURN
CHCCCCCCCCCCCCCCCCCCCCC END MAPGEN.SOURCE(ADDBIN_M) CCCCCCCCCCCCCCCCCCCCCC
      END
