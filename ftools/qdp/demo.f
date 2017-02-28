C---
C DEMO.FOR demonstrates how to call PLT from a Fortran program.
C---
C [AFT]
C---
      INTEGER   MXROW, MXCOL, MXVEC, MXCMD
      PARAMETER (MXROW=200, MXCOL=3, MXVEC=2, MXCMD=10)
      CHARACTER CMD(MXCMD)*72
      REAL      Y(MXROW, MXCOL)
      INTEGER   IERY(MXVEC), NVEC, NPTS, NCMD, IER
      INTEGER   I
C---
C Create two vectors.  The first vector will contain the X locations
C and a symmetric error (with constant value of 0.5).  The second
C vector will contain X*X and no error.
      NVEC=2
      IERY(1)=1
      IERY(2)=0
      NPTS=100
      DO 190 I=1,NPTS
         Y(I,1)=I
         Y(I,2)=.5
         Y(I,3)=Y(I,1)*Y(I,1)
  190 CONTINUE
C---
C Now add a couple of commands, to make plot look nicer.
      CMD(1)='LAB X Time (sec)'
      CMD(2)='LAB Y Distance (m)'
      CMD(3)='LAB T Made with DEMO.FOR'
      CMD(4)='LINE STEP 2'
      NCMD=4
C---
C Call the PLT subroutine.
      CALL PLT(Y, IERY, MXROW, NPTS, NVEC, CMD, NCMD, IER)
      END
