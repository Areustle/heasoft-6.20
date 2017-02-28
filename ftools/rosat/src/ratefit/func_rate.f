        SUBROUTINE FUNC (TIME,SSX,ALOC,YMODEL,DYDA,DY2DA2,
     +      LISTA,MFIT,MA)
C
CC  Calculates Prescott function distribution and partial derivatives
C
C************************ FFORM VERSION 1.0 ************ 21-APR-88 08:24
C
CA  author : GRH
CU  update : SLS               date: 20-APR-1988 16:21
CU  update : SLS               date: 26-SEP-1989 14:10 
CU  update : SLS               date: 8-NOV-1989 15:00  
C
CT  status: not tested
C
C   general description 
CG  PFUNCS is the model value subroutine for PRES_FIT for use with the
CG  fitting subroutine MQMLMN.FOR. For a given charge value and set of
CG  Prescott function parameters, it returns the
CG  model value of the pulse height as well as the required partial
CG  derivatives for the marquardt-Jordan method.
C
C   call_var.          type I/O description 
CP  CHG                 R4  I/? charge value.
CP  A                   R4  I/? array of model values.
CP                              A(1) = constant
CP                              A(2) = scale factor for dipole
CP                              A(3) = longitude of dipole axis
CP                              A(4) = latitude of dipole axis
CP                              A(5) = scale factor for qpole
CP                              A(6) = longitude of qpole axis
CP                              A(7) = latitude of qpole axis
CP  YMOD                R4      value of the model pulse height.
CP  DYDA                R4  I/O array of first partial derivatives
CP                              w.r.t. each parameter. (Only the
CP                              parameters actually being fit are
CP                              computed.)
CP  DY2DA2              R4  I/O array of second partial derivatives
CP                              w.r.t. the parameters.  (Only those
CP                              parameters being fit will be computed.)
CP  LISTA               I4  I   list of parameters being fitted.
CP  MFIT                I4  I   number of parameters being fit.
CP  MA                  I4  I   number of parameters in model.
CP                              (Should be 15)
C
C   include_block name          description 
CI  R$COMMON:CGENL.CMN          
CI  CBCKLN.CMN                  
C
C   routines_called    type     description 
CR  HFLAG               R4      
CR  PRESCO              R4      
CR  WRFLAG              SR      
C
C   extensions/system calls     description 
CX  MTH$EXP                     
C
C***********************************************************************
C
        IMPLICIT NONE
C
        SAVE
C
        INTEGER*4 I, II, MFIT, LISTA(MFIT), MA
C
        REAL*4 ALOC(MA), DY2DA2(10,10), DYDA(10), SSX, THETA, 
     +      TIME, YMODEL
C
        REAL*8 A(10), DTIME, DSSX, YMOD
C
        DO I=1,10
            A(I) = DBLE(ALOC(I))
        ENDDO
        DTIME = DBLE(TIME)
        DSSX = DBLE(SSX)
C
C  Compute the model value
C
        THETA = AMOD(TIME,5760.)
        THETA = THETA*6.283185/5760.
        YMOD = A(1) + 1.E-5*A(2)*DTIME + 1.E-10*A(3)*DTIME*DTIME + 
     +      1.E-15*A(4)*DTIME*DTIME*DTIME + 
     +      1.E-20*A(5)*DTIME*DTIME*DTIME*DTIME + 
     +      DSSX*(A(6) + 1.E-5*A(7)*DTIME) + 
     +      (A(8) + 1.E-5*A(9)*DTIME)*SIN(THETA+A(10))
        YMODEL = SNGL(YMOD)
C
C  Compute values on intervals to prepare for computation
C  of numerical derivatives.
C
        DYDA(1) = 1
        DYDA(2) = 1.E-5*DTIME
        DYDA(3) = 1.E-10*DTIME*DTIME
        DYDA(4) = 1.E-15*DTIME*DTIME*DTIME
        DYDA(5) = 1.E-20*DTIME*DTIME*DTIME*DTIME
        DYDA(6) = DSSX
        DYDA(7) = DSSX*1.E-5*DTIME
        DYDA(8) = SIN(THETA+A(10))
        DYDA(9) = 1.E-5*DTIME*SIN(THETA+A(10))
        DYDA(10) = (A(8) + 1.E-5*A(9)*DTIME)*COS(THETA+A(10))
C
        DO I=1,7
            DO II=1,10
                DY2DA2(I,II) = 0
            ENDDO
        ENDDO
        DO I=8,10
            DO II=1,7
                DY2DA2(I,II) = 0
            ENDDO
        ENDDO
        DY2DA2(8,8) = 0
        DY2DA2(8,9) = 0
        DY2DA2(8,10) = COS(THETA+A(10))
        DY2DA2(9,8) = 0
        DY2DA2(9,9) = 0
        DY2DA2(9,10) = 1.E-5*DTIME*COS(THETA+A(10))
        DY2DA2(10,8) = 0
        DY2DA2(10,9) = 0
        DY2DA2(10,10) = -(A(8) + 1.E-5*A(9)*DTIME)*SIN(THETA+A(10))
C
        RETURN
        END
