C This module contains the template for a user-defined function
C that can be used the the PLT/FIT software.  To create a new user
C function you should copy/rename this routine and install your
C own code into it.  You are particularly encouraged to create a
C new version of UINFO that gives your component and parameters
C different names.  Once the new routine is written, you can test
C it by explicitly linking to it.  For example,
C
C $ LINK QDP,UFNYDEMO,XANADU:[LIB]XANLIB/LI
C
C Once you have a working function, you should test it in PLT.  Use
C the "MOdel ?" command to see if your component is listed.  If not
C make sure you have linked a new version, and are running that new
C version.  Next define a model that is composed only of your new
C component, and enter a resonable set of parameters.  Do not attempt
C to fit at this time, but rather just plot the data and model.  Use the
C "Fit Plot 200" command to ensure that the plot is evalutate at 200
C points over the visible range.  Is the plotted function what you
C expected? If it is not then you should try to figure out what has
C gone wrong.
C
C Once the function is doing what you expect, then you can try to fit
C it.  If certain parameter values can cause a program crash, then you
C should write a version of ULIMIT that prohibits these values.
C---
      SUBROUTINE UINFO(IPAR, CNAME, NPAR)
      INTEGER   IPAR, NPAR
      CHARACTER CNAME*(*)
C---
C When FIT starts, it calls UINFO with IPAR set equal to 0 to get the
C name of the user component and the number of parameters.  This component
C name will be included in the names of the built-in components, and
C therefore, should not match any existing name (such as CONS, LINE,
C etc.)  If the user component is selected, then UINFO will be called
C for each parameter to get the name of that parameter.
C---
C IPAR    I    The parameter number.
C CNAME     O  The name of the paramter IPAR.  Note if IPAR=0, then
C              -return the name of the model.
C NPAR      O  The number of parameters in your user model.
C---
C Number of parameters
      NPAR=1
      IF(IPAR.EQ.0) THEN
C Component name
         CNAME='DEMO'
      ELSE IF(IPAR.EQ.1) THEN
C Name of paramter 1
         CNAME='U1'
      END IF
      RETURN
      END
C*********
      SUBROUTINE ULIMIT(PVAL, PLIM, NT, NPAR)
      REAL      PVAL(*), PLIM(3,*)
      INTEGER   NT, NPAR
C---
C ULIMIT is always called after any parameter values have been changed
C and before UFNY is called.  The purpose of ULIMIT is two-fold.  First,
C it should check the parameter values in PVAL and adjust any that may
C cause a problem in UFNY (for example, if UFNY divides by a parameter
C value, then ULIMIT should ensure that the parameter does not equal
C zero).  Second, ULIMIT can be used to set up any initial data that
C UFNY needs.  Since, UFNY is often called many times with the same
C parameter set, this can result in a speedup.  The parameter values
C are stored in PVAL(NT) to PVAL(NT+NPAR-1).  The PLIM array contains
C SIG, PLO, and PHI.  If PLIM(1,I) is less than zero, then that parameter
C is frozen and you should not adjust the parameter value.  Also, if
C PLO<PHI, then an effective range is active and you should not adjust
C a parameter outside that range.
C---
C PVAL(*)   I/O  The current parameter values
C PLIM(1,*) I    If <0 then the corresponding parameter is frozen
C NT        I    Pointer to first parameter value in array PVAL(*)
C NPAR      I    Number of parameters
C---
      REAL RJUNK
      INTEGER IJUNK

      IJUNK=NPAR
      RJUNK=PVAL(NT)
      RJUNK=PLIM(1,1)

      RETURN
      END
C*********
      REAL FUNCTION UFNY(X, PVAL, NT, NPAR)
      REAL      X, PVAL(*)
      INTEGER   NT, NPAR
C---
C UFNY the function that actually calculates user component at
C location X with parameter values given by PVAL.
C This demo version calculates a constant.
C---
C X       I    The current X value
C PVAL    I    The current parameter values
C NT      I    Pointer to first parameter value in array PVAL(*)
C NPAR    I    Number of parameters
C---
      REAL RJUNK
      INTEGER IJUNK

      RJUNK=X
      IJUNK=NPAR

      UFNY=PVAL(NT)
      RETURN
      END
C*********
      SUBROUTINE UDERIV(X, PVAL, PLIM, DERIV, NT, NPAR)
      REAL      X, PVAL(*), PLIM(3,*), DERIV(*)
      INTEGER   NT, NPAR
C---
C UDERIV should calculate the derivative of the UFNY function with respect
C to each parameter.  The DEMO version evaluates the derivative
C numerically and hence you may be able to use it without modification.
C When using the DEMO method you should try to scale the problem so
C that parameter values are in the range .1-100, values outside this
C range work, but the convergence can be slower.
C
C If PLIM(1,I)=-1 then that parameter is frozen and hence you do not
C need to calculate the derivative.  If PLIM(1,I)<-1 then the parameter
C has been set equal to another parameter and you should calculate the
C derivative in the normal manner (the FIT routine assumes that the
C derivative has been correctly calculated).
C
C If you able to compute the analytic derivative of your function with
C respect to the parameter values, then you should use it, as an accurate
C derivative can greatly improve the fitting process.  NOTE, slow
C convergence is most often due to the derivative being incorrectly
C calculated.  If you find that chi^2 drops slowly, and that FIT is
C unable to precisely locate the minimum, then you should carefully
C check both your equations and the UDERIV implementation for typical
C errors, such as an incorrect sign.
C---
C X       I    The current X value
C PVAL    I    The current parameter values
C PLIM    I    The constraints array
C DERIV     O  The calculated derivative
C NT      I    Pointer to first parameter value in array PVAL(*)
C NPAR    I    Number of parameters
C---
      REAL      UFNY
C
      REAL      DX, PTMP, TMP, UFNY0
      INTEGER   I
C---
      UFNY0=UFNY(X,PVAL,NT,NPAR)
      DO 190 I=NT,NT+NPAR-1
         IF(PLIM(1,I).GE.0.) THEN
            PTMP=PVAL(I)
            TMP=ABS(PTMP)
            IF(TMP.GT.0.01 .AND. TMP.LT.100.) THEN
C- For scaled parameters, use a fixed step size.  This is more
C- numerically stable.
               DX=.001
            ELSE IF(TMP.LT.1.E-15) THEN
C- Near zero, avoid divide by zero problems.
               DX=.001
            ELSE
               DX=.001*TMP
            END IF
            PVAL(I)=PVAL(I)+DX
            CALL ULIMIT(PVAL,PLIM,NT,NPAR)
            DERIV(I)=(UFNY(X,PVAL,NT,NPAR)-UFNY0)/DX
            PVAL(I)=PTMP
         END IF
  190 CONTINUE
      RETURN
      END
