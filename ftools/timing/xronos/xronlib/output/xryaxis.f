c
      SUBROUTINE xryaxis (iflags, rflags, nmaxa, nanal, 
     &                     yr, syr, expr)

c
c  iflags    I   I  Integer flags array
c  rflags    I   R  real flags array 
c  nmaxa     I   I  size for array
c  nanal     I   I  number of bin calculated
c  rebin     I   R  rebin flag (0=no, > 0=linear, < 0 =geometric) 
c  yr      I/O   R  input/output results values
c  syr     I/O   R  input/output error on results values
c  expr    I/O   R  input/output expr. 
c
c
c Input variable
        INTEGER*4 iflags(*), nmaxa, nanal 
        REAL*4 rflags(*)
        REAL*4 yr(nmaxa, *), syr(nmaxa, *), expr(nmaxa)
c local variable
        INTEGER*4 k, m 
c
c Rebin results
c
      k = 1
      CALL xryrebin(rflags(1), nanal, k, iflags, nmaxa, yr, syr, expr)
c
c Apply result factor and additive constant 
c specified in xronos.pf if necessary .
c (note that rflags(2)=rpf(3) ; rflags(3)=rpf(4))
c
c Reset gap for QDP and apply constant tfrom parameter file 
c if necessary
c
c  
         DO m = 1, (iflags(8)-1)/2
            DO k = 1, iflags(14)
c apply contstant
               IF (rflags(2).NE.1 .OR. rflags(3).NE.0.) THEN
                   IF (yr(k,m).GT.-1.1E34) THEN
                      yr(k, m) = yr(k, m)*rflags(2) + rflags(3)
c only factor for errors
                      syr(k, m) = syr(k, m)*rflags(2)
                   ENDIF
               ENDIF 
c reset gap 
               IF( yr(k,m).LT.-1.1E34)THEN
                  yr(k, m) = -1.2e-34 
                  syr(k, m)= -1.2e-34 
                  expr(k)= -1.2e-34
               ENDIF
            ENDDO
         ENDDO
      RETURN 
      END
