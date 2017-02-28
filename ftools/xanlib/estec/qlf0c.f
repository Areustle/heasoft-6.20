**==QLF0C.spg  processed by SPAG 3.09I  at 09:46 on 20 Aug 1992
*- qlf0c - get named character string from qualifier
      SUBROUTINE QLF0C(String,Parameter,Value,Status)
* Author :
*  Andy Pollock (EXOSAT::ANDY)
* History :
*  26 April 1989 : original
 
      INCLUDE 'status.codes'
* Import :
      CHARACTER*(*) String
      CHARACTER*(*) Parameter
* Export :
      CHARACTER*(*) Value
* Status :
      INTEGER Status
* Local constants :
      INTEGER MAXPAR
      PARAMETER (MAXPAR=25)
* Local variables :
      character(80) s
      INTEGER ls
      character(80) p
      INTEGER lp
      INTEGER nf
      INTEGER field_start(MAXPAR)
      INTEGER field_length(MAXPAR) , lf
      INTEGER jf
      INTEGER k
      character(80) name
      INTEGER ln
      character(80) v
      INTEGER lv
      INTEGER j , j1 , j2 , l
* External reference :
      INTEGER LENACT
*-
      IF ( Status.NE.OK__ ) RETURN
 
      s = String
      CALL LOCASE(s)
      ls = LENACT(s)
      CALL DECOMP(s,ls,'/',nf,field_start,field_length,Status)
      p = Parameter
      CALL LOCASE(p)
      lp = LENACT(p)
      k = 0
      DO 100 jf = 1 , nf
         lf = field_length(jf)
         IF ( lf.GT.0 ) THEN
            j1 = field_start(jf)
            j2 = j1 + lf - 1
            j = INDEX(s(j1:j2),'=')
            IF ( j.EQ.0 ) THEN
               name = s(j1:j2)
               ln = lf
               lv = 0
            ELSEIF ( j.EQ.lf ) THEN
               name = s(j1:j2-1)
               ln = lf - 1
               lv = 0
            ELSE
               name = s(j1:j1+j-2)
               ln = j - 1
               v = s(j1+j:j2)
               lv = lf - j
            ENDIF
            l = MIN(ls,lp)
            IF ( name(:l).EQ.p(:l) ) THEN
               k = k + 1
               IF ( k.NE.1 ) THEN
                  Status = WARNING__
               ELSEIF ( lv.GT.0 ) THEN
                  Value = v
               ELSE
                  CALL GET0C(Parameter(1:lp),Value,Status)
               ENDIF
            ENDIF
         ENDIF
 100  CONTINUE
 
      IF ( k.EQ.0 ) Status = EOF__
 
      RETURN
 
      END
