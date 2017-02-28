**==QLF0L.spg  processed by SPAG 3.09I  at 09:47 on 20 Aug 1992
*- qlf0l - get named logical*4 from qualifier
      SUBROUTINE QLF0L(String,Parameter,Value,Status)
* Description :
*  looks for a logical value in a qualifier and sets it by default to .true.
* Author :
*  Andy Pollock (EXOSAT::ANDY)
* History :
*  16 March 1989 : original
*  25 June 1991 : renamed
      INCLUDE 'status.codes'
* Import :
      CHARACTER*(*) String
c  string containing parameter values
      CHARACTER*(*) Parameter
c  parameter name
* Export :
      LOGICAL Value
* Status :
      INTEGER Status
* Local constants :
      INTEGER MAXPAR
c  maximum no of string parameters
      PARAMETER (MAXPAR=25)
* Local variables :
      character(80) s
c  copy of string
      INTEGER ls
c  non-blank length of string
      character(80) p
c  copy of parameter
      INTEGER lp
c  non-blank length of parameter
      INTEGER nf
c  no of string fields
      INTEGER field_start(MAXPAR)
c  field starting character
      INTEGER field_length(MAXPAR) , lf
c  field length
      INTEGER jf
c  field index
      INTEGER k
c  no of field/parameter coincidences
      character(80) name
c  parameter name in string
      INTEGER ln
c  non-blank length of name
      character(80) v
c  value field
      INTEGER lv
c  non-blank length of v
      INTEGER j , j1 , j2 , l
* External reference :
      INTEGER LENACT
*-
      IF ( Status.NE.OK__ ) RETURN
 
      s = String
      ls = LENACT(String)
      CALL LOCASE(s)
      CALL DECOMP(s,ls,'/',nf,field_start,field_length,Status)
      p = Parameter
      lp = LENACT(p)
      CALL LOCASE(p)
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
                  CALL PAR0L(v,lv,-1,' ',Value,Status)
               ELSE
                  Value = .TRUE.
               ENDIF
            ENDIF
         ENDIF
 100  CONTINUE
 
      RETURN
 
      END
