**==PAR0C.spg  processed by SPAG 3.09I  at 09:45 on 20 Aug 1992
*- par0c - get character string from string
      SUBROUTINE PAR0C(String,Length,Position,Parameter,Value,Status)
* Author :
*  Andy Pollock (EXOSAT::ANDY)
* History :
*  21 March 1988 : original
*  15 November 1988 : don't prompt if position<0
*  29 May 1992: moved from XANADU:[lib.estec] and replaced references to
*               PAR_DECOMPOSE with DECOMP and PAR_PROMPT with PRMPT.
 
      INCLUDE 'status.codes'
      INCLUDE 'par.constants'
* Import :
      CHARACTER*(*) String
      INTEGER Length
      INTEGER Position
      CHARACTER*(*) Parameter
* Export :
      CHARACTER*(*) Value
* Status :
      INTEGER Status
* Local variables :
      INTEGER nf
      INTEGER field_start(PAR_MAXF)
      INTEGER field_length(PAR_MAXF)
      INTEGER i , j , k
*-
      IF ( Status.NE.OK__ ) RETURN
 
      CALL DECOMP(String,Length,',',nf,field_start,field_length,Status)
      k = ABS(Position)
      IF ( k.LE.nf ) THEN
         j = field_length(k)
         IF ( j.GT.0 ) THEN
            i = field_start(k)
            Value = String(i:i+j-1)
         ELSE
            Status = ERROR__
         ENDIF
      ELSE
         Status = EOF__
      ENDIF
      IF ( (Status.NE.OK__) .AND. (Position.GT.0) ) THEN
         Status = OK__
         CALL PRMPT(Parameter,Status)
         READ (*,'(a)',IOSTAT=Status) Value
      ENDIF
 
      RETURN
 
      END
