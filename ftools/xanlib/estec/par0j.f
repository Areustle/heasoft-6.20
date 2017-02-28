**==PAR0J.spg  processed by SPAG 3.09I  at 09:45 on 20 Aug 1992
*- par0j - get integer*2 from string
      SUBROUTINE PAR0J(String,Length,Position,Parameter,Value,Status)
* Author :
*  Andy Pollock (EXOSAT::ANDY)
* History :
*  21 March 1988 : original
*  16 June 1988 : if position < 0 don't prompt
*  29 May 1992: moved from XANADU:[lib.estec] and replaced references to
*               PAR_DECOMPOSE with DECOMP and PAR_PROMPT with PRMPT.
 
* Global constants :
      INCLUDE 'status.codes'
      INCLUDE 'par.constants'
* Import :
      CHARACTER*(*) String
      INTEGER Length
      INTEGER Position
      CHARACTER*(*) Parameter
* Export :
      INTEGER*2 Value
* Status :
      INTEGER Status
* Local variables :
      INTEGER nf
      INTEGER field_start(PAR_MAXF)
      INTEGER field_length(PAR_MAXF)
      INTEGER i
*-
      IF ( Status.NE.OK__ ) RETURN
 
      CALL DECOMP(String,Length,',',nf,field_start,field_length,Status)
      i = ABS(Position)
      IF ( field_length(i).GT.0 ) THEN
         READ (String(field_start(i):),*,IOSTAT=Status) Value
      ELSE
         Status = ERROR__
      ENDIF
      IF ( (Status.NE.OK__) .AND. (Position.GT.0) ) THEN
         Status = OK__
         CALL PRMPT(Parameter,Status)
         READ (*,*,IOSTAT=Status) Value
      ENDIF
 
      RETURN
 
      END
