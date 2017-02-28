**==PAR0L.spg  processed by SPAG 3.09I  at 09:45 on 20 Aug 1992
*- par0l - get logical value from string
      SUBROUTINE PAR0L(String,Length,Position,Parameter,Value,Status)
* Author :
*  Andy Pollock (EXOSAT::ANDY)
* History :
*  21 March 1988 : original
*  29 May 1992: moved from XANADU:[lib.estec] and replaced references to
*               PAR_DECOMPOSE with DECOMP and PAR_GET0L with GET0L.
 
* Global constants :
      INCLUDE 'status.codes'
      INCLUDE 'par.constants'
* Import :
      CHARACTER*(*) String
      INTEGER Length
      INTEGER Position
      CHARACTER*(*) Parameter
* Export :
      LOGICAL Value
* Status :
      INTEGER Status
* Local variables :
      INTEGER nf
      INTEGER field_start(PAR_MAXF)
      INTEGER field_length(PAR_MAXF)
      character(80) answer
      INTEGER i , j , k
*-
      IF ( Status.NE.OK__ ) RETURN
 
      CALL DECOMP(String,Length,',',nf,field_start,field_length,Status)
      IF ( Position.GT.0 ) THEN
         k = Position
      ELSE
         k = -Position
      ENDIF
      IF ( k.GT.0 ) THEN
         j = field_length(k)
      ELSE
         j = 0
      ENDIF
      IF ( j.GT.0 ) THEN
         i = field_start(k)
         answer = String(i:i+j-1)
         CALL LOCASE(answer)
         IF ( (answer(1:j).EQ.'y') .OR. (answer(1:j).EQ.'ye') .OR. 
     &        (answer(1:j).EQ.'yes') .OR. (answer(1:j).EQ.'t') .OR. 
     &        (answer(1:j).EQ.'tr') .OR. (answer(1:j).EQ.'tru') .OR. 
     &        (answer(1:j).EQ.'true') .OR. (answer(1:j).EQ.'.t') .OR. 
     &        (answer(1:j).EQ.'.tr') .OR. (answer(1:j).EQ.'.tru') .OR. 
     &        (answer(1:j).EQ.'.true') .OR. (answer(1:j).EQ.'.true.') )
     &        THEN
            Value = .TRUE.
         ELSEIF ( (answer(1:j).EQ.'n') .OR. (answer(1:j).EQ.'no') .OR. 
     &            (answer(1:j).EQ.'f') .OR. (answer(1:j).EQ.'fa') .OR. 
     &            (answer(1:j).EQ.'fal') .OR. (answer(1:j).EQ.'fals')
     &            .OR. (answer(1:j).EQ.'false') .OR. 
     &            (answer(1:j).EQ.'.f') .OR. (answer(1:j).EQ.'.fa') .OR. 
     &            (answer(1:j).EQ.'.fal') .OR. (answer(1:j).EQ.'.fals')
     &            .OR. (answer(1:j).EQ.'.false') .OR. 
     &            (answer(1:j).EQ.'.false.') ) THEN
            Value = .FALSE.
         ELSE
            Status = ERROR__
         ENDIF
      ELSE
         Status = ERROR__
      ENDIF
      IF ( (Status.NE.OK__) .AND. (Position.GT.0) ) THEN
         Status = OK__
         CALL GET0L(Parameter,Value,Status)
      ENDIF
 
      RETURN
 
      END
