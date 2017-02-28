**==GET0L.spg  processed by SPAG 3.09I  at 09:42 on 20 Aug 1992
      SUBROUTINE GET0L(Parameter,Value,Status)
      INCLUDE 'status.codes'
      CHARACTER*(*) Parameter
      LOGICAL Value
      LOGICAL answered
      character(80) answer
      character(15) string
      INTEGER Status
* External reference :
      INTEGER LENACT
 
      IF ( Status.NE.OK__ ) RETURN
 
      CALL PRMPT(Parameter,Status)
      answered = .FALSE.
      DO WHILE ( .NOT.answered )
         READ (*,'(a)',IOSTAT=Status) answer
         CALL UPC(answer)
         IF ( answer(1:1).EQ.'.' ) answer = answer(2:)
         IF ( (answer.EQ.'Y') .OR. (answer.EQ.'YE') .OR. 
     &        (answer.EQ.'YES') .OR. (answer.EQ.'T') .OR. 
     &        (answer.EQ.'TR') .OR. (answer.EQ.'TRU') .OR. 
     &        (answer.EQ.'TRUE') ) THEN
            answered = .TRUE.
            Value = .TRUE.
         ELSEIF ( (answer.EQ.'N') .OR. (answer.EQ.'NO') .OR. 
     &            (answer.EQ.'F') .OR. (answer.EQ.'FA') .OR. 
     &            (answer.EQ.'FAL') .OR. (answer.EQ.'FALS') .OR. 
     &            (answer.EQ.'FALSE') ) THEN
            answered = .TRUE.
            Value = .FALSE.
         ELSE
            string = 'yes or no ? >'
            CALL PROMPT(string,LENACT(string))
            Status = OK__
         ENDIF
      ENDDO
      RETURN
      END
