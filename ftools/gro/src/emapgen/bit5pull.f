C    FUNCTION: BIT5PULL
C
C     The input of this function is an integer*2 variable that
C  is between 0 and 255.  This function checks to see if the 5th 
C  bit would be high if the input were in binary notation.
C
C
       LOGICAL FUNCTION BIT5PULL(IN_INT)
       INTEGER*2 IN_INT,FACTOR
       FACTOR = IN_INT/32
       BIT5PULL = .FALSE.
       IF ( MOD(INT(FACTOR),2) .EQ. 1) THEN
           BIT5PULL = .TRUE.
       ENDIF
       RETURN
       END
