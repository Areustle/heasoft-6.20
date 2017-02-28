**==GTQUAL.spg  processed by SPAG 3.09I  at 09:43 on 20 Aug 1992
*- gtqual - split up command/qualifiers into command and qualifiers
      SUBROUTINE GTQUAL(Command,Lcomm,Nqual,Qualifier,Lqual,Status)
* Description :
*  splits up command/qualifier into command and qualifier where the qualifier
*  does not include the '/'
* Author :
*  Andy Pollock (EXOSAT::ANDY)
* History :
*  1 February 1989 : original
*  5 March 1990 : discard double quotes from qualifiers
* 25 June 1991 : renamed gtqual
      INCLUDE 'status.codes'
* Import/export :
      CHARACTER*(*) Command
      INTEGER Lcomm
* Export :
      INTEGER Nqual
      CHARACTER*(*) Qualifier(*)
      INTEGER Lqual(*)
* Status :
      INTEGER Status
* Local constants :
      INTEGER MAXQ
      PARAMETER (MAXQ=20)
      character(1) DOUBLE_QUOTES
      PARAMETER (DOUBLE_QUOTES='"')
* Local variables :
      character(256) string
      INTEGER lstr
      INTEGER iq(MAXQ)
      INTEGER i1 , i2
      INTEGER i , j
*-
      IF ( Status.NE.OK__ ) RETURN
 
      i = INDEX(Command(1:Lcomm),'/')
      IF ( i.GT.0 ) THEN
         lstr = Lcomm - i
         IF ( lstr.GT.0 ) THEN
            string = Command(i+1:Lcomm)
            CALL DECOMP(string,lstr,'/',Nqual,iq,Lqual,Status)
            DO 20 j = 1 , Nqual
               IF ( Lqual(j).GT.0 ) THEN
                  i1 = iq(j)
                  i2 = i1 + Lqual(j) - 1
                  IF ( (string(i1:i1).EQ.DOUBLE_QUOTES) .AND. 
     &                 (INDEX(string(i1+1:i2),DOUBLE_QUOTES).NE.0) )
     &                 THEN
                     i = i1 + INDEX(string(i1+1:),DOUBLE_QUOTES)
                     Qualifier(j) = string(i1+1:i-1) // string(i+1:i2)
                  ELSE
                     Qualifier(j) = string(i1:i2)
                  ENDIF
               ENDIF
 20         CONTINUE
         ELSE
            Nqual = 1
            Lqual(1) = 0
         ENDIF
         Command = Command(1:i-1)
         Lcomm = i - 1
      ELSE
         Nqual = 0
      ENDIF
 
      RETURN
 
      END
