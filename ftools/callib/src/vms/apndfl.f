*+APNDFL
        SUBROUTINE APNDFL(DIR, FILE)

        CHARACTER*(*) DIR, FILE

C***********************************************************
C Description: Concatenates DIR and FILE interposing a '/'
C              in the unix version.  Ex. DIR='$DISK4:[ZELLAR],
C              and FILE='TEST.TXT' --> $DISK4:[ZELLAR]TEST.TXT.
C              Warning: the concatenated output string is
C                       written to FILE!
C
C Arguments:   DIR     (i) :  The directory which will precede
C                             FILE
C              FILE    (i/o): The file which will follow DIR
C
C Origin:       Written for the Calibration Database
C
C Author/Modification History:
C              Ron Zellar (1993 Mar 23), original version
C
C************************************************************
*-Version 1.0

         Integer len, fcstln, fillen, dirlen, I

C        Get actual length of FILE, and DIR
         fillen = fcstln(FILE)
         dirlen = fcstln(DIR)

C        Check to see that full path name will not overrun FILE
         If((fcstln(DIR)+fillen).gt.(len(FILE)))Then
              Call fcecho('APNDFL: FILE buffer insufficient')
              STOP
         Endif

C        Move the file name down so DIR fits
         Do 100 I=0, fillen-1
              FILE((dirlen+fillen)-I:(dirlen+fillen)-I) =
     &             FILE(fillen-I:fillen-I)
100      Continue

C        Append the file name
         FILE(:dirlen)=DIR(:dirlen)
         Return
         END
