**==DFAULT.spg  processed by SPAG 3.09I  at 09:42 on 20 Aug 1992
      SUBROUTINE DFAULT(N,Flag,String,Ios)
c
c        get (flag=1) or set (flag=0) defaults
c        n = parameter number to retrieve or store
c        defaults file contains :
c        parameter no.  1 -> database name
c        parameter no.  2 -> sample file name
c        parameter no.  3 -> sort file name
c        parameter no.  4 -> right ascension (1950 independently of line 9)
c        parameter no.  5 -> declination     (1950 independently of line 9)
c        parameter no.  6 -> search radius in arcmin
c        parameter no.  7 -> plot device
c        parameter no.  8 -> source name
c        parameter no.  9 -> equinox year used to precess coordinates
c        parameter is returned in a string of characters
c        parameter no. 10 -> sequence number
c        parameter no. 15 -> default file for user-defined parameters
c        parameter no. 16 -> x pixels
c        parameter no. 17 -> y pixels
c        parameter no. 18 -> previous def ra
c        parameter no. 19 -> previous def dec
c        parameter no. 20 -> previous radius
c        parameter no. 21 -> ra entered at sc
c        parameter no. 22 -> dec entered at sc
c        parameter no. 23 -> spare
c        parameter no. 24 -> def inner radius
c        parameter no. 25 -> prev def inner radius
c        parameter no. 26 -39 -> spare
c	 parameter no. 40-50-> pe command
c        parameter no. 51   -> default ascii table for database 1
c        parameter no. 50+n -> default ascii table for database n
c        parameter no. 200  -> reserved for internal use
c
      INCLUDE 'estec.inc'
      character(40) String
      INTEGER*4 Flag , N , Ios
c
      Ios = 0
c
c  read it
c
      IF ( Flag.EQ.1 ) THEN
         READ (Zlun_def,'(a)',REC=N,IOSTAT=Ios) String
         CALL UNLOCK(Zlun_def)
         IF ( Ios.NE.0 ) THEN
            WRITE (Zwrite,99001) Ios
            CALL XWRITE(Zwrite,5)
            RETURN
         ENDIF
c
c  write it
c
      ELSEIF ( Flag.EQ.0 ) THEN
         WRITE (Zlun_def,'(a)',REC=N,IOSTAT=Ios) String
         CALL UNLOCK(Zlun_def)
         IF ( Ios.NE.0 ) THEN
            WRITE (Zwrite,99002) Ios
            CALL XWRITE(Zwrite,5)
            RETURN
         ENDIF
      ELSE
c
c error
c
         WRITE (Zwrite,99003) Flag
         CALL XWRITE(Zwrite,5)
      ENDIF
c
      RETURN
99001 FORMAT (' Error reading defaults file no ',i4)
99002 FORMAT (' Error writing defaults file no ',i4)
99003 FORMAT (' Error: invalid defaults flag no ',i10)
      END
