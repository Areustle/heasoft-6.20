**==GETUDR.spg  processed by SPAG 3.09I  at 09:43 on 20 Aug 1992
      SUBROUTINE GETUDR(Rootname)
 
      CHARACTER*(*) Rootname
      CHARACTER(255) Rootname_alt
 
c get the user's directory name where system files are to be stored
 
      CALL GETROT(Rootname)

c Allow for an alternate directory to be specified via the
c XSEL_HISTORY_DIR environment variable:
      Rootname_alt = ''
      CALL getenv ("XSEL_HISTORY_DIR",Rootname_alt)
      if (Rootname_alt .ne. '') Rootname = 
     &       Rootname_alt(:lenact(Rootname_alt))//'/'
 
      RETURN
      END
