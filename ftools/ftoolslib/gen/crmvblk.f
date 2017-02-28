*+ CRMVBLK
      	subroutine crmvblk(name)
	
	IMPLICIT NONE
        character*(*) Name
c
c Description:
c  Removes all blanks from a string
c
c Passed Parameters:
c  NAME		i/o: String to be de-blanked
c
c Called Routines:
c  None
c 
c Origin:
c  Direct copy of the XANADU routine RMVBLK (copied 1993 March 31), 
c 
c Authors/Modification History:
c  Linda Osborne 	(1989 Apr 12), original 
c  Nick White		(1992 Jun 08), removed c*400 dummy array
c  Ian M George		(1993 Mar 31), direct copy to caldb library
	character(7) version
	parameter (version = '1.0.0')
*-

c Internals
      INTEGER*4 jj , i , k , FCSTLN , out , newlen
c Main
      jj = FCSTLN(Name)
      i = 1
      out = 0
      DO 100 k = 1 , jj
         IF ( Name(k:k).NE.' ' ) THEN
            Name(i:i) = Name(k:k)
            i = i + 1
         ELSE
            out = out + 1
         ENDIF
 100  CONTINUE
c
      newlen = jj - out
      Name(newlen+1:jj) = ' '

      RETURN
      END
