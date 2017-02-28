*+ CMA_DETNAM
      	SUBROUTINE cma_detnam(chatter,instring, instrume, filter, ierr)

	IMPLICIT NONE
	integer chatter, ierr
	character(20) instring, instrume, filter
c
c Description
c   Works out the official OGIP Detnam string for the EXOSAT ME 
c
c Passed Parameters
c  CHATTER	i   : Chatter flag (<5=quite,10=normal,>20=silly)
c  INSTRING     i   : I/p character string
c  INSTRUME       o : O/p (decoupled) instrume string
c  FILTER         o : O/p (decoupled) filter string
c  IERR           o : Return error flag (0 = OK)
c
c Called Routines
c  subroutine FCECHO            : (FTOOLS) writes to standard o/p
c
c Origin
c   IMG special
c
c Authors/Modification History
c  Ian M George    (1.0.0: 1993 Oct 11), original
	character(7) version
	parameter (version = ' 1.0.0 ')
*-
c Internals
	integer fcstln, i, lc
	character(30) errstr,wrnstr
	character(70) subinfo
c Initialize
	ierr = 0
        errstr = '** CMA_DETNAM ERROR :'
        wrnstr = '** CMA_DETNAM WARNING :'

c Stick in a message for the really keen user
      IF (chatter.GE.20) THEN
         subinfo = ' ... using CMA_DETNAM Ver '//version
         call fcecho(subinfo)
         subinfo = 
     &		' ...... trying to decouple INSTRUME & FILTER strings'
         call fcecho(subinfo)
      ENDIF


c - Break up the SF-supplied instrument name into 2 halves, in an
c   attempt to decouple the detector from the filter
        call crmvlbk(instring)
        lc = fcstln(instring)
        if(lc.GT.0) then
          do i = 1, lc
                if(instring(i:i).EQ.' ') then
                        lc = i-1
                        goto 765
                endif
          enddo
765       instrume = instring(1:lc)
          filter = instring(lc+1:)
          call crmvlbk(filter)
        else
                instrume = ' '
		filter = ' '
        endif


998     if(ierr .NE.0) then
          subinfo = wrnstr // 'BUM BUM string will be set to NONE'
          call fcecho(subinfo)
	  filter = 'NONE'
        endif

      RETURN
      END
