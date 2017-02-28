*+OP_NPA
        subroutine op_npa(filnam, chatter, ounit, ierrstat)

        IMPLICIT NONE
        integer chatter
        integer ounit, ierrstat
        character*(*) filnam
c
c Description:
c  Opens a new (bitpix=-32,blocksize=2880) FITS file & writes a
c  primary header & null primary array.
c  (system) DATE keyword is also written into the P.header.
c
c  !!! Note !!! File is left open on return
c      and  MUST BE CLOSED              by FTCLOS
c      or   ANOTHER EXTENSION ADDED     by FTCRHD
c  in order to (automatically) write the mandatory END header keyword
c
c Passed parameters
c  FILNAM        i   : full name (incl extension) of the FITS file to be
C                      opened
c  CHATTER       i   : chattiness flag for o/p (5 quiet,10 normal,>20
C                      silly)
c  OUNIT           o : FORTRAN unit number of opened file
c  IERRSTAT        o : Error flag (.NE.0 on error)
c
c User i/ps required (prompted for):
c  None
c
c Include files
c  None
c
c Called Routines:
c  subroutine CGETLUN    : (CALLIB) Gets free i/o unit number
c  subroutine FCECHO     : (FTOOLS) Write to standard i/o
c  subroutine FCERR      : (FTOOLS) Write to standard error
c  subroutine FFINIT     : (FTOOLS) Creates a new FITS file (FTOOLSLIB version)
c  subroutine FTGERR     : (FITSIO) Returns Error text
c  subroutine FTPDAT     : (FITSIO) Writes (current system) DATE keyword
c  subroutine FTPDEF     : (FITSIO) Defines structure of FITS P.Array
c  subroutine FTPHPR     : (FITSIO) Writes mandatory P.Array keywords
c  subroutine WT_FERRMSG : (CALLIB) write standard fitsio message
c
c Compilation & Linking
c  link with XANLIB & FITSIO & FTOOLS
c
c Origin:
c  Code mostly hacked from within the BBRSP program
c
c Authors/Modification History:
c  Alan Smale       (1992 Sept/Oct), original BBRSP version
c  Ian M George     (1.0.1:1992 Dec 28), tidied-up version
C  Ron Zellar and Rehana Yusaf (1.0.2:1993 Jul 16), Changed write(5,*) to fcecho
c  Ian M George     (1.0.3: 93 Jul 20), better error handling
c  Lawrence E Brown     (1.0.4: 94 Aug 16), changed ftinit to ffinit to
c                        get "clobber" capability
c  Jeff Guerber     (1.0.5: 1999-02-17)  Made filnam arg assumed-length.
c
        character(7) version
        parameter (version = '1.0.5')
*-
c Internals
        integer status, bitpix, naxis, naxes(2), pcount, gcount
        logical simple, extend
        character(80) message
        character(40)  errstr, wrnstr
c Initialize
	status = 0
	ierrstat = 0
        errstr = '** OP_NPA '//version// ' ERROR: '
        wrnstr = '** OP_NPA '//version// ' WARNING: '


c Give user info if requested
        if(chatter.GE.20) then
                call fcecho(' ... using OP_NPA '//version)
        endif


c Get a new FORTRAN unit & create the FITS file
c Note: FFINIT refers to the one in ftoolslib/gen/, not cfitsio's.

        call cgetlun(ounit)
	call crmvlbk(filnam)
        call FFINIT(ounit, filnam,status)
        if(status.NE.0) then
		message = errstr // ' Calling FFINIT'
       		call wt_ferrmsg(status,message)
		if(status.EQ.105 .AND. chatter.GE.10) then
		  message = ' ... O/p file already exists (?)'
		  call fcecho(message)
		endif
		goto 789
        endif

c Define Primary Header keywords
        simple   = .true.
        bitpix   = -32
        naxis    = 0
        naxes(1) = 0
        naxes(2) = 0
        pcount   = 0
        gcount   = 1
        extend   = .TRUE.

c Write Mandatory Primary Array Keywords
        call FTPHPR(ounit,simple,bitpix,naxis,naxes,pcount,gcount,
     &          extend,status)
        if(status.NE.0) then
		message = errstr // ' Calling FTPHPR'
       		call wt_ferrmsg(status, message)
		goto 789
        endif

c Write Additional Primary Header Keywords
        call FTPDAT(ounit,status)
        if(status.NE.0) then
		message = errstr // ' Calling FTPDAT'
       		call wt_ferrmsg(status, message)
		goto 789
        endif

c Define the structure of the Primary Array
        call FTPDEF(ounit,bitpix,naxis,naxes,pcount,gcount,status)
        if(status.NE.0) then
		message = errstr // ' Calling FTPDEF'
       		call wt_ferrmsg(status, message)
		goto 789
        endif

c Fix the the error flag
789	ierrstat = status
	if(ierrstat.NE.0) call fcerr(errstr)

        return
        end
