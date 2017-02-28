*+OPNPA
        subroutine opnpa(filnam,chatter,ounit,killit,ierrstat)

        IMPLICIT NONE
        integer chatter
        integer ounit, ierrstat
        character*(*) filnam
        logical killit
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
c  subroutine FCSTLN     : (FTOOLS) Get length of string
c  subroutine OPFITS     : (CALLIB) Creates a new FITS file
c  subroutine FTPDAT     : (FITSIO) Writes (current system) DATE keyword
c  subroutine FTPDEF     : (FITSIO) Defines structure of FITS P.Array
c  subroutine FTPHPR     : (FITSIO) Writes mandatory P.Array keywords
c  subroutine WTINFO     : (FTOOLS) Writes message to STDOUT
c  subroutine WTERRM	 : (FTOOLS) Writes internal error message
c  subroutine WTFERR	 : (FTOOLS) Writes internal + fitsio error message
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
c  Rehana Yusaf (1.0.4:94 Sept 13), call opfits instead of ftinit
c                                   additional argument,killit passed to
c                                   this routine
c                                   RENAMED from op_npa to opnpa
c  Ian M George     (1.0.5:95 Dec 19) added wtinfo & friends
c  Jeff Guerber (1.0.6: 1999-02-17)  Made filnam arg assumed-length.
c
        character(7) version
        parameter (version = '1.0.6')
*-
c Internals
        character(5) subname
        parameter (subname = 'opnpa')
        integer status, bitpix, naxis, naxes(2), pcount, gcount
        logical simple, extend
        character(80) message
c Initialize
	status = 0
	ierrstat = 0

c Give user info if requested
         message = ' using '//subname//' '//version
         call wtinfo(chatter,20,1,message)

c Get a new FORTRAN unit & create the FITS file
        call cgetlun(ounit)
	call crmvlbk(filnam)
        call opfits(ounit, filnam,killit,chatter,status)
        if(status.NE.0) then
		call wtferr(subname, version, status,
     &		  'problem opening new file')
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
                call wtferr(subname, version, status,
     &        'problem writing mandatory p.header keywords (FTPHPR)')
		goto 789
        endif

c Write Additional Primary Header Keywords
        call FTPDAT(ounit,status)
        if(status.NE.0) then
                call wtferr(subname, version, status,
     &        ' with fitsio FTPDAT call')
		goto 789
        endif

c Define the structure of the Primary Array
        call FTPDEF(ounit,bitpix,naxis,naxes,pcount,gcount,status)
        if(status.NE.0) then
                call wtferr(subname, version, status,
     &        ' with fitsio FTPDEF call')
		goto 789
        endif

c Fix the the error flag
789	ierrstat = status
	if(ierrstat.ne.0) then
          call wterrm(subname, version, ' aborting')
        endif

        return
        end
