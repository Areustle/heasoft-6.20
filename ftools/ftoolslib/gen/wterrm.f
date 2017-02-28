*+WTERRM
        subroutine wterrm(subrout, version, string)

        IMPLICIT NONE
        character*(*) subrout, version, string
c 
c Description:
c  Writes callib/roslib standard error message string(s) to STDOUT.
c  Current format is approximately:
c       ' ERROR - '//subrout//version//string
c  but with a few extra bits of punctuation, plus some attempt to 
c  handle strings greater than 80 characters.
c
c Passed parameters
c  SUBROUT       i   : (char) Name of the subroutine from which wterr called
c  VERSION       i   : (char) Version of SUBROUT
c  STRING        i   : (char) Context string to be appended to standard msg
c
c Called Routines:
c  function CLENACT      : (CALLIB) Returns actual length of string
c  subroutine CRMVLBK    : (CALLIB) Removes leading blanks from a string
c  subroutine FCECHO     : (FTOOLS) Writes to standard o/p device
c
c Compilation & Linking
c  link with FITSIO & CALLIB & FTOOLS
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1.0.0: 1995 Nov 29) original
c  Keith Arnaud     (1.1.0: 1996 Aug 21) added tsubrout, tversion, tstring
c                                        internal variables to prevent probs
c  					 under Solaris 2.2 when i/p string 
c    					 starts with leading spaces 
c       character(7) version
c       parameter (version = '1.1.0')
*- 
c Internals 
        integer sublen, verlen, strlen
        integer istart, istop, str1len
        integer clenact
        character(80) outstr
        character(255) tsubrout, tversion, tstring

C Copy the passed parameters into temporary strings to avoid any problems
C with modifying constant strings

        tsubrout = subrout(:MIN(len(tsubrout),len(subrout)))
        tversion = version(:MIN(len(tversion),len(version)))
        tstring  = string(:MIN(len(tstring),len(string)))

c Initialize
        sublen = 0
        verlen = 0
        strlen = 0      
        istart = 1
        istop = 0
        str1len = 0

c Remove all leading blanks from i/p strings
        call crmvlbk(tsubrout)
        call crmvlbk(tversion)
        call crmvlbk(tstring)

c Check out the size of each character string
        sublen = clenact(tsubrout)
        verlen = clenact(tversion)
        strlen = clenact(tstring)

c Work out the 1st bit of the first line (up to where string will begin)        
        outstr = ' ERROR - '//tsubrout(:sublen)//
     &          ' '//tversion(:verlen)//': '
        str1len = clenact(outstr) + 1

c Dump the first line
        istop = MIN(strlen,80 - str1len)
        outstr = outstr(:str1len)//tstring(1:istop)
        call fcecho(outstr)

c Return if we've finished
123     if(istop.GE.strlen) then
                return
        else
                istart = MAX(1,istop+1)
                istop = MIN(strlen,istop + 80 - 9)
                outstr = '         '//tstring(istart:istop)
                call fcecho(outstr)
                go to 123
        endif

        end
c -------------------------------------------------------------------



