*+WTARF1
        subroutine wtarf1(ounit, chatter,
     &          nk_hist, hist,
     &          nk_comm, comment,arfversn,phafil,
     &          telescop, instrume, detnam, filter,
     &          maxen, ienerg, energ_lo, energ_hi,
     &          sprsp, ierr)

        IMPLICIT NONE
        integer chatter, ierr,maxen
        integer ounit, nk_hist, nk_comm
        integer ienerg
        real energ_lo(maxen), energ_hi(maxen)
        real sprsp(maxen)
        character*(*) arfversn, phafil
        character*(*) telescop, instrume, detnam, filter
        character*(*) hist(*), comment(*)
c
c Description:
c  Creates and Writes the SPECRESP extension for an ARF file one of the formats
c  conforming to the HDUVERS='1.*.*' family.
c Currently the following formats are supported (see OGIP/92-002a)
c   HDUVERS = '1.1.0'
c The requested format is checked, and if belonging to the '1.*.*' family,
c but not included above, the extension is written in the last format listed.
c  Assumes the FITS file is open and has had the Primary Header written
c  !!! Note !!!! File is left open at the end
c      and  MUST BE CLOSED               by FTCLOS
c      or   ANOTHER EXTENSION ADDED      by FTCRHD
c  in order to (automatically) write the mandatory END header keyword.
c
c Passed parameters
c  OUNIT         i   : FORTRAN unit number of open RMF file
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  NK_HIST       i   : No. records to be written as HISTORY records
c  HIST          i   : Array of history strings to be written
c  NK_COMM       i   : No. records to be written as COMMENT records
c  COMMENT       i   : Array of comment strings to be written
c  ARFVERSN      i   : String denoting OGIP HDUVERS family
c  TELESCOP      i   : String listing telescope/mission
c  INSTRUME      i   : String listing instrument/detector
c  DETNAM        i   : String listing specific detector name
c  FILTER        i   : String listing instrument filter in use
c  MAXEN         i   : Maximum Energy index array dimension
c  IENERG        i   : No. energy bins
c  ENERG_LO      i   : Array containing lower bound to each energy bin
c  ENERG_HI      i   : Array containing upper bound to each energy bin
c  SPRSP         i   : Array containing the specresp dataset
c  IERR            o : Error flag (0 = OK)
c
c Called Routines:
c  subroutine CRMVBLK    : (CALLIB) Removes blanks from a string
c  subroutine FCECHO     : (FTOOLS) Writes to standard o/p device
c  subroutine FTBDEF     : (FITSIO) Defines the BINTABLE data structure
c  subroutine FTCRHD     : (FITSIO) Creates a new FITS extension file
c  subroutine FTPHBN     : (FITSIO) Writes the required header keywords
c  subroutine FTPCOM     : (FITSIO) Writes a FITS comment keyword
c  subroutine FTPCLx     : (FITSIO) Writes the data
c  subroutine FTPHIS     : (FITSIO) Writes a FITS history keyword
c  subroutine FTPKYS     : (FITSIO) Writes a keyword
c  subroutine WT_FERRMSG : (CALLIB) Writes FITSIO error message etc
c
c Compilation & Linking
c  link with FITSIO & CALLIB & FTOOLS
c
c Origin:
c  Code mostly hacked from within the BBRSP program
c
c Authors/Modification History:
c  Ian M George     (1.0.0: 1993 Oct 17), original "proper" version
c  Ian M George     (1.0.1: 1994 Jan 24), cosmetics
c  Jeff Guerber (1.1.0: 1999-04-05), write HDUVERS & HDUDOC; warn of old format
c
        character(7) version
        parameter (version = '1.1.0')
*-

c Internals
        integer status, decimals, itemp
        integer nfields, ie
        parameter (nfields=3, decimals=6)
        character(5) hduvers
        character(16) ttype(nfields), tform(nfields), tunits(nfields)
        character(70) string
        character(80) message
        character(40)  errstr, wrnstr

        integer i
c Initialize
        ierr = 0
        status = 0
        errstr = '** WTARF1 '//version//' ERROR: '
        wrnstr = '** WTARF1 '//version//' WARNING: '


c Give user info if requested
        if(chatter.GE.20) then
             message = ' ... using WTARF1 '// version
             call fcecho(message)
        endif

c Check for sillies
        if(arfversn(1:1).NE.'1') then
           message = wrnstr // ' Format/subroutine mismatch'
           call fcecho(message)
           message =
     &          ' ...... This routine writes only the 1.*.* family' //
     &          ' of formats'
           call fcecho(message)
           message =
     &          ' ...... requested format: '// arfversn
           call fcecho(message)
           ierr = 15
           goto 998
        endif
c Check that we know the format
        if (arfversn .eq. '1.1.0') then
           hduvers = arfversn
        elseif (arfversn .EQ. '1.0.0') then
           hduvers = '1.1.0'
           message = wrnstr// ' Old ARF format requested: ' //arfversn
           call fcecho(message)
           message = ' ...... Resetting format (HDUVERS) to '//hduvers
           call fcecho(message)
        else
           hduvers = '1.1.0'
           message = wrnstr // ' Unknown ARF format: '// arfversn
           call fcecho(message)
           message =
     &        ' ...... Resetting format (HDUVERS) to '//hduvers
           call fcecho(message)
        endif

c Create a new extension
        call FTCRHD(ounit,status)
        message = errstr
        call wt_ferrmsg(status, message)
        if(chatter.GE.20) then
          message = ' ... new extension created'
          call fcecho(message)
        endif
        if(status.NE.0) then
                ierr = 1
                goto 998
        endif

c Set up the columns
        ttype(1)   = 'ENERG_LO'
        tform(1)   = 'E'
        tunits(1)  = 'keV'
        ttype(2)   = 'ENERG_HI'
        tform(2)   = 'E'
        tunits(2)  = 'keV'
        ttype(3)   = 'SPECRESP'
        tform(3)   = 'E'
        tunits(3)  = 'cm**2'

c Write the required header keywords
        call FTPHBN(ounit,ienerg,nfields,ttype,tform,tunits,
     &          'SPECRESP',0,status)
        message = errstr
        call wt_ferrmsg(status, message)
        if(chatter.GE.20) then
          message = ' ... written the extension header keywords'
          call fcecho(message)
        endif
        if(status.NE.0) then
                ierr = 1
                goto 998
        endif

c
c --- WRITE THE HDUCLASn & HDUVERSn keywords
c
        call FTPKYS(ounit,'HDUCLASS',
     &          'OGIP',
     &          'format conforms to OGIP standard',
     &          status)
        message = wrnstr // ' Problem putting HDUCLASS keyword '
        call wt_ferrmsg(status, message)
        status = 0

        call FTPKYS(ounit,'HDUCLAS1 ',
     &          'RESPONSE',
     &          'dataset relates to spectral response',
     &          status)
        message = wrnstr // ' Problem putting HDUCLAS1 keyword '
        call wt_ferrmsg(status, message)
        status = 0

        call FTPKYS(ounit,'HDUCLAS2 ',
     &          'SPECRESP',
     &          'dataset contains spectral response ',
     &          status)
        message = wrnstr // ' Problem putting HDUCLAS2 keyword '
        call wt_ferrmsg(status, message)
        status = 0

        call FTPKYS(ounit,'HDUVERS',
     &          hduvers,
     &          'Version of format (OGIP memo CAL/GEN/92-002a)',
     &          status)
        message = wrnstr // ' Problem putting HDUVERS keyword '
        call wt_ferrmsg(status, message)
        status = 0

        call FTPKYS( ounit, 'HDUDOC',
     &      'OGIP memos CAL/GEN/92-002 & 92-002a',
     &      'Documents describing the format', status)
        message = wrnstr // ' Problem putting HDUDOC keyword '
        call wt_ferrmsg(status, message)
        status = 0

        call FTPKYS(ounit,'HDUVERS1',
     &          '1.0.0',
     &          'Obsolete - included for backwards compatibility',
     &          status)

        call FTPKYS(ounit,'HDUVERS2',
     &          hduvers,
     &          'Obsolete - included for backwards compatibility',
     &          status)
        message = wrnstr // ' Problem putting HDUVERS1/2 keywords'
        call wt_ferrmsg(status, message)
        status = 0

c Add other passed keyword values
        call FTPKYS(ounit,'TELESCOP ',
     &          telescop,
     &          'mission/satellite name',
     &          status)
        message = wrnstr // ' Putting TELESCOP keyword '
        call wt_ferrmsg(status, message)
        status = 0

        call FTPKYS(ounit,'INSTRUME ',
     &          instrume,
     &          'instrument/detector name',
     &          status)
        message = wrnstr // ' Putting INSTRUME keyword '
        call wt_ferrmsg(status, message)
        status = 0

        if(detnam.NE.' ') then
        call FTPKYS(ounit,'DETNAM ',
     &          detnam,
     &          'specific detector name in use',
     &          status)
        message = wrnstr // ' Putting DETNAM keyword '
        call wt_ferrmsg(status, message)
        status = 0
        endif

        call FTPKYS(ounit,'FILTER   ',
     &          filter,
     &          'filter in use',
     &          status)
        message = wrnstr // ' Putting FILTER keyword '
        call wt_ferrmsg(status, message)
        status = 0


c Add other advised keywords
        call FTPKYS(ounit,'ARFVERSN ',
     &          '1992a',
     &          'Obsolete - included for backwards compatibility',
     &          status)
        message = wrnstr // ' Putting ARFVERSN keyword '
        call wt_ferrmsg(status, message)
        status = 0

        if(phafil.EQ.' ') phafil = 'UNKNOWN'
        call FTPKYS(ounit,'PHAFILE',
     &          phafil,
     &          'PHA file for which this ARF created',
     &          status)
        message = wrnstr // ' Putting PHAFILE keyword '
        call wt_ferrmsg(status, message)
        status = 0

        if(chatter.GE.20) then
          message = ' ... written the OGIP required keywords'
          call fcecho(message)
        endif


c Add the (passed) history cards, adding one related to this programme
        itemp = 0
        do i = 1, nk_hist
                call FTPHIS(ounit, hist(i), status)
                if(status.NE.0) then
                        itemp = status
                        status = 0
                        call FTPHIS(ounit,
     &          ' - (missing record) fitsio illegal character ?',
     &           status)
                endif
        enddo
        write(string,'(2a)')
     &                  ' FITS ARF extension written by WTARF1 ',
     &                   version
        call FTPHIS(ounit,string,status)
        message = wrnstr // ' Putting at least one History record'
        call wt_ferrmsg(itemp, message)
        if(chatter.GE.20) then
          message = ' ... written the history keywords'
          call fcecho(message)
        endif
        status = 0

c Add the (passed) comment cards
        itemp = 0
        do i = 1, nk_comm
                call FTPCOM(ounit, comment(i), status)
                if(status.NE.0) then
                        itemp = status
                        status = 0
                        call FTPCOM(ounit,
     &          ' - (missing record) fitsio illegal character ?',
     &           status)
                endif
        enddo
        message = wrnstr // ' Putting at least one Comment record'
        call wt_ferrmsg(itemp, message)
        status = 0
        if(chatter.GE.20) then
          message = ' ... written the comment header keywords'
          call fcecho(message)
        endif

c Define the extension data structure
        call FTBDEF(ounit,nfields,tform,0,ienerg,status)
        message = errstr // ' Defining Data Structure '
        call wt_ferrmsg(status, message)
        if(status.NE.0) then
                ierr = 1
                goto 998
        endif
        if(chatter.GE.20) then
          message = ' ... defined the extension data structure'
          call fcecho(message)
        endif

c Write the data
        do ie = 1, ienerg

c               ... the energy bin
                call FTPCLE(ounit, 1, ie, 1, 1, energ_lo(ie),status)
                call FTPCLE(ounit, 2, ie, 1, 1, energ_hi(ie),status)

c               ... the spectral response
                call FTPCLE(ounit, 3, ie, 1, 1, sprsp(ie),status)
        enddo

        if(chatter.GE.20) then
          message = ' ... written the data'
          call fcecho(message)
        endif

c Final check for errors
        message = wrnstr // ' Writing Data '
        call wt_ferrmsg(status, message)

998     if(ierr.NE.0) then
           message = errstr // ' FATAL: Extension not written'
           call fcecho(message)
        endif

        return
        end
