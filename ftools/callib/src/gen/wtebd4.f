*+WTEBD4
        subroutine wtebd4(ounit, chatter, extname, extcomm,
     &          nk_hist, hist,
     &          nk_comm, comment,rmfversn,
     &          telescop, instrume, detnam, filter, areascal,
     &          chantype, fchan, iebound, e_min, e_max, ierr)

        IMPLICIT NONE
        integer chatter, ierr
        integer ounit, nk_hist, nk_comm
        integer iebound, fchan
        real areascal
        real e_min(*), e_max(*)
        character*(*) rmfversn, chantype
        character*(*) telescop, instrume, detnam, filter
        character*(*) hist(*), comment(*), extname,extcomm
c
c Description:
c  Creates and Writes the EBOUNDS extension for an RMF file one of the formats
c  conforming to the HDUVERS='1.*.*' family.
c Currently the following formats are supported (see OGIP/92-002a):
c   HDUVERS = '1.3.0'
c   HDUVERS = '1.0.0', '1.1.0' & '1.2.0' will be overridden such that '1.3.0' is
c   written.
c Assumes the FITS is open and has had the Primary Header written
c  !!! Note !!!! File is left open at the end.
c      and  MUST BE CLOSED               by FTCLOS
c      or   ANOTHER EXTENSION ADDED      by FTCRHD
c  in order to (automatically) write the mandatory END header keyword.
c This routine differs from wtebd1.f only in so far that the FCHAN parameter
c  is passed which specifies the channel number of the first PHA channel
c  The value of this parameter therefore is usually either 0 or 1
c
c Passed parameters
c  OUNIT         i   : FORTRAN unit number of open RMF file
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  NK_HIST       i   : No. records to be written as HISTORY records
c  HIST          i   : Array of history strings to be written
c  NK_COMM       i   : No. records to be written as COMMENT records
c  COMMENT       i   : Array of comment strings to be written
c  RMFVERSN      i   : String denoting OGIP HDUVERS family
c  TELESCOP      i   : String listing telescope/mission
c  INSTRUME      i   : String listing instrument/detector
c  DETNAM        i   : String listing specific detector name
c  FILTER        i   : String listing instrument filter in use
c  AREA          i   : Area scaling factor
c  FCHAN         i   : No. ("name") of first channel (usually 0 or 1)
c  CHANTYPE      i   : Type of detector channels in use (PHA, PI)
c  IEBOUND       i   : No. channels in the full array
c  E_MIN         i   : Array containing min nominal energy bound to each chan
c  E_MAX         i   : Array containing max nominal energy bound to each chan
c  IERR            o : Error Flag (0=OK)
c
c User i/ps required (prompted for):
c  None
c
c Include files
c  None
c
c Called Routines:
c  subroutine FCECHO     : (FTOOLS) writes to standard o/p unit
c  subroutine FTBDEF     : (FITSIO) Defines the BINTABLE data structure
c  subroutine FTCRHD     : (FITSIO) Creates a new FITS extension file
c  subroutine FTPHBN     : (FITSIO) Writes the required header keywords
c  subroutine FTPCOM     : (FITSIO) Writes a FITS comment keyword
c  subroutine FTPCLx     : (FITSIO) Writes the data
c  subroutine FTPHIS     : (FITSIO) Writes a FITS history keyword
c  subroutine FTPKYS     : (FITSIO) Writes a keyword
c  subroutine WT_FERRMSG : (CALLIB) Dumps FITSIO Error message etc
c
c Compilation & Linking
c  link with FITSIO & CALLIB & FTOOLS
c
c Origin:
c  Code mostly hacked from wtebd1.f, which itself was hacked from
c  within Alan Smale's BBRSP program
c
c Authors/Modification History:
c  Ian M George     (1.0.0; 95 Jun 21), copied from wtebd2.f
c  Jeff Guerber (1.1.0; 1999-04-02)  Replaced HDUVERS2 w/HDUVERS.  Clarified
c     the invalid-format msgs & made it a warning, to not panic the users.
c     Fixed a few other minor bugs.
c  Ning Gan (1.1.1; 2000-06-04) Modified to accept the HDUVERS = 
c               1.3.0  

        character(7) version
        parameter (version = '1.1.1')
*-
c Internals
        character(6) subname
        parameter (subname = 'wtebd3')
        integer status, decimals, itemp
        integer npar, nfields
        integer ie, i, ichan
        parameter (npar = 6, nfields=3, decimals=6)
        character(5) hduvers
        character(16) ttype(nfields), tform(nfields), tunits(nfields)
        character(70) string
        character(80) message
c Initialization
        ierr = 0
        status = 0

c User info, if requested
        message = ' using '//subname//' '//version
        call wtinfo(chatter,15,1,message)

c Check for sillies
        if(rmfversn(1:1).NE.'1') then
           message = 'Format/subroutine mismatch'
           call wterrm(subname, version,message)
           message =
     &          ' This routine writes only the 1.*.* family' //
     &          ' of formats'
           call wtinfo(chatter,1,1,message)
           message = ' requested format: ' // rmfversn
           call wtinfo(chatter,1,2,message)
           ierr = 15
           goto 998
        endif

c Check that we know the format, and override if an old format
        if(rmfversn.EQ.'1.3.0') then
           hduvers = rmfversn
        elseif((rmfversn.EQ.'1.0.0').OR.(rmfversn.EQ.'1.1.0').or.
     &          rmfversn.eq.'1.2.0'  ) then
           hduvers = '1.3.0'
           message = subname // ' Old EBOUNDS format requested: '
     &         // rmfversn
           call wtinfo(chatter,1,2,message)
           message = 'Resetting format to HDUVERS=' // hduvers
           call wtinfo(chatter,1,2,message)
        else
           hduvers = '1.3.0'
           message = 'Unsupported EBOUNDS format requested: '//rmfversn
           call wtwarm(subname, version, chatter, 1, message)
           message = 'Resetting format to HDUVERS=' // hduvers
           call wtinfo(chatter,1,2,message)
        endif

c Create a new extension
        call FTCRHD(ounit,status)
        if(status.ne.0) then
           call wtferr(subname,version,status,
     &          'Problem creating new extension')
           ierr = 1
           goto 998
        else
           call wtinfo(chatter,15,1,'new extension created')
        endif

c Set up the columns n stuff
        ttype(1)   = 'CHANNEL'
        tform(1)   = 'J'
        tunits(1)  = ' '
        ttype(2)   = 'E_MIN'
        tform(2)   = 'E'
        tunits(2)  = 'keV'
        ttype(3)   = 'E_MAX'
        tform(3)   = 'E'
        tunits(3)  = 'keV'

c Write the required header keywords
        call FTPHBN(ounit,iebound,nfields,ttype,tform,tunits,
     &          'EBOUNDS',0,status)
        if(status.ne.0) then
           call wtferr(subname,version,status,
     &          'Problem writing header keywords')
           ierr = 1
           goto 998
        else
           message = 'written the extension header keywords'
           call wtinfo(chatter,15,1,message)
        endif
c WRITE THE EXTNAME keyword
        call FTUKYS(ounit,'EXTNAME',
     &          extname,
     &          extcomm,
     &          status)
        if(status.ne.0) then
           call wtferr(subname,version,status,
     &          'Problem writing EXTNAME keyword')
           status = 0
        endif

c WRITE THE HDUCLASn & HDUVERS keywords
        call FTPKYS(ounit,'HDUCLASS',
     &          'OGIP',
     &          'format conforms to OGIP standard',
     &          status)
        if(status.ne.0) then
           call wtferr(subname,version,status,
     &          'Problem writing HDUCLASS keyword')
           status = 0
        endif

        call FTPKYS(ounit,'HDUCLAS1',
     &          'RESPONSE',
     &          'dataset relates to spectral response',
     &          status)
        if(status.ne.0) then
           call wtferr(subname,version,status,
     &          'Problem writing HDUCLAS1 keyword')
           status = 0
        endif

        call FTPKYS(ounit,'HDUCLAS2',
     &          'EBOUNDS',
     &          'nominal energies of PHA chan boundaries',
     &          status)
        if(status.ne.0) then
           call wtferr(subname,version,status,
     &          'Problem writing HDUCLAS2 keyword')
           status = 0
        endif

        call FTPKYS( ounit,'HDUVERS', hduvers,
     &          'Version of format (OGIP memo CAL/GEN/92-002a)',
     &          status)
        if(status.ne.0) then
           call wtferr(subname,version,status,
     &          'Problem writing HDUVERS keyword')
           status = 0
        endif

        call FTPKYS( ounit, 'HDUDOC',
     &      'OGIP memos CAL/GEN/92-002 & 92-002a',
     &      'Documents describing the format', status)
        if(status.ne.0) then
            call wtferr(subname,version,status,
     &          'Problem writing HDUDOC keyword')
            status = 0
        endif

        call FTPKYS(ounit,'HDUVERS1 ', '1.0.0',
     &          'Obsolete - included for backwards compatibility',
     &          status)

        call FTPKYS(ounit,'HDUVERS2 ', hduvers,
     &          'Obsolete - included for backwards compatibility',
     &          status)
        if(status.ne.0) then
           call wtferr(subname,version,status,
     &          'Problem writing HDUVERS1 or 2 keyword')
           status = 0
        endif

c Add the other (passed) OGIP required keywords
        call FTPKYS(ounit,'TELESCOP ',
     &          telescop,
     &          'mission/satellite name',
     &          status)
        if(status.ne.0) then
           call wtferr(subname,version,status,
     &          'Problem writing TELESCOP keyword')
           status = 0
        endif

        call FTPKYS(ounit,'INSTRUME ',
     &          instrume,
     &          'instrument/detector name',
     &          status)
        if(status.ne.0) then
           call wtferr(subname,version,status,
     &          'Problem writing INSTRUME keyword')
           status = 0
        endif

        if(detnam.NE.' ') then
        call FTPKYS(ounit,'DETNAM ',
     &          detnam,
     &          'specific detector name in use',
     &          status)
        if(status.ne.0) then
           call wtferr(subname,version,status,
     &          'Problem writing DETNAM keyword')
           status = 0
        endif
        endif

        call FTPKYS(ounit,'FILTER   ',
     &          filter,
     &          'filter in use',
     &          status)
        if(status.ne.0) then
           call wtferr(subname,version,status,
     &          'Problem writing FILTER keyword')
           status = 0
        endif

        call FTPKYJ(ounit,'DETCHANS ',
     &          iebound,
     &          'total number of detector channels',
     &          status)
        if(status.ne.0) then
           call wtferr(subname,version,status,
     &          'Problem writing DETCHANS keyword')
           status = 0
        endif

        if((chantype.NE.'PHA').or.(chantype.NE.'PI')) then
           string = 'WARNING This is NOT an OGIP-approved value'
        else
           string = 'Detector Channel Type in use (PHA or PI)'
        endif
        call ftpkys(ounit,'CHANTYPE',
     &                  chantype, string,
     &                  status)
        if(status.ne.0) then
           call wtferr(subname,version,status,
     &          'Problem writing CHANTYPE keyword')
           status = 0
        endif

        call FTPKYF(ounit,'EFFAREA ',
     &          areascal, decimals,
     &          'Area scaling factor',
     &          status)
        if(status.ne.0) then
           call wtferr(subname,version,status,
     &          'Problem writing EFFAREA keyword')
           status = 0
        endif

        call FTPKYS(ounit,'RMFVERSN ', '1992a',
     &      'Obsolete - included for backwards compatibility', status)
        if(status.ne.0) then
            call wtferr(subname,version,status,
     &          'Problem writing RMFVERSN keyword')
            status = 0
        endif

        call FTPKYJ(ounit,'TLMIN1 ',
     &          fchan,
     &          'Minimum value legally allowed in column 1',
     &          status)
        if(status.ne.0) then
           call wtferr(subname,version,status,
     &          'Problem writing TLMIN1 keyword')
           status = 0
        endif

        call FTPKYJ(ounit,'TLMAX1 ',
     &          fchan+iebound-1,
     &          'Maximum value legally allowed in column 1',
     &          status)
        if(status.ne.0) then
           call wtferr(subname,version,status,
     &          'Problem writing TLMAX1 keyword')
           status = 0
        endif

        call wtinfo(chatter,15,1,'written the OGIP required keywords')

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
        write(string,'(4a)')
     &                  'EBOUNDS extension written by ',
     &                   subname, ' ', version
        call FTPHIS(ounit,string,status)
        if(itemp.ne.0) then
           call wtferr(subname,version,itemp,
     &          'Problem writing at least one History record')
        else
           call wtinfo(chatter,15,1,'written the history keywords')
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
        if(itemp.ne.0) then
           call wtferr(subname,version,itemp,
     &          'Problem writing at least one COMMENT record')
        else
           call wtinfo(chatter,15,1,'written the comment keywords')
        endif
        status = 0

c Define the extension data structure
        call FTBDEF(ounit,nfields,tform,0,iebound,status)
        if(status.ne.0) then
           call wtferr(subname, version, status,
     &          'Problem defining Data Structure')
           ierr = 1
           goto 998
        else
           call wtinfo(chatter,15,1,'defined the data structure')
        endif

c Write the data
        do ie = 1, iebound
                ichan = fchan + ie - 1
                call FTPCLJ(ounit, 1, ie, 1, 1, ichan,status)
                call FTPCLE(ounit, 2, ie, 1, 1, e_min(ie),status)
                call FTPCLE(ounit, 3, ie, 1, 1, e_max(ie),status)
        enddo

c Final check for errors
        if(status.ne.0) then
           call wtferr(subname, version, status,
     &          'writing the data')
           ierr = 1
           goto 998
        endif

998     if(ierr.NE.0) then
          call wterrm(subname, version, 'Fatal - aborting')
        else
          call wtinfo(chatter,15,1,
     &          'successfully written EBOUNDS data')
        endif

        return
        end
