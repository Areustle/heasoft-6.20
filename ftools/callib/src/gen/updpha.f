*+UPDPHA
      subroutine updpha(iunit,ounit,chatter,nk_hist,hist,nk_comm,
     &                  comment,telescop,instrume,detnam,filter,
     &                  phaversn,hduclas2,fchan,texpos,qareasc,areascal,
     &                  backfil,qbacksc,backscal,corrfil, 
     &                  corrscal,respfil,ancrfil,detchans,chantyp,
     &                  channel,counts,dtype,qerror,serr,qsys,
     &                  syserr,qqual,qualty,qgroup,grping,nchan,
     &                  ierr)

        IMPLICIT NONE
        integer iunit,ounit,chatter,nk_hist, nk_comm
        integer detchans, dtype, nchan, ierr
        integer channel(detchans),qualty(detchans),grping(detchans)
        integer fchan
        real texpos,corrscal
        real counts(detchans), serr(detchans), syserr(detchans)
        real areascal(*), backscal(*)
        character*(*) phaversn
        character*(*) telescop,instrume,detnam,filter
        character*(*) chantyp
        character*(*) hduclas2
        character*(*) hist(nk_hist), comment(nk_comm)
        character*(*) backfil,corrfil,respfil,ancrfil
        logical qerror,qsys,qqual,qgroup
        logical qareasc,qbacksc
c
c Description 
c   This subroutine updates the SPECTRUM extension for a PHA file in 
c one of the formats conforming to the HDUVERS='1.*.*' family. 
c Currently the following formats are supported (see OGIP/92-007a)
c   HDUVERS1 = '1.0.0' 
c   HDUVERS1 = '1.1.0'
c   HDUVERS  = '1.1.0'
c   HDUVERS  = '1.2.0'
c The requested format is checked, and if belonging to the '1.*.*' family,
c but not included above, the extension is written in the last format listed.
c !!! Note !!! the o/p file is assumed to have been opened, and wound to the 
c              desired location. The file is left open at the end of the 
c              newly written SPECTRUM extension on return and MUST be closed 
c              using FTCLOS or another extension written starting with FTCRHD
c              in order that the mandatory END keyword is written              
c In all cases, the 1.*.* family of formats consists of a BINTABLE extension, 
c with the number of rows equal to the number of channels passed (this does 
c not have to be the total number of detector channels the instrument is 
c capable of).
c
c Passed Parameters
c  OUNIT        i   : FORTRAN unit number of open PHA file
c  CHATTER      i   : chattiness flag for o/p (5 quite,10 norm,>19 silly)
c  NK_HIST      i   : No. records to be written as HISTORY records
c  HIST         i   : Array of history record strings to be written
c  NK_COMM      i   : No. records to be written as COMMENT records
c  COMMENT      i   : Array of comment record strings to be written
c  TELESCOP     i   : String giving telescope/mission 
c  INSTRUME     i   : String giving instrument/detector name
c  DETNAM       i   : String giving specific detector name/code (if any)
c  FILTER       i   : String giving instrument filetr in use (if any)
c  PHAVERSN     i   : String denoting OGIP HDUVERS family
c  HDUCLAS2     i   : String containing HDUCLAS2 value
c  FCHAN        i   : First legal channel number (ie 0, 1 etc)
c  TEXPOS       i   : exposure (Live) time
c  QAREASC      i   : true if a vector AREASCAL is passed
c  AREASCAL     i   : area scaling factor
c  BACKFIL      i   : associated background filename
c  QBACKSC      i   : true if a vector BACKSCAL is passed
c  BACKSCAL     i   : background scaling factor
c  CORRFIL      i   : associated correction filename
c  CORRSCAL     i   : correction scaling factor
c  RESPFIL      i   : detector redistribution matrix file (RMF)
c  ANCRFIL      i   : ancillary response file (ARF)
c  DETCHANS     i   : total number of possible detector channels 
c  CHANTYP      i   : type of detector channels (PHA, PI etc)
c  CHANNEL      i   : array of detector channel numbers
c  COUNTS       i   : array of observed counts (or count rate) per chan
c  DTYPE        i   : flag to denote counts (1) or count rates (2)
c  QERROR       i   : flag as to whether stat errors passed down
c  SERR         i   : array of statistical errors on COUNTS (for QERROR=T)
c  QSYS         i   : flag as to whether systematic errors passed down
c  SYSERR       i   : array of systematic errors on COUNTS (for QSYS=T)
c  QQUAL        i   : flag as to whether quality array passed down
c  QUALTY       i   : array of quality flags (for QQUAL=T)
c  QGROUP       i   : flag as to whether grouping array passed down
c  GRPING       i   : array of quality flags (for QGROUP=T)
c  NCHAN        i   : No. channels actually passed down
c  IERR           o : Error Flag (0=OK)
c                          
c Format Written
c  Each row consists of the following columns/contents:
c  CHANNEL - (int) the channel number
c  either: COUNTS  - (int) the number of counts/channel 
c  or      RATE    - (real) the number of counts/channel/second
c  STAT_ERR- (real) the statistical error on COUNTS or RATE
c  SYS_ERR - (real) the fractional systematic error on COUNTS or RATE
c  <Any existing columns from the input file not included in this list>
c  QUALITY - (int) quality flag for the data in channel
c  GROUPING- (int) grouping flag for this channel
c  AREASCAL- (real) area scaling for this channel
c  BACKSCAL- (real) background scaling for this channel
c If all the rows would contain the same value in the STAT_ERR,SYS_ERR,
c   QUALITY,GROUPING,AREASCAL, or BACKSCAL columns, then that column is 
c   not written and the value supplied by a header keyword instead.
c
c The following keywords are written:
c  HDUCLASS='OGIP'-indicating the format conforms to OGIP standards
c  HDUCLAS1='SPECTRUM' - indicating major class in the heirarchy
c  HDUVERS - (passed) The phaversn to be written (must in '1.*.*' family)
c  HDUCLAS2- (passed) - Whether the spectrum is srce, bkgd, or both
c  HDUCLAS3- Indicating whether the data are stored as counts of count rates
c  TELESCOP- The mission/satellite name'
c  INSTRUME- The instrument/detector name'
c  DETNAM  - The sub-detector in use
c  FILTER  - The name of the filter in use
c  EXPOSURE- The exposure time (or "live-time") in seconds
c  BACKFIL - The name of the associated background file
c  CORRFIL - The name of the associated correction file
c  CORRSCAL- The correction file scaling factor
c  RESPFILE- The name of the associated redistrib matrix file
c  ANCRFILE- The name of the associated ancillary response file
c  PHAVERSN='1992a' - The OGIP classification of FITS format
c  DETCHANS- The total number possible detector channels
c  CHANTYPE- The channel type (PHA, PI etc)
c  POISSERR- Logical as the whether poissonian errors are to be assumed
c
c Called routines etc
c subroutine FCECHO        : (FTOOLS) Writes to standard o/p
c subroutine FTOPEN        : (FITSIO) Opens FITS file
c subroutine FTCRHD        : (FITSIO) Creates header
c subroutine FTMAHD        : (FITSIO) Move to specified header number
c subroutine FTMCOM        : (FITSIO) Modify comment of existing keyword
c subroutine FTMRHD        : (FITSIO) Move a specified number of headers
c subroutine FTGHSP        : (FITSIO) Obtain the number of keywords
c subroutine FTPHIS        : (FITSIO) Write history keywords
c subroutine FTBDEF        : (FITSIO) Define Binary header
c subroutine FTPCLn        : (FITSIO) Write FITS column of type n
c subroutine WT_FERRMSG    : (CALLIB) writes FITSIO error message 
c 
c Authors/modification history 
c   kaa    3/19/01    modified from wrpha1.f v3.2.1 for HDUVERS=1.2.0
c   bki    3/28/03    v1.0.1:
c       For potentially long filename keywords (BACKFILE, CORRFILE, RESPFILE,
c       ANCRFILE) change FTPKYS calls to FTPKLS (write long string keywords:
c       will put in a CONTINUE keyword when necessary) and warn using FTPLSW
c       if applicable.
c   bki    8/27/07    modified from wtpha2.f v1.0.1 to update the spectrum
c                     extension (rather than just write a new one), i.e.
c                     any additional columns in the input file are copied.
c ----------------------------------------------------------------------
      character(5) version
      parameter (version = '1.0.0')
*-
c Internals 
        integer maxsize
        parameter (maxsize=1000000)
        integer*2  conv2(maxsize)
        integer conv4(maxsize)
        integer nfields,status, decimals, itemp
        parameter (nfields = 16, decimals=6)
        integer i,key_val,vd, lchan
        integer frow,felem,colnum,tfields
        character(5) hduvers
        character(8) keywrd
        character(20) keyval
        character(16) ttype(nfields),tform(nfields),tunit(nfields)
        character(16) tformat
        character(40) tcomm(nfields)
        character(30) errstr, wrnstr
        character(70) subinfo
        integer fcstln, strlen
        integer n,icol,ncols
        character(20) colnam(nfields)
        character comm*80, contxt*72, extrattype*8, extratform*8

c Initialization
        ierr = 0
        status = 0
        errstr = '** UPDPHA '//version//' ERROR:'
        wrnstr = '** UPDPHA '//version//' WARNING:'
        DO n = 1, nfields
           ttype(n)=''
           tform(n)=''
           tunit(n)=''
           tcomm(n)=''
        ENDDO

c --- USER INFO ---
c
      IF (chatter.GE.20) THEN
         subinfo = ' ... using UPDPHA Ver '//version
         call fcecho(subinfo)
      ENDIF
c Check for sillies
        if(nchan.gt.maxsize) then
           subinfo = errstr // ' Internal arrays too small'
           call fcecho(subinfo)
           subinfo = ' ... MAXSIZE needs to be increased'
           call fcecho(subinfo)
           ierr = 3
           goto 998
        endif
        if(phaversn(1:1).NE.'1') then
           subinfo = wrnstr // ' Format/subroutine mismatch'
           call fcecho(subinfo)
           subinfo = 
     &        ' ...... This routine writes only the 1.*.* family' //
     &        ' of formats'
           call fcecho(subinfo)
           subinfo = 
     &        ' ...... requested format: '// phaversn
           call fcecho(subinfo)
           ierr = 15
           goto 998
        endif
c Check that we know the format
        if(phaversn.EQ.'1.0.0'.OR.phaversn.EQ.'1.1.0'.OR.
     &     phaversn.EQ.'1.2.0') then
           hduvers = phaversn
        else
           hduvers = '1.2.0'
           subinfo = wrnstr // ' Unknown format: '// phaversn
           call fcecho(subinfo)
           subinfo = 
     &              ' ...... Resetting format (HDUVERS) to '//hduvers
           call fcecho(subinfo)
        endif

c Check that the HDUVERS requested is compatible with the input data

        IF ( (hduvers.EQ.'1.0.0'.OR.hduvers.EQ.'1.1.0') .AND.
     &       (qareasc.OR.qbacksc) ) THEN
           subinfo = errstr // 'Vector AREASCAL or BACKSCAL requested'
           CALL fcecho(subinfo)
           subinfo = 'for HDUVERS < 1.2.0'
           CALL fcecho(subinfo)
           ierr = 1
        ENDIF

c
c --- SETUP DATA EXTENSION ---
c
c Create data extension

      call ftcrhd(ounit,status)
      subinfo = errstr // ' On return from FTCRHD'
      call wt_ferrmsg(status,subinfo)
      if(status.ne.0) then
        ierr = 1
        goto 998
      endif
      if(chatter.ge.20) then
        subinfo = ' ... new extension created'
        call fcecho(subinfo)
      endif

c Setup header keywords
      tfields = 2
      ttype(1) ='CHANNEL'
      IF ( hduvers .EQ. '1.2.0' ) THEN
         tform(1) = 'J'
      ELSE
         tform(1) = 'I'
      ENDIF
      tunit(1) = ' '
      if(chantyp.eq.'PHA' .OR. chantyp.EQ.'pha') then
        tcomm(1) = 'Pulse Height Analyser (PHA) Channel '
      elseif(chantyp.eq.'PI' .OR. chantyp.EQ.'pi') then
        tcomm(1) = 'Pulse Invarient (PI) Channel '
      else
        tcomm(1) = 'Detector channel (type unknown)'
      endif
      IF (dtype .EQ. 1) THEN
         ttype(2) = 'COUNTS'
         tform(2) = 'J'
         tunit(2) = 'count'
         tcomm(2) = 'Counts per channel'
      ELSEIF (dtype .EQ. 2) THEN
         ttype(2) = 'RATE'
         tform(2) = 'E'
         tunit(2) = 'count/s'
         tcomm(2) = 'Counts per second per channel'
      ENDIF
      IF (qerror) THEN
         tfields = tfields + 1
         ttype(tfields) = 'STAT_ERR'
         tform(tfields) = 'E'
         if(dtype .EQ. 1) then
           tunit(tfields) = 'count'
           tcomm(tfields) = 'Statistical error on COUNTS'
         elseif(dtype .EQ. 2) then
           tunit(tfields) = 'count/s'
           tcomm(tfields) = 'Statistical error on RATE'
         endif
      ENDIF
      IF (qsys) THEN
         tfields = tfields + 1
         ttype(tfields) = 'SYS_ERR'       
         tform(tfields) = 'E'
         tunit(tfields) = ' '
         if(dtype .EQ. 1) then
           tcomm(tfields) = 'Fractional systematic error on COUNTS'
         elseif(dtype .EQ. 2) then
           tcomm(tfields) = 'Fractional systematic error on RATE'
         endif
      ENDIF

      call ftgncl(iunit, ncols, status)
      contxt = 'Failed to read # of columns from SPECTRUM extension'
      IF ( status .NE. 0 ) GOTO 998

      DO icol = 1, ncols
         extrattype = 'TTYPE'
         extratform = 'TFORM'
         IF ( icol .LE. 9 ) THEN
            WRITE(extrattype(6:6), '(i1)') icol
            WRITE(extratform(6:6), '(i1)') icol
         ELSEIF ( icol .LE. 99 ) THEN
            WRITE(extrattype(6:7), '(i2)') icol
            WRITE(extratform(6:7), '(i2)') icol
         ELSE
            WRITE(extrattype(6:8), '(i3)') icol
            WRITE(extratform(6:8), '(i3)') icol
         ENDIF
         CALL ftgkys(iunit,extrattype,colnam(icol),comm,status)
         contxt = 'Failed to read '//extrattype//
     &            ' from SPECTRUM extension'
         CALL ftgkys(iunit,extratform,tformat,comm,status)
         contxt = 'Failed to read '//extratform//
     &            ' from SPECTRUM extension'
         IF ( status .NE. 0 ) GOTO 998
         IF ( colnam(icol) .NE. 'CHANNEL' .AND.
     &        colnam(icol) .NE. 'COUNTS' .AND.
     &        colnam(icol) .NE. 'RATE' .AND.
     &        colnam(icol) .NE. 'STAT_ERR' .AND.
     &        colnam(icol) .NE. 'SYS_ERR' .AND.
     &        colnam(icol) .NE. 'QUALITY' .AND.
     &        colnam(icol) .NE. 'GROUPING' .AND.
     &        colnam(icol) .NE. 'AREASCAL' .AND.
     &        colnam(icol) .NE. 'BACKSCAL' ) THEN
            tfields = tfields + 1
            ttype(tfields) = colnam(icol)
            tform(tfields) = tformat
         ENDIF
      ENDDO

      IF (qqual) THEN
         tfields = tfields + 1
         ttype(tfields) = 'QUALITY'
         tform(tfields) = 'I'
         tunit(tfields) = ' '
         tcomm(tfields) = 'Quality flag of this channel (0=good)'
      ENDIF
      IF (qgroup) THEN
         tfields = tfields + 1
         ttype(tfields) = 'GROUPING'
         tform(tfields) = 'I'
         tunit(tfields) = ' '
         tcomm(tfields) = 'Grouping flag for channel (0=undefined)'
      ENDIF
      IF (qareasc) THEN
         tfields = tfields + 1
         ttype(tfields) = 'AREASCAL'
         tform(tfields) = 'E'
         tunit(tfields) = ' '
         tcomm(tfields) = 'Area scaling factor'
      ENDIF
      IF (qbacksc) THEN
         tfields = tfields + 1
         ttype(tfields) = 'BACKSCAL'
         tform(tfields) = 'E'
         tunit(tfields) = ' '
         tcomm(tfields) = 'Background scaling factor'
      ENDIF


c --- WRITE THE MAIN HEADER KEYWORDS ---

      status = 0
      vd = 0
      CALL ftphbn(ounit, nchan, tfields, ttype, tform, tunit,
     &            'SPECTRUM', vd, status)     
        subinfo = errstr // 'On return from FTPHBN'
        call wt_ferrmsg(status,subinfo)
        if(status.NE.0) then
           ierr = 1
           goto 998
        endif
        if(chatter.ge.20) then
          subinfo = ' ... written the extension header keywords'
          call fcecho(subinfo)
        endif
c 
c - Fix up the Comments in the TTYPE keywords
c
        status = 0
        do i = 1, tfields
          write(keywrd,'(a5,i2)') 'TTYPE',i
          call crmvblk(keywrd)
          call ftmcom(ounit,keywrd,tcomm(i),status)
          if(status.NE.0) then
           subinfo = wrnstr // 'Problem altering '// keywrd
           call wt_ferrmsg(status,subinfo)
           status = 0
          endif
        enddo


c
c --- WRITE THE HDUCLASn & HDUVERS keywords
c
        call FTPKYS(ounit,'HDUCLASS ',
     &        'OGIP',
     &        'format conforms to OGIP standard',
     &        status)
        subinfo = wrnstr // ' Problem putting HDUCLASS keyword '
        call wt_ferrmsg(status, subinfo)
        status = 0

        call FTPKYS(ounit,'HDUCLAS1 ',
     &        'SPECTRUM',
     &        'PHA dataset (OGIP memo OGIP-92-007)',
     &        status)
        subinfo = wrnstr // ' Problem putting HDUCLAS1 keyword '
        call wt_ferrmsg(status, subinfo)
        status = 0

        IF ( hduvers .EQ. '1.0.0' ) THEN
           call FTPKYS(ounit,'HDUVERS1 ', hduvers,
     &        'Version of format (OGIP memo OGIP-92-007a)',
     &        status)
           subinfo = wrnstr // ' Problem putting HDUVERS1 keyword '
           call wt_ferrmsg(status, subinfo)
           status = 0
        ELSE
           call FTPKYS(ounit,'HDUVERS1 ', hduvers,
     &        'Obsolete - included for backwards compatibility',
     &        status)
           subinfo = wrnstr // ' Problem putting HDUVERS1 keyword '
           call wt_ferrmsg(status, subinfo)
           status = 0
           call FTPKYS(ounit,'HDUVERS ', hduvers,
     &        'Version of format (OGIP memo OGIP-92-007)',
     &        status)
           subinfo = wrnstr // ' Problem putting HDUVERS keyword '
           call wt_ferrmsg(status, subinfo)
           status = 0
        ENDIF

        keyval = hduclas2
        call ftupch(keyval)
        if(keyval.EQ.'TOTAL')then
                subinfo = 'Gross PHA Spectrum (source + bkgd)'
        elseif(keyval.EQ.'NET')then
                subinfo = 'Bkgd-subtracted PHA Spectrum'
        elseif(keyval.EQ.'BKG')then
                subinfo = 'Bkgd PHA Spectrum'
        elseif(keyval.EQ.'UNKNOWN')then
                subinfo = 'Maybe TOTAL, NET or BKG Spectrum'
        else
                subinfo = 'WARNING This is NOT an OGIP-approved value'
        endif
        call FTPKYS(ounit,'HDUCLAS2 ',
     &              keyval,
     &              subinfo,
     &              status)
        subinfo = wrnstr // ' Problem putting HDUCLAS2 keyword '
        call wt_ferrmsg(status, subinfo)
        status = 0


        if(dtype.EQ.1) then
                keyval  = 'COUNT'
                subinfo = 'PHA data stored as Counts (not count/s)'
        elseif(dtype.EQ.2) then
                keyval  = 'RATE'
                subinfo = 'PHA data stored in count/s'
        else
                keyval  = 'UNKNOWN'
                subinfo = 'Unknown storage method for PHA data'
        endif
        call FTPKYS(ounit,'HDUCLAS3 ',
     &              keyval,
     &              subinfo,
     &              status)
        subinfo = wrnstr // ' Problem putting HDUCLAS3 keyword '
        call wt_ferrmsg(status, subinfo)
        status = 0

c
c --- WRITE ADDITIONAL KEYWORDS DESCRIBING DATA
c
        call FTPKYJ(ounit,'TLMIN1',
     &              fchan,
     &              'Lowest legal channel number',
     &              status)
        subinfo = wrnstr // ' Problem putting TLMIN1 keyword '
        call wt_ferrmsg(status, subinfo)
        status = 0

        lchan = fchan + detchans - 1
        call FTPKYJ(ounit,'TLMAX1',
     &              lchan,
     &              'Highest legal channel number',
     &              status)
        subinfo = wrnstr // ' Problem putting TLMAX1 keyword '
        call wt_ferrmsg(status, subinfo)
        status = 0

        call FTPKYS(ounit,'TELESCOP ',
     &              telescop,
     &              'mission/satellite name',
     &              status)
        subinfo = wrnstr // ' Problem putting TELESCOP keyword '
        call wt_ferrmsg(status, subinfo)
        status = 0

        call FTPKYS(ounit,'INSTRUME ',
     &              instrume,
     &              'instrument/detector name',
     &              status)
        subinfo = wrnstr // ' Problem putting INSTRUME keyword '
        call wt_ferrmsg(status, subinfo)
        status = 0
           
        if(detnam.NE.' ') then
        call FTPKYS(ounit,'DETNAM ',
     &              detnam,
     &              'specific detector name in use',
     &              status)
        subinfo = wrnstr // ' Problem putting DETNAM keyword '
        call wt_ferrmsg(status, subinfo)
        status = 0
        endif
      
        if (filter.EQ.' ') THEN
          call FTPKYS(ounit,'FILTER ',
     &          'NONE',
     &          'no filter in use',
     &          status)
        ELSE

        call FTPKYS(ounit,'FILTER ',
     &              filter,
     &              'filter in use',
     &              status)
        endif
        subinfo = wrnstr // ' Problem putting FILTER keyword '
        call wt_ferrmsg(status, subinfo)
        status = 0

        call FTPKYE(ounit,'EXPOSURE ',
     &              texpos,decimals,
     &              'exposure (in seconds)',
     &              status)
        subinfo = wrnstr // ' Problem putting EXPOSURE keyword '
        call wt_ferrmsg(status, subinfo)
        status = 0

        IF ( .NOT.qareasc ) THEN
              call FTPKYE(ounit,'AREASCAL ',
     &                    areascal(1),decimals,
     &                    'area scaling factor',
     &                    status)
           subinfo = wrnstr // ' Problem putting AREASCAL keyword '
           call wt_ferrmsg(status, subinfo)
           status = 0
        ENDIF

        strlen = fcstln(backfil)
        if (strlen .GT. 68) then
           call FTPLSW(ounit,status)
        endif
        call FTPKLS(ounit,'BACKFILE ',
     &              backfil,
     &              'associated background filename',
     &              status)
        subinfo = wrnstr // ' Problem putting BACKFILE keyword '
        call wt_ferrmsg(status, subinfo)
        status = 0

        IF ( .NOT.qbacksc ) THEN
           call FTPKYE(ounit,'BACKSCAL ',
     &                 backscal,decimals,
     &                 'background file scaling factor',
     &                 status)
           subinfo = wrnstr // ' Problem putting BACKSCAL keyword '
           call wt_ferrmsg(status, subinfo)
           status = 0
        ENDIF

        strlen = fcstln(corrfil)
        if (strlen .GT. 68) then
           call FTPLSW(ounit,status)
        endif
        call FTPKLS(ounit,'CORRFILE ',
     &              corrfil,
     &              'associated correction filename',
     &              status)
        subinfo = wrnstr // ' Problem putting CORRFILE keyword '
        call wt_ferrmsg(status, subinfo)
        status = 0

        call FTPKYE(ounit,'CORRSCAL ',
     &              corrscal, decimals,
     &              'correction file scaling factor',
     &              status)
        subinfo = wrnstr // ' Problem putting CORRSCAL keyword '
        call wt_ferrmsg(status, subinfo)
        status = 0

        strlen = fcstln(respfil)
        if (strlen .GT. 68) then
           call FTPLSW(ounit,status)
        endif
        call FTPKLS(ounit,'RESPFILE ',
     &              respfil,
     &              'associated redistrib matrix filename',
     &              status)
        subinfo = wrnstr // ' Problem putting RESPFILE keyword '
        call wt_ferrmsg(status, subinfo)
        status = 0

        strlen = fcstln(ancrfil)
        if (strlen .GT. 68) then
           call FTPLSW(ounit,status)
        endif
        call FTPKLS(ounit,'ANCRFILE ',
     &              ancrfil,
     &              'associated ancillary response filename',
     &              status)
        subinfo = wrnstr // ' Problem putting ANCRFILE keyword '
        call wt_ferrmsg(status, subinfo)
        status = 0

        call FTPKYS(ounit,'PHAVERSN ', '1992a', 'obsolete', status)
        subinfo = wrnstr // ' Problem putting PHAVERSN keyword '
        call wt_ferrmsg(status, subinfo)
        status = 0

        call FTPKYJ(ounit,'DETCHANS ',
     &              detchans,
     &              'total number possible channels',
     &              status)
        subinfo = wrnstr // ' Problem putting DETCHANS keyword '
        call wt_ferrmsg(status, subinfo)
        status = 0

        call FTPKYS(ounit,'CHANTYPE ',
     &              chantyp,
     &              'channel type (PHA, PI etc)',
     &              status)
        subinfo = wrnstr // ' Problem putting CHANTYPE keyword '
        call wt_ferrmsg(status, subinfo)
        status = 0


      IF ((dtype.EQ.1).and.(.NOT.qerror)) THEN
         CALL ftpkyl(ounit, 'POISSERR', .TRUE.,
     &               'Poissonian errors to be assumed', status)
         subinfo = wrnstr // ' Problem putting POISSER keyword '
         call wt_ferrmsg(status, subinfo)
         status = 0
      ELSE
         CALL ftpkyl(ounit, 'POISSERR', .FALSE.,
     &               'Poissonian errors not applicable', status)
         subinfo = wrnstr // ' Problem putting POISSER keyword '
         call wt_ferrmsg(status, subinfo)
         status = 0
      ENDIF

      IF (.NOT. qerror) THEN
         key_val = 0
         CALL ftpkyj(ounit, 'STAT_ERR', key_val,
     &               'no statistical error specified', status)
         subinfo = wrnstr // ' Problem putting STAT_ERR keyword '
         call wt_ferrmsg(status, subinfo)
         status = 0
      ENDIF
      IF (.NOT. qsys) THEN
         key_val = 0
         CALL ftpkyj(ounit, 'SYS_ERR', key_val,
     &               'no systematic error specified', status)
         subinfo = wrnstr // ' Problem putting SYS_ERR keyword '
         call wt_ferrmsg(status, subinfo)
         status = 0
      ENDIF
      IF (.NOT. qgroup) THEN
         key_val = 0
         CALL ftpkyj(ounit, 'GROUPING', key_val,
     &               'no grouping of the data has been defined',
     &               status)
         subinfo = wrnstr // ' Problem putting GROUPING keyword '
         call wt_ferrmsg(status, subinfo)
         status = 0
      ENDIF
      IF (.NOT. qqual) THEN
         key_val = 0
         CALL ftpkyj(ounit, 'QUALITY', key_val,
     &               'no data quality information specified', status)
         subinfo = wrnstr // ' Problem putting QUALITY keyword '
         call wt_ferrmsg(status, subinfo)
         status = 0
      ENDIF        
      
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
        write(subinfo,'(2a)')
     &        ' FITS SPECTRUM extension written by UPDPHA ',
     &                          version
        call FTPHIS(ounit,subinfo,status)
        subinfo = wrnstr // 
     &          ' Problem putting at least one History record'
        call wt_ferrmsg(itemp, subinfo)
        if(chatter.GE.20) then
          subinfo = ' ... written the history keywords'
          call fcecho(subinfo)
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
        subinfo = wrnstr // 
     &          ' Problem putting at least one Comment record'
        call wt_ferrmsg(itemp, subinfo)
        status = 0
        if(chatter.GE.20) then
          subinfo = ' ... written the comment header keywords'
          call fcecho(subinfo)
        endif
c
c --- DEFINE DATA ---
c
      status = 0
      vd = 0
      call ftbdef(ounit,tfields,tform,vd,nchan,status)
c
c --- WRITE THE ELEMENTS INTO TABLE ---
c
      frow = 1
      felem = 1

c --- WRITE CHANNEL COLUMN ---

      do i=1,nchan
        conv2(i) = channel(i)
      enddo
      status = 0
      colnum = 1
      IF ( hduvers .EQ. '1.2.0' ) THEN
         call ftpclj(ounit,colnum,frow,felem,nchan,channel,status)
      ELSE
         call ftpcli(ounit,colnum,frow,felem,nchan,conv2,status)
      ENDIF
      if(status.NE.0) then
         subinfo = errstr // ' Writing CHANNEL Data'
         call wt_ferrmsg(status, subinfo)
         ierr = 2
         goto 998
      endif

c --- WRITE COUNTS ---

      status = 0
      colnum = 2
      frow = 1
      felem = 1
      IF (dtype .EQ. 1) THEN
         do i=1,nchan
           conv4(i) = NINT(counts(i))
         enddo
         CALL ftpclj(ounit,colnum,frow,felem,nchan,conv4,status)
      ELSEIF (dtype .EQ. 2) THEN
         CALL ftpcle(ounit,colnum,frow,felem,nchan,counts,status)
      ENDIF
      if(status.NE.0) then
         subinfo = errstr // ' Writing PHA Data'
         call wt_ferrmsg(status, subinfo)
         ierr = 3
         goto 998
      endif


c --- WRITE ERRORS IF PRESENT --- 

      IF (qerror) THEN
         status = 0
         frow=1
         felem=1
         colnum = colnum + 1
         CALL ftpcle(ounit,colnum,frow,felem,nchan,serr,status)
         if(status.NE.0) then
            subinfo = errstr // ' Writing STAT_ERR Data'
            call wt_ferrmsg(status, subinfo)
            ierr = 4
            goto 998
         endif
      ENDIF
      IF (qsys) THEN
         status = 0
         frow=1
         felem=1
         colnum = colnum + 1
         CALL ftpcle(ounit,colnum,frow,felem,nchan,syserr,status)
         if(status.NE.0) then
            subinfo = wrnstr // ' Writing SYS_ERR Data'
            call wt_ferrmsg(status, subinfo)
         endif
      ENDIF

c --- COPY ANY EXTRA COLUMNS IF PRESENT ---

      DO icol = 1, ncols
         IF ( colnam(icol) .NE. 'CHANNEL' .AND.
     &        colnam(icol) .NE. 'COUNTS' .AND.
     &        colnam(icol) .NE. 'RATE' .AND.
     &        colnam(icol) .NE. 'STAT_ERR' .AND.
     &        colnam(icol) .NE. 'SYS_ERR' .AND.
     &        colnam(icol) .NE. 'QUALITY' .AND.
     &        colnam(icol) .NE. 'GROUPING' .AND.
     &        colnam(icol) .NE. 'AREASCAL' .AND.
     &        colnam(icol) .NE. 'BACKSCAL' ) THEN
            colnum = colnum + 1
            CALL ftcpcl(iunit, ounit, icol, colnum, .FALSE., status)
            WRITE(contxt, '(a,i4)') 'Failed to copy column ', icol
            IF ( status .NE. 0 ) GOTO 998
         ENDIF
      ENDDO

c --- WRITE QUALITY IF PRESENT ---

      IF (qqual) THEN
         colnum = colnum + 1
         frow=1
         felem =1
         status = 0
         do i=1,nchan    
           conv2(i) = qualty(i)
         enddo
         CALL ftpcli(ounit,colnum,frow,felem,nchan,conv2,status)
         if(status.NE.0) then
            subinfo = wrnstr // ' Writing QUALITY Data'
            call wt_ferrmsg(status, subinfo)
         endif
      ENDIF

c --- WRITE GROUPING IF PRESENT ---

      IF (qgroup) THEN
         colnum = colnum + 1
         frow=1
         felem=1
         status = 0
         do i=1,nchan
           conv2(i) = grping(i)
         enddo
         CALL ftpcli(ounit,colnum,frow,felem,nchan,conv2,status)
         if(status.NE.0) then
            subinfo = wrnstr // ' Writing GROUPING Data'
            call wt_ferrmsg(status, subinfo)
         endif
      ENDIF

c --- WRITE VECTOR AREASCAL IF REQUIRED ---

      IF (qareasc) THEN
         colnum = colnum + 1
         frow=1
         felem=1
         status = 0
         CALL ftpcle(ounit,colnum,frow,felem,nchan,areascal,status)
         if(status.NE.0) then
            subinfo = wrnstr // ' Writing AREASCAL Data'
            call wt_ferrmsg(status, subinfo)
         endif
      ENDIF

c --- WRITE VECTOR AREASCAL IF REQUIRED ---

      IF (qbacksc) THEN
         colnum = colnum + 1
         frow=1
         felem=1
         status = 0
         CALL ftpcle(ounit,colnum,frow,felem,nchan,backscal,status)
         if(status.NE.0) then
            subinfo = wrnstr // ' Writing BACKSCAL Data'
            call wt_ferrmsg(status, subinfo)
         endif
      ENDIF

      if(chatter.GE.5) then
         subinfo = ' ... written the PHA data Extension'
         call fcecho(subinfo)
      endif

c
c --- WRITE ERROR MESSAGE ---
c
998   if(ierr.NE.0) then
         subinfo = errstr // 'FATAL: Extension not written'
         call fcecho(subinfo)
      endif


      return
      end


