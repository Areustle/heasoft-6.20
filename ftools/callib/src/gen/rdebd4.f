*+RDEBD4
c     --------------------------------------------------------
        subroutine rdebd4(iunit,chatter,extname, extcomm,
     &          maxchan,
     &          telescop,instrume,detnam,filter,areascal,
     &          chantype, flchan,
     &          iebound,channel,e_min,e_max,rmfversn,ierr)
c     --------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------------
c
c  Reads the EBOUNDS extension for an RMFVERSN=1992a RMF file
c Assumes the extension conforms to HDUVERS2='1.*.*' family
c Currently the following formats are supported -
c HDUVERS='1.0.0'
c HDUVERS='1.1.0'
c HDUVERS='1.2.0'
c see OGIP/92-002a
c The HDU CLASS keywords have only been currently introduced thus DO NOT
c have to be present to use this reader.
c
c  Assumes the FITS is open.
c  !!! Note !!!! File is left open at the end
c      ... close file using FTCLOS, or
c      ... read another extension
c
c  Columns read are ...
c  CHANNEL     : Channel numbers
c  E_MIN       : Nominal lower energy bound
c  E_MAX       : Nominal upper energy bound
c
c  Keywords read ...
c  TELESCOP : Mission/Telescope name, NOTE: If not present set to UNKNOWN
c  INSTRUME : Instrument/Detector name NOTE: If not present set to UNKNOWN
c  DETNAM   : Specific detector name NOTE: If not present set to NONE
c  FILTER   : Filter in use, if not present set to NONE
c  TLMIN/MAX: (for CHANNEL col) for legal min & max for channel numbering
c  CHANTYPE : Type of detector channel in use (PHA, PI)
c  EFFAREA  : Areascaling factor, if not present set to 1
c  RMFVERSN : RMF version
c
c --- VARIABLES -------------------------------------------------------
c
        IMPLICIT NONE
        integer chatter, ierr, maxchan
        integer iunit, flchan
        integer iebound, channel(maxchan)
        real areascal
        real e_min(maxchan), e_max(maxchan)
        character*(*) rmfversn, chantype
        character*(*) telescop, instrume, detnam, filter,extname,extcomm
c
c --- VARIABLE DIRECTORY ----------------------------------------------
c
c Passed parameters
c
c  IUNIT         i   : FORTRAN unit number of open RMF file
c  CHATTER       i   : chattiness flag for o/p (0 quite,10 normal,>20 silly)
c  TELESCOP      o   : String listing telescope/mission
c  INSTRUME      o   : String listing instrument/detector
c  DETNAM        o   : String listing specific detector name
c  FILTER        o   : String listing instrument filter in use
c  AREA          o   : Area scaling factor
c  CHANTYPE : Type of detector channel in use (PHA, PI)
c  FLCHAN        o   : Lowest legal channel for this detector
c  IEBOUND       o   : No. channels in the full array
c  CHANNEL       o   : Channel array
c  E_MIN         o   : Array containing min nominal energy bound to each chan
c  E_MAX         o   : Array containing max nominal energy bound to each chan
c  RMFVERSN      o   : RMF version
c  IERR          o   : Error Flag, ierr = 0 okay
c                                  ierr = 1 error finding extension
c                                  ierr = 2 error finding Column number
c                                  ierr = 3 error reading data
c                                  ierr = 4 NAXIS2 not found
c                                  ierr = 5 maxchan, array size NOT large enough
c
c --- CALLED ROUTINES --------------------------------------------------
c
c  subroutine FTMRHD     : (FITSIO) Move to extension
c  subroutine FTGKYS     : (FITSIO) Read FITS extension header keyword
c  subroutine FTGCNO     : (FITSIO) Get column number
c  subroutine FTGCVx     : (FITSIO) Read data in x format
c  subroutine WT_FERRMSG : (CALLIB) Dumps FITSIO Error message etc
c
c Compilation & Linking
c  link with FITSIO & CALLIB & FTOOLS
c
c Authors/Modification History:
c PARENT Routine (rdebd1.f) history
c       Rehana Yusaf (1.0.0:93 Jul 15)
c       Rehana Yusaf (1.0.1:93 Oct 27) Rename from RD_EBD1992a, and
c                                   additional argument passed, rmfversn
c                                   Also HDUCLASS is now used to find
c                                   extension if not present then extname
c                                   searched for.
c       Rehana Yusaf (1.0.2:93 Nov 10) Read HDUVERS2 keyword to get rmfversn
c                                   if HDUVERSN not present the read RMFVERSN
c                                   previously RMFVERSN was read
c       Ian M George (1.1.0:93 Nov 17) Took out extension searching stuff
c       Rehana Yusaf (1.1.1:94 Jan 11) Additional argument, maxchan - array
c                                   dimension, compared to Naxis2 and error out
c                                   if too small
c       Rehana Yusaf (1.1.2:94 Jun 24) Less verbose;Only printer warnings at
c                                   high chatter. Also add fcecho call
c                                   after channel error check
c THIS ROUTINE (rdebd3.f) history
c Ian M George (1.0.0:96 Oct 04) copied from rdebd1 (v1.1.2), with flchan and
c                               chantype added as passed parameters
c
c Banashree M Seifert (1.1.0, Oct 10, 1996)
c       . wrtstr was char*80 made *8
c Jeff Guerber (1.2.0, 1999-04-05) Read HDUVERS kwd, via gtclas call, in
c       preference to HDUVERS2 or RMFVERSN. (HDUCLASn, EXTNAME aren't used.)
c ----------------------------------------------------------------------
        character(7) version
        parameter (version = '1.2.0')
*-
c Internals
        character(6) subname
        parameter (subname = 'rdebd3')
        character(30) comm, hduclass, hduclasn(9)
        character(80) message
        character(8) wrtstr
        character(8) schannel
        integer status,  inull, felem, frow, colnum, nclasn
        real enull
        logical anyflg
c Initialise
        ierr = 0
        status = 0

c User info, if requested
        message = 'using '//subname//' '//version
        call wtinfo(chatter,15,1,message)

c READING DATA
c NAXIS2 ...
      status = 0
      call ftgkyj(iunit,'NAXIS2',iebound,comm,status)
      IF (status.NE.0) THEN
        call wtferr(subname,version,status,
     &          ' reading NAXIS2 keyword')
        ierr = 4
        goto 987
      ENDIF
      IF (iebound.GT.maxchan) THEN
        ierr = 5
        call wtferr(subname,version,status,
     &          'Channel Array dimension is too small')
        goto 987
      ENDIF

c RMFVERSN ...
      rmfversn = '  '
      nclasn = 0
      status = 0
      call gtclas(iunit, 'RMFVERSN', extname, hduclass, hduclasn,
     &    nclasn, rmfversn, status)
      call wtfwrn(subname,version,chatter, 30, status,
     &    'Problem reading HDUCLAS/HDUVERS keywords')
      IF ( rmfversn .EQ. '1992a' ) rmfversn = '1.0.0'

c EXTNAME ...
      status = 0
      call ftgkys(iunit,'EXTNAME',extname,extcomm,status)
      call wtfwrn(subname,version,chatter, 30, status,
     &          'Problem reading EXTNAME keyword')
      IF (status.EQ.202) THEN
        EXTNAME = 'EBOUNDS'
        EXTCOMM = ' '
      ENDIF

c TELESCOP ...
      status = 0
      call ftgkys(iunit,'TELESCOP',telescop,comm,status)
      call wtfwrn(subname,version,chatter, 20, status,
     &          'Problem reading TELESCOP keyword')
      IF (status.EQ.202) THEN
        telescop = 'UNKNOWN'
      ENDIF

c INSTRUME ...
      status = 0
      call ftgkys(iunit,'INSTRUME',instrume,comm,status)
      call wtfwrn(subname,version,chatter, 20, status,
     &          'Problem reading INSTRUME keyword')
      IF (status.EQ.202) THEN
        instrume = 'UNKNOWN'
      ENDIF

c FILTER ...
      status = 0
      call ftgkys(iunit,'FILTER',filter,comm,status)
      call wtfwrn(subname,version,chatter, 30, status,
     &          'Problem reading FILTER keyword')
      IF (status.EQ.202) THEN
        filter = 'NONE'
      ENDIF

c DETNAM ...
      status = 0
      call ftgkys(iunit,'DETNAM',detnam,comm,status)
      call wtfwrn(subname,version,chatter, 30, status,
     &          'Problem reading DETNAM keyword')
      IF (status.EQ.202) THEN
        detnam = 'NONE'
      ENDIF

c CHANTYPE ...
      status = 0
      call ftgkys(iunit,'CHANTYPE',chantype,comm,status)
      if(status.NE.0) then
        call wtfwrn(subname,version,chatter, 20, status,
     &          'Problem reading CHANTYPE keyword from EBOUNDS')
        call wtinfo(chatter,1, 1,'setting CHANTYPE = UNKNOWN')
        chantype = 'UNKNOWN'
        status= 0
      endif

c EFFAREA ...
      status = 0
      call ftgkye(iunit,'EFFAREA',areascal,comm,status)
      call wtfwrn(subname,version,chatter, 30, status,
     &          'Problem reading EFFAREA keyword')
      IF (status.NE.0) THEN
        areascal = 1
      ENDIF
c Reasure user, if requested
        call wtinfo(chatter,20,1,'read all the keywords')
        call wtinfo(chatter,20,1,'reading the data')

c CHANNEL COLUMN NUMBER ...
      status = 0
      call ftgcno(iunit,.false.,'CHANNEL',colnum,status)
      IF (status.NE.0) THEN
        call wtferr(subname,version,status,
     &          'Problem finding CHANNEL column')
         ierr = 4
         goto 987
      ENDIF

c READ CHANNEL COLUMN ...
      status = 0
      frow = 1
      felem = 1
      inull = 0
      call ftgcvj(iunit,colnum,frow,felem,iebound,inull,channel,
     &            anyflg,status)
      IF (status.NE.0) THEN
        call wtferr(subname,version,status,
     &          'Problem reading CHANNEL column')
        ierr = 3
        goto 987
      ENDIF
        write(wrtstr, '(a,i1)') 'TLMIN', colnum
        call crmvblk(wrtstr)

        status = 0
        call ftgkyj(iunit,wrtstr(1:8),flchan,comm,status)
        if(status.NE.0) then
            message = 'Problem reading '//wrtstr//' keyword'
            call wtwarm(subname,version,chatter,20,message)
            write(schannel, '(i8)') channel(1)
            message = 'Using first element of CHANNEL column: '//
     &          schannel//' as first legal channel'
            call wtinfo(chatter,20, 1,message)
            flchan = channel(1)
            status= 0
        endif

c E_MIN COLUMN NUMBER ...
      status = 0
      call ftgcno(iunit,.false.,'E_MIN',colnum,status)
      IF (status.NE.0) THEN
        call wtferr(subname,version,status,
     &          'Problem finding E_MIN column')
         ierr = 2
         goto 987
      ENDIF

c READ E_MIN ...
      status = 0
      enull = 0
      call ftgcve(iunit,colnum,frow,felem,iebound,enull,e_min,
     &            anyflg,status)
      IF (status.NE.0) THEN
        call wtferr(subname,version,status,
     &          'Problem reading E_MIN column')
        ierr = 3
        goto 987
      ENDIF

c E_MAX COLUMN NUMBER ...

      status = 0
      call ftgcno(iunit,.false.,'E_MAX',colnum,status)
      IF (status.NE.0) THEN
        call wtferr(subname,version,status,
     &          'Problem finding E_MIN column')
         ierr = 2
         goto 987
      ENDIF

c READ E_MAX ...
      status = 0
      enull = 0
      call ftgcve(iunit,colnum,frow,felem,iebound,enull,e_max,
     &            anyflg,status)
      IF (status.NE.0) THEN
        call wtferr(subname,version,status,
     &          'Problem reading E_MIN column')
        ierr = 3
        goto 987
      ENDIF

987     if(ierr.NE.0) then
          call wterrm(subname, version, ' Fatal - aborting')
        else
          call wtinfo(chatter,20,1,
     &          'successfully read EBOUNDS data')
        endif

      return
      end
c --------------------------------------------------------------------
c     END OF RDEBD3
c --------------------------------------------------------------------
