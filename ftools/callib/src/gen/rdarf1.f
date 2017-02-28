*+RDARF1
c     --------------------------------------------------------
        subroutine rdarf1(iunit,chatter,
     &          telescop,instrume,detnam,filter,
     &          iebound,e_lo,e_hi,sprsp, arfversn,ierr)
c     --------------------------------------------------------
        IMPLICIT NONE
        integer chatter, ierr
        integer iunit
        integer iebound
        real e_lo(*), e_hi(*)
        real sprsp(*)
        character*(*) arfversn
        character*(*) telescop, instrume, detnam, filter

c --- DESCRIPTION ------------------------------------------------------
c  Reads the SPECRESP extension for an ARFVERSN=1992a ARF file
c Assumes the extension conforms to HDUVERS2='1.*.*' family
c Currently the following formats are supported -
c HDUVERS2='1.0.0'
c HDUVERS2='1.1.0'
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
c  ENERG_LO    : lower energy bound
c  ENERG_HI    : upper energy bound
c  SPECRESP    : Spectral Response
c
c  Keywords read ...
c  TELESCOP : Mission/Telescope name, NOTE: If not present set to UNKNOWN
c  INSTRUME : Instrument/Detector name NOTE: If not present set to UNKNOWN
c  DETNAM   : Specific detector name NOTE: If not present set to NONE
c  FILTER   : Filter in use, if not present set to NONE
c  ARFVERSN : RMF version
c
c Passed Paramters
c  IUNIT         i   : FORTRAN unit number of open RMF file
c  CHATTER       i   : chattiness flag for o/p (0 quite,10 normal,>20 silly)
c  TELESCOP      o   : String listing telescope/mission
c  INSTRUME      o   : String listing instrument/detector
c  DETNAM        o   : String listing specific detector name
c  FILTER        o   : String listing instrument filter in use
c  AREA          o   : Area scaling factor
c  IEBOUND       o   : No. channels in the full array
c  CHANNEL       o   : Channel array
c  E_MIN         o   : Array containing min nominal energy bound to each chan
c  E_MAX         o   : Array containing max nominal energy bound to each chan
c  ARFVERSN      o   : ARF version
c  IERR          o   : Error Flag, ierr = 0 okay
c
c CALLED ROUTINES
c  subroutine FTMRHD     : (FITSIO) Move to extension
c  subroutine FTGKYS     : (FITSIO) Read FITS extension header keyword
c  subroutine FTGCNO     : (FITSIO) Get column number
c  subroutine FTGCVx     : (FITSIO) Read data in x format
c  subroutine WT_FERRMSG : (CALLIB) Dumps FITSIO Error message etc
c  subroutine GTCLAS     : (CALLIB) Read HDUVERS, HDUCLAS, etc.
c
c Compilation & Linking
c  link with FITSIO & CALLIB & FTOOLS
c
c Authors/Modification History:
c Ian M George (93 Nov 16) Original
c Jeff Guerber (1999-02-25) Use enull instead of inull (integer) for ftgcve
c       for specresp column nullval arg.  inull now unused, so removed it.
c Jeff Guerber (1999-04-05) Read HDUVERS in pref to HDUVERS2, using gtclas()
c
        character(7) version
        parameter (version = '1.1.0')
*-
c INTERNALS
        character(25) errstr, wrnstr
        character(30) comm
        character(70) desc, errinfo
        integer status, felem, frow, colnum, nclasn
        real enull
        character(30) extname, hduclass, hduclasn(9)
        logical anyflg, foundcol

c INITIALISATION
      ierr = 0
      status = 0
      errstr = ' RDARF1 '//version//' ERROR:'
      wrnstr = ' RDARF1 '//version//' WARNING:'
c
c --- USER INFO ---
c
      IF (chatter.GE.15) THEN
        desc = ' ... using RDARF1 ' // version
        call fcecho(desc)
      ENDIF
c
c --- READING DATA ---
c

c NAXIS2 ...

      status = 0
      call ftgkyj(iunit,'NAXIS2',iebound,comm,status)
      errinfo = errstr//' reading NAXIS2 value '
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        ierr = 4
        return
      ENDIF

c ARFVERSN ...

      status = 0
      ARfversn = '  '
      nclasn = 0
      call gtclas(iunit, 'ARFVERSN', extname, hduclass, hduclasn,
     &    nclasn, arfversn, status)
      errinfo = wrnstr//' reading HDUVERS/ARFVERSN'
      IF (chatter.GE.30) THEN
        call wt_ferrmsg(status,errinfo)
      ENDIF

c TELESCOP ...

      status = 0
      call ftgkys(iunit,'TELESCOP',telescop,comm,status)
      errinfo = wrnstr//' reading TELESCOP'
      call wt_ferrmsg(status,errinfo)
      IF (status.EQ.202) THEN
        telescop = 'UNKNOWN'
      ENDIF

c INSTRUME ...

      status = 0
      call ftgkys(iunit,'INSTRUME',instrume,comm,status)
      errinfo = wrnstr//' reading INSTRUME'
      call wt_ferrmsg(status,errinfo)
      IF (status.EQ.202) THEN
        instrume = 'UNKNOWN'
      ENDIF

c FILTER ...

      status = 0
      call ftgkys(iunit,'FILTER',filter,comm,status)
      errinfo = wrnstr//' reading FILTER'
      call wt_ferrmsg(status,errinfo)
      IF (status.EQ.202) THEN
        filter = 'NONE'
      ENDIF

c DETNAM ...

      status = 0
      call ftgkys(iunit,'DETNAM',detnam,comm,status)
      errinfo = wrnstr//' reading DETNAM'
      call wt_ferrmsg(status,errinfo)
      IF (status.EQ.202) THEN
        detnam = 'NONE'
      ENDIF


      frow = 1
      felem = 1
      enull = 0
c Find and Read ENERG_LO Column
      status = 0
      foundcol = .true.
      call ftgcno(iunit,.false.,'ENERG_LO',colnum,status)
      IF (status.NE.0) THEN
        foundcol = .false.
      ENDIF
      IF (.NOT.foundcol) THEN
        errinfo = errstr//' ENERG_LO COLUMN not present'
        call fcecho(errinfo)
        ierr = 2
        return
      ENDIF
c ... Reading
      status = 0
      call ftgcve(iunit,colnum,frow,felem,iebound,enull,e_lo,
     &            anyflg,status)
      IF (status.NE.0) THEN
        errinfo = errstr//' reading ENERG_LO column'
        call fcecho(errinfo)
        ierr = 3
        return
      ENDIF

c Find and Read ENERG_HI Column
      status = 0
      foundcol = .true.
      call ftgcno(iunit,.false.,'ENERG_HI',colnum,status)
      IF (status.NE.0) THEN
        foundcol = .false.
      ENDIF
      IF (.NOT.foundcol) THEN
        errinfo = errstr//' ENERG_HI COLUMN not present'
        call fcecho(errinfo)
        ierr = 2
        return
      ENDIF
c ... Reading
      status = 0
      call ftgcve(iunit,colnum,frow,felem,iebound,enull,e_hi,
     &            anyflg,status)
      IF (status.NE.0) THEN
        errinfo = errstr//' reading ENERG_HI column !'
        call fcecho(errinfo)
        ierr = 3
        return
      ENDIF


c Find and Read SPECRESP Column
      status = 0
      foundcol = .true.
      call ftgcno(iunit,.false.,'SPECRESP',colnum,status)
      IF (status.NE.0) THEN
        foundcol = .false.
      ENDIF
      IF (.NOT.foundcol) THEN
        errinfo = errstr//' SPECRESP COLUMN not present'
        ierr = 2
        return
      ENDIF
c ... reading
      status = 0
      call ftgcve(iunit,colnum,frow,felem,iebound,enull,sprsp,
     &            anyflg,status)
      IF (status.NE.0) THEN
        errinfo = errstr//' reading SPECRESP column'
        call fcecho(errinfo)
        ierr = 3
        return
      ENDIF


      IF (chatter.GE.20) THEN
        desc = '      ... read SPECRESP data '
        call fcecho(desc)
      ENDIF


      return
      end

c --------------------------------------------------------------------
