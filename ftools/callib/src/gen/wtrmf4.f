*+WTRMF4
      SUBROUTINE WTRMF4(Ounit,Chatter,Nk_hist,Hist,Nk_comm,Comment,
     &                  Rmfversn,Hduclas3,Telescop,Instrume,Detnam,
     &                  Filter,Areascal,Chantype,Flchan,Numelt,
     &                  Nchan,Nenerg,Numgrp,Energ_lo,Energ_hi,
     &                  Ngrp,F_chan,N_chan,Qorder,Order,Fmatrix,
     &                  Lo_thresh,Ierr)
 
      IMPLICIT NONE
      INTEGER Nenerg , Numelt, Numgrp
      INTEGER Chatter , Ierr
      INTEGER Ounit , Nk_hist , Nk_comm
      INTEGER Nchan , Flchan
      INTEGER Ngrp(Nenerg) , F_chan(Numgrp)
      INTEGER N_chan(Numgrp) , Order(Numgrp)
      REAL Areascal , Lo_thresh
      REAL Energ_lo(Nenerg) , Energ_hi(Nenerg)
      REAL Fmatrix(Numelt)
      CHARACTER*(*) Chantype
      CHARACTER*(*) Rmfversn
      CHARACTER*(*) Telescop , Instrume , Detnam , Filter
      CHARACTER*(*) Hduclas3
      CHARACTER*(*) Hist(*) , Comment(*)
      LOGICAL Qorder
 
c
c Description:
c  Creates and Writes the RMF extension for an RMF file one of the formats
c  conforming to the HDUVERS='1.*.*' family.
c Currently the following formats are supported (see OGIP/92-002a)
c   HDUVERS2 = '1.0.0'
c   HDUVERS2 = '1.1.0'
c   HDUVERS2 = '1.2.0'
c   HDUVERS  = '1.3.0'
c but HDUVERS2 = '1.0.0' or '1.1.0' or '1.2.0' will be overridden such 
c that HDUVERS = '1.3.0' is written.
c  Assumes the FITS file is open and has had the Primary Header written
c  !!! Note !!!! File is left open at the end
c      and  MUST BE CLOSED               by FTCLOS
c      or   ANOTHER EXTENSION ADDED      by FTCRHD
c  in order to (automatically) write the mandatory END header keyword.
c  For The matrix will be written as a VARIABLE LENGTH ARRAY if the reduction
c  in storage requirements (ie total number of stroed values) exceeds a
c  factor 3.0 over that obtained using a FIXED length array.
c
c Passed parameters
c  OUNIT         i   : FORTRAN unit number of open RMF file
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  NK_HIST       i   : No. records to be written as HISTORY records
c  HIST          i   : Array of history strings to be written
c  NK_COMM       i   : No. records to be written as COMMENT records
c  COMMENT       i   : Array of comment strings to be written
c  RMFVERSN      i   : String denoting OGIP HDUVERS family
c  HDUCLAS3      i   : String containing HDUCLAS3 value
c  TELESCOP      i   : String listing telescope/mission
c  INSTRUME      i   : String listing instrument/detector
c  DETNAM        i   : String listing specific detector name
c  FILTER        i   : String listing instrument filter in use
c  AREASCAL      i   : Area scaling factor
c  CHANTYPE      i   : Type of detector channels in use (PHA, PI)
c  FLCHAN        i   : Lowest legal channel for this detector
c  NUMELT        i   : No. response matrix elements
c  NCHAN         i   : No. channels in the full array
c  NENERG        i   : No. energy bins
c  NUMGRP        i   : No. response groups
c  ENERG_LO      i   : Array containing lower bound to each energy bin
c  ENERG_HI      i   : Array containing upper bound to each energy bin
c  NGRP          i   : Array containing no. channel subsets at each energy
c  F_CHAN        i   : Array containing 1st chan of each subset at each energy
c  N_CHAN        i   : Array containing no. chans within each subset
c                           at each energy
c  QORDER        i   : If true order information will be written
c  ORDER         i   : Grating order to which the response group belongs
c  FMATRIX       i   : Array containing the full matrix
c  LO_THRESH     i   : The lower threshold used to construct the matrix
c  IERR          o   : Error flag (0 = OK)
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
c  Alan Smale       (1992 Sept/Oct), original BBRSP version
c  Ian M George     (1.0.1; 1992 Dec 29), tidied-up version
c  Ian M George     (1.0.2; 1993 Feb 28), minor debugging of History records
c  Ian M George     (1.0.3; 1993 May 19), hist & comment made chara*70
c  Rehana Yusaf     (1.0.4; 1993 July 27), array dimensions changed from
c                                         hard coded to using new arguments
c                                         MAXCHAN and MAXEN
c  Ian M George     (1.1.0; 1993 Jul 30), cosmetics
c  Ian M George     (2.0.0; 1993 Sep 03), added variable length arrays
c  Ian M George     (2.1.0; 1993 Oct 12), added HDUCLASn stuff
c  Ian M George     (3.0.0: 1993 Oct 13), renamed from wt_rmf1992a & major
c                                       overhaul of HDUCLAS/VERS stuff
c  Ian M George     (3.0.1: 1993 Dec 01), xtra checks before var length array
c  Ian M George     (3.1.0: 1994 Jan 24), added varidat for var length arrays
c  Ian M George     (3.2.0: 1994 Jun 27), made passed chars free form
c  Ian M George     (1.0.0: 1995 Nov 22), copied from wtrmf1 (v3.2.0), but
c                                         added chantype as passed parameter
c  Ian M George     (1.0.0:96 Oct 04) copied from wtrmf2 (v1.0.0), to support
c                               HDUVERS2 = '1.2.0' (and flchan added
c                               as passed parameter)
c
c Banashree M Seifert (1.1.0 Oct 10,1996)
c         . internal formatted write i# instead of i
c
c Banashree M Seifert (1.2.0 Nov 13, 1996)
c          . introduced istart, istop
c            this is needed due to the fact that when first channel
c            flchan=0, then index for channel no. should start from 1
c            and not from 0
c
c Banashree M Seifert (1.3.0 Nov 21, 1996)
c          . format in internal writing of wrtstr changed from
c            i2 to i4
c kaa (1.4.0 Dec 17, 1998)
c          . uses compressed matrix and replaces HDUVERS2 with HDUVERS
c            added support for version 1.3.0 of the file format with optional
c            grating order information and NUMGRP & NUMELT keywords. added
c            option to use variable length arrays on F_CHAN and N_CHAN columns.
c --------------------------------------------------------------------------
      character(7) VERSION
      PARAMETER (VERSION='1.4.0')
*-
c Internals
      character(6) SUBNAME
      PARAMETER (SUBNAME='wtrmf4')
      INTEGER status , DECIMALS , itemp
      INTEGER MAXFLD , nvar , sum , nfixed
      INTEGER i , kk , siz_mat , siz_ngrp , ielt, igrp
      INTEGER ie , jj , varidat , nfields
      PARAMETER (MAXFLD=7,DECIMALS=6)
      character(16) ttype(MAXFLD) , tform(MAXFLD) , tunits(MAXFLD)
      character(5) hduvers
      character(70) string
      character(25) wrtstr
      character(80) message
      LOGICAL qvrmat, qvrchn
c Initialize
      Ierr = 0
      status = 0
      qvrmat = .FALSE.
      qvrchn = .FALSE.
 
c User info, if requested
      message = ' using '//SUBNAME//' '//VERSION
      CALL WTINFO(Chatter,15,1,message)
 
c Check for sillies
      IF ( Rmfversn(1:1).NE.'1' ) THEN
         message = 'Format/subroutine mismatch'
         CALL WTERRM(SUBNAME,VERSION,message)
         message = ' This routine writes only the 1.*.* family'//
     &             ' of formats'
         CALL WTINFO(Chatter,1,1,message)
         message = ' requested format: '//Rmfversn
         CALL WTINFO(Chatter,1,2,message)
         Ierr = 15
         GOTO 100
      ENDIF
c Check that we know the format, and override if an old format
      IF ( Rmfversn.EQ.'1.3.0' ) THEN
         hduvers = Rmfversn
      ELSEIF ( (Rmfversn.EQ.'1.0.0') .OR. (Rmfversn.EQ.'1.1.0') .OR.
     &         (Rmfversn.EQ.'1.2.0') ) THEN
         hduvers = '1.3.0'
         message = SUBNAME//' Old format requested: '//Rmfversn
         CALL WTINFO(Chatter,1,2,message)
         message = 'Resetting format (HDUVERS) to '//hduvers
         CALL WTINFO(Chatter,1,2,message)
      ELSE
         hduvers = '1.3.0'
         message = 'Unknown format: '//Rmfversn
         CALL WTERRM(SUBNAME,VERSION,message)
         message = 'Resetting format (HDUVERS) to '//hduvers
         CALL WTINFO(Chatter,1,2,message)
      ENDIF
 
c Create a new extension
      CALL FTCRHD(Ounit,status)
      IF ( status.NE.0 ) THEN
         CALL WTFERR(SUBNAME,VERSION,status,
     &               'Problem creating new extension')
         Ierr = 1
         GOTO 100
      ELSE
         CALL WTINFO(Chatter,15,1,'new extension created')
      ENDIF
 
c Calculate the necessary dimensions of the arrays (columns)
      siz_ngrp = 0
      siz_mat = 0
      nvar = 0
      igrp = 1
      DO ie = 1 , Nenerg
         siz_ngrp = MAX(siz_ngrp,Ngrp(ie))
         sum = 0
         DO jj = 1 , Ngrp(ie)
            sum = sum + N_chan(igrp+jj-1)
            nvar = nvar + sum
         ENDDO
         siz_mat = MAX(siz_mat,sum)
         igrp = igrp + Ngrp(ie)
      ENDDO

c Decide whether variable length arrays for the F_CHAN, N_CHAN, and MATRIX
c columns make sense.
      
      varidat = 0
      nfixed = siz_ngrp * Nenerg
      IF ( (siz_ngrp.GT.3) .AND. (igrp.NE.nfixed) .AND.
     &     (igrp.LE.3*nfixed) ) THEN
         qvrchn = .TRUE.
         varidat = varidat + 4 * igrp
      ENDIF

      nfixed = siz_mat*Nenerg
      IF ( (siz_mat.GT.3) .AND. (nvar.NE.nfixed) .AND. 
     &     (nvar.LE.3*nfixed) ) THEN
         qvrmat = .TRUE.
         varidat = varidat + 4*nvar
      ENDIF
 
c Dump to user if requested
      WRITE (message,'(a,i8)') 
     &                    ' Max # elements in F_CHAN & N_CHAN arrays = '
     &                    , siz_ngrp
      CALL WTINFO(Chatter,15,1,message)
      WRITE (message,'(a,i8)') ' Max # elements in MATRIX array = ' , 
     &                         siz_mat
      CALL WTINFO(Chatter,15,1,message)
      IF ( qvrmat ) THEN
         message = 'Using Variable length array '//'for MATRIX column'
      ELSE
         message = 'Using Fixed length array '//'for MATRIX column'
      ENDIF
      CALL WTINFO(Chatter,15,1,message)
 
c Set up the columns n stuff
      ttype(1) = 'ENERG_LO'
      tform(1) = 'E'
      tunits(1) = 'keV'
      ttype(2) = 'ENERG_HI'
      tform(2) = 'E'
      tunits(2) = 'keV'
      ttype(3) = 'N_GRP'
      tform(3) = 'I'
      tunits(3) = ' '
      ttype(4) = 'F_CHAN'
      IF ( qvrchn ) THEN
         wrtstr = 'PI'
      ELSE
         WRITE (wrtstr,'(i8,a)') siz_ngrp , 'I'
         CALL CRMVLBK(wrtstr)
      ENDIF
      tform(4) = wrtstr(1:10)
      tunits(4) = ' '
      ttype(5) = 'N_CHAN'
      tunits(5) = ' '
      tform(5) = tform(4)
      ttype(6) = 'MATRIX'
      IF ( qvrmat ) THEN
         wrtstr = 'PE'
      ELSE
         WRITE (wrtstr,'(i8,a)') siz_mat , 'E'
         CALL CRMVLBK(wrtstr)
      ENDIF
      tform(6) = wrtstr(1:10)
      tunits(6) = ' '
      nfields = 6
      IF ( Qorder ) THEN
         ttype(7) = 'ORDER'
         tform(7) = tform(4)
         tunits(7) = ' '
         nfields = nfields + 1
      ENDIF
c Write the required header keywords
      IF ( Hduclas3.EQ.'REDIST' ) THEN
         string = 'MATRIX'
      ELSE
         string = 'SPECRESP MATRIX'
      ENDIF
 
      CALL FTPHBN(Ounit,Nenerg,nfields,ttype,tform,tunits,string,
     &            varidat,status)
      IF ( status.NE.0 ) THEN
         message = 'Problem writing header keywords'
         CALL WTFERR(SUBNAME,VERSION,status,message)
         Ierr = 1
         GOTO 100
      ELSE
         message = 'written the extension header keywords'
         CALL WTINFO(Chatter,15,1,message)
      ENDIF
 
c New for HDUVERS 1.2.0
      CALL FTPKYJ(Ounit,'TLMIN4 ',Flchan,
     &            'Minimuum value legally allowed in column 4',status)
      IF ( status.NE.0 ) THEN
         CALL WTFERR(SUBNAME,VERSION,status,
     &               'Problem writing TLMIN4 keyword')
         status = 0
      ENDIF
 
      CALL FTPKYJ(Ounit,'TLMAX4 ',Flchan+Nchan-1,
     &            'Maximum value legally allowed in column 4',status)
      IF ( status.NE.0 ) THEN
         CALL WTFERR(SUBNAME,VERSION,status,
     &               'Problem writing TLMAX4 keyword')
         status = 0
      ENDIF
 
 
 
c WRITE THE HDUCLASn & HDUVERS keywords
      CALL FTPKYS(Ounit,'HDUCLASS ','OGIP',
     &            'format conforms to OGIP standard',status)
      IF ( status.NE.0 ) THEN
         CALL WTFERR(SUBNAME,VERSION,status,
     &               'Problem writing HDUCLASS keyword')
         status = 0
      ENDIF
 
      CALL FTPKYS(Ounit,'HDUCLAS1 ','RESPONSE',
     &            'dataset relates to spectral response',status)
      IF ( status.NE.0 ) THEN
         CALL WTFERR(SUBNAME,VERSION,status,
     &               'Problem writing HDUCLAS1 keyword')
         status = 0
      ENDIF
 
      CALL FTPKYS(Ounit,'HDUCLAS2 ','RSP_MATRIX',
     &            'dataset is a spectral response matrix',status)
      IF ( status.NE.0 ) THEN
         CALL WTFERR(SUBNAME,VERSION,status,
     &               'Problem writing HDUCLAS2 keyword')
         status = 0
      ENDIF
 
      IF ( Hduclas3.EQ.'REDIST' ) THEN
         string = 'photon redistribution matrix (only)'
      ELSEIF ( Hduclas3.EQ.'FULL' ) THEN
         string = 'convolved w/ all effects (det + optics)'
      ELSEIF ( Hduclas3.EQ.'DETECTOR' ) THEN
         string = 'convolved w/ detector effects (only)'
      ELSE
         string = 'WARNING This is NOT an OGIP-approved value'
      ENDIF
      CALL FTPKYS(Ounit,'HDUCLAS3 ',Hduclas3,string,status)
      IF ( status.NE.0 ) THEN
         CALL WTFERR(SUBNAME,VERSION,status,
     &               'Problem writing HDUCLAS3 keyword')
         status = 0
      ENDIF
 
      CALL FTPKYS(Ounit,'HDUVERS ',hduvers,
     &            'Version of format (OGIP memo CAL/GEN/92-002a)',
     &            status)
      IF ( status.NE.0 ) THEN
         CALL WTFERR(SUBNAME,VERSION,status,
     &               'Problem writing HDUVERS keyword')
         status = 0
      ENDIF
 
      CALL FTPKYS(Ounit,'HDUVERS1 ','1.0.0',
     &            'Obsolete - included for backwards compatibility',
     &            status)
      IF ( status.NE.0 ) THEN
         CALL WTFERR(SUBNAME,VERSION,status,
     &               'Problem writing HDUVERS1 keyword')
         status = 0
      ENDIF
 
      CALL FTPKYS(Ounit,'HDUVERS2 ',hduvers,
     &            'Obsolete - included for backwards compatibility',
     &            status)
      IF ( status.NE.0 ) THEN
         CALL WTFERR(SUBNAME,VERSION,status,
     &               'Problem writing HDUVERS2 keyword')
         status = 0
      ENDIF

c Add the other (passed) OGIP required keywords
      CALL FTPKYS(Ounit,'TELESCOP ',Telescop,'mission/satellite name',
     &            status)
      IF ( status.NE.0 ) THEN
         CALL WTFERR(SUBNAME,VERSION,status,
     &               'Problem writing TELESCOP keyword')
         status = 0
      ENDIF
 
      CALL FTPKYS(Ounit,'INSTRUME ',Instrume,'instrument/detector name',
     &            status)
      IF ( status.NE.0 ) THEN
         CALL WTFERR(SUBNAME,VERSION,status,
     &               'Problem writing INSTRUME keyword')
         status = 0
      ENDIF
 
      IF ( Detnam.NE.' ' ) THEN
         CALL FTPKYS(Ounit,'DETNAM ',Detnam,
     &               'specific detector name in use',status)
         IF ( status.NE.0 ) THEN
            CALL WTFERR(SUBNAME,VERSION,status,
     &                  'Problem writing DETNAM keyword')
            status = 0
         ENDIF
      ENDIF
 
      CALL FTPKYS(Ounit,'FILTER   ',Filter,'filter in use',status)
      IF ( status.NE.0 ) THEN
         CALL WTFERR(SUBNAME,VERSION,status,
     &               'Problem writing FILTER keyword')
         status = 0
      ENDIF
 
      CALL FTPKYJ(Ounit,'DETCHANS ',Nchan,
     &            'total number of detector channels',status)
      IF ( status.NE.0 ) THEN
         CALL WTFERR(SUBNAME,VERSION,status,
     &               'Problem writing DETCHANS keyword')
         status = 0
      ENDIF

c New for HDUVERS = 1.3.0 are the NUMGRP and NUMELT keywords giving the
c total number of response groups and response elements. Enables s/w
c reading the file to allocate enough memory.

      CALL FTPKYJ(Ounit,'NUMGRP  ',Numgrp,
     &            'total number of response groups',status)
      IF ( status.NE.0 ) THEN
         CALL WTFERR(SUBNAME,VERSION,status,
     &               'Problem writing NUMGRP keyword')
         status = 0
      ENDIF

      CALL FTPKYJ(Ounit,'NUMELT  ',Numelt,
     &            'total number of response elements',status)
      IF ( status.NE.0 ) THEN
         CALL WTFERR(SUBNAME,VERSION,status,
     &               'Problem writing NUMELT keyword')
         status = 0
      ENDIF
 
      CALL FTPKYS(Ounit,'CHANTYPE',Chantype,
     &            'Detector Channel Type in use (PHA or PI)',status)
      IF ( status.NE.0 ) THEN
         CALL WTFERR(SUBNAME,VERSION,status,
     &               'Problem writing CHANTYPE keyword')
         status = 0
      ENDIF
 
      CALL FTPKYF(Ounit,'LO_THRES',Lo_thresh,DECIMALS,
     &            'lower threshold for stored matrix',status)
      IF ( status.NE.0 ) THEN
         CALL WTFERR(SUBNAME,VERSION,status,
     &               'Problem writing LO_THRES keyword')
         status = 0
      ENDIF
 
      CALL FTPKYF(Ounit,'EFFAREA ',Areascal,DECIMALS,
     &            'Area scaling factor',status)
      IF ( status.NE.0 ) THEN
         CALL WTFERR(SUBNAME,VERSION,status,
     &               'Problem writing EFFAREA keyword')
         status = 0
      ENDIF
 
c Add other advised keywords
      CALL FTPKYS(Ounit,'RMFVERSN ','1992a',
     &            'OGIP classification of FITS format',status)
      IF ( status.NE.0 ) THEN
         CALL WTFERR(SUBNAME,VERSION,status,
     &               'Problem writing RMFVERSN keyword')
         status = 0
      ENDIF
 
      CALL WTINFO(Chatter,15,1,'written the OGIP required keywords')
 
 
c Add the (passed) history cards, adding one related to this programme
      itemp = 0
      DO i = 1 , Nk_hist
         CALL FTPHIS(Ounit,Hist(i),status)
         IF ( status.NE.0 ) THEN
            itemp = status
            status = 0
            CALL FTPHIS(Ounit,
     &                  ' - (missing record) fitsio illegal character ?'
     &                  ,status)
         ENDIF
      ENDDO
      WRITE (string,'(4a)') 'RSP_MATRIX extension written by ' , 
     &                      SUBNAME , ' ' , VERSION
      CALL FTPHIS(Ounit,string,status)
      IF ( itemp.NE.0 ) THEN
         CALL WTFERR(SUBNAME,VERSION,itemp,
     &               'Problem writing at least one History record')
      ELSE
         CALL WTINFO(Chatter,15,1,'written the history keywords')
      ENDIF
      status = 0
 
c Add the (passed) comment cards
      itemp = 0
      DO i = 1 , Nk_comm
         CALL FTPCOM(Ounit,Comment(i),status)
         IF ( status.NE.0 ) THEN
            itemp = status
            status = 0
            CALL FTPCOM(Ounit,
     &                  ' - (missing record) fitsio illegal character ?'
     &                  ,status)
         ENDIF
      ENDDO
      IF ( itemp.NE.0 ) THEN
         CALL WTFERR(SUBNAME,VERSION,itemp,
     &               'Problem writing at least one COMMENT record')
      ELSE
         CALL WTINFO(Chatter,15,1,'written the comment keywords')
      ENDIF
      status = 0
 
c Define the extension data structure
      CALL FTBDEF(Ounit,nfields,tform,varidat,Nenerg,status)
      IF ( status.NE.0 ) THEN
         CALL WTFERR(SUBNAME,VERSION,itemp,
     &               'Problem defining Data Structure')
         Ierr = 1
         GOTO 100
      ELSE
         CALL WTINFO(Chatter,15,1,'defined the data structure')
      ENDIF
 
c Write the data
      ielt = 1
      igrp = 1
      DO ie = 1 , Nenerg
 
c             ... the energy bin
         CALL FTPCLE(Ounit,1,ie,1,1,Energ_lo(ie),status)
         CALL FTPCLE(Ounit,2,ie,1,1,Energ_hi(ie),status)
 
c            ... the Grping info
         CALL FTPCLJ(Ounit,3,ie,1,1,Ngrp(ie),status)

         CALL FTPCLJ(Ounit,4,ie,1,Ngrp(ie),F_chan(igrp),status)
         CALL FTPCLJ(Ounit,5,ie,1,Ngrp(ie),N_chan(igrp),status)
 
c            ... the matrix elements
         kk = 0
         DO jj = 1 , Ngrp(ie)
            kk = kk + N_chan(igrp+jj-1)
         ENDDO
         CALL FTPCLE(Ounit,6,ie,1,kk,Fmatrix(ielt),status)

c            ... the optional order info (new for HDUVERS=1.3.0)
         IF ( Qorder ) THEN
            CALL FTPCLJ(Ounit,7,ie,1,Ngrp(ie),Order(igrp),status)
         ENDIF

c Update the pointers
         igrp = igrp + Ngrp(ie)
         ielt = ielt + kk

      ENDDO
 
c Final check for errors
      IF ( status.NE.0 ) THEN
         CALL WTFERR(SUBNAME,VERSION,itemp,'writing the data')
         Ierr = 1
      ENDIF
 
 
 100  IF ( Ierr.NE.0 ) THEN
         CALL WTERRM(SUBNAME,VERSION,'Fatal - aborting')
      ELSE
         CALL WTINFO(Chatter,15,1,
     &               'successfully written RSP_MATRIX data')
      ENDIF
 
      RETURN
      END
 
