C**********************************************************************
C
C     SUBROUTINE:
C     access_caldb
C
C     DESCRIPTION:  This routine gets the caldb files using gtcalf.
C
C     MODIFICATIONS:
C         6/29/1998 Ning Gan:
C            Adopted the new fits format.
C            Changed the length of timeobs and timend  from 12 to 68.
C            Changed the length of dateobs and dateend from  8 to 68.
C         1998-07-13 Jeff Guerber: Look for DATE-, TIME- keywords first as
C             DATE-OBS, then as DATE_OBS, etc.  Untabified.
C
C     Note: The input data file (test_ANC.FITS) has INSTRUME=PSPC_
C     which applies for WTRANS, ENERGY_GRID, MATRIX and DET_EFF files.
C     However, for EFFAREA, SOLAR_GEOPHYS and RAYMOND_SPEC files the INSTRUME
C     keyword needs to be corrected as described below.
C     (ALSO TELESCOP keyword for SOLAR_GEOPHYS and RAYMOND_SPEC)
C
C     CALLED ROUTINES :
C     subroutine GTPARM       : (CALLIB) gets parameters from sssarf.par file
C     subroutine CGETLUN      : (CALLIB) get free FORTRAN logical unit no
C     subroutine FCECHO       : (FITSIO) write to standard o/p
c     subroutine FTMAHD       : (FITSIO) Move to an absolute xtens no.
c     subroutine FTOPEN       : (FITSIO) Open a FITS file
c     subroutine FTGKYx       : (FITSIO) gets a keyword of type "x"
C     subroutine CALDB_INFO   : (CALLIB) checks local CALDB available to user
C     for required mission/instrument
C     subroutine GTCALF       : (CALLIB) finds specific type of cal data set
C     subroutine FCPARS       : (CALLIB) parses a cal filename
C
C
C******************************************************************************

      subroutine access_caldb (infile, dataset, calfile, errstat)
      implicit none

C     internals
      character(80) context,comment
      character*(*) dataset, calfile, infile
      character(30) online,caldbl(2), calexpr
      character(20) telescop,instrume
c     hh:mm:ss.dddd
      character(68) timeobs,timeend
c     yyyy-mm-dd
      character(68) dateobs,dateend
      character(160) calfexp,caldbf(2)
      integer errstat,blksize,chatter,calextno,nret,nfound,
     &    hdtype, extnum, caldbe(2), funit
      logical inopen
C     initialize
      inopen = .false.
      errstat = 0
      extnum = 0
      calexpr = '-'


C     Get the calfexp parameter
      call uclgst('calfexp', calfexp, errstat)
      if(errstat.ne.0)then
          context = 'could not get calfexp parameter'
          call fcerr(context)
          goto 999
      endif

C     Get the chatter parameter
      call uclgsi('chatter', chatter, errstat)
      if(errstat.ne.0)then
          context = 'could not get chatter parameter'
          call fcerr(context)
          chatter=9
          context = ' setting chatter=9'
          call fcerr(context)
          goto 999
      endif

C     Open the input data FITS file
      call cgetlun(funit)
      call ftopen(funit,infile,0,blksize,errstat)
      if(errstat.ne.0) then
          context = 'access_caldb: unable to open infile: '//infile
          call wt_ferrmsg(errstat,context)
          return
      endif
      inopen = .true.

C     Move to 1st extension (assume first extension if none specified)
      if (extnum .eq. -99) extnum = 1
      call ftmahd(funit, extnum+1, hdtype, errstat)
      if(errstat.ne.0)then
          context = 'access_caldb: Unable to move to extension:'//infile
          call wt_ferrmsg(errstat,context)
          return
      endif

C     Read parameters from the input FITS file
      call ftgkys(funit, 'TELESCOP', telescop, comment, errstat)
      if(errstat.ne.0)then
          context = 'unable to get TELESCOP parameter: '//infile
          call wt_ferrmsg(errstat,context)
          return
      endif



      call ftgkys(funit, 'INSTRUME', instrume, comment, errstat)
C
C     For 'EFFAREA' fits file set INSTRUME='XRT'
      if (dataset .eq. 'EFFAREA') then
          instrume = 'XRT'
      endif
C     For 'SOLAR_GEOPHYS' fits file set INSTRUME='INS' and TELESCOP ='GEN'
      if (dataset .eq. 'SOLAR_GEOPHYS') then
          instrume = 'INS'
          telescop = 'GEN'
      endif
C
      if(errstat.ne.0)then
          context = 'unable to get INSTRUME parameter: '//infile
          call wt_ferrmsg(errstat,context)
          return
      endif

C     obtain observation start date and time
      call ftgkys(funit, 'DATE-OBS', dateobs, comment, errstat)
      if (errstat .ne. 0) then
          errstat = 0
          call ftgkys(funit, 'DATE_OBS', dateobs, comment, errstat)
          if (errstat .ne. 0) then
              context = 'unable to get DATE-OBS parameter: '//infile
              call wt_ferrmsg(errstat,context)
              return
          endif
      endif

      call ftgkys(funit, 'TIME-OBS', timeobs, comment, errstat)
      if (errstat .ne. 0) then
          errstat = 0
          call ftgkys(funit, 'TIME_OBS', timeobs, comment, errstat)
          if (errstat .ne. 0) then
              context = 'unable to get TIME-OBS parameter: '//infile
              call wt_ferrmsg(errstat,context)
              return
          endif
      endif

C     obtain observation end date and time
      call ftgkys(funit, 'DATE-END', dateend, comment, errstat)
      if (errstat .ne. 0) then
          errstat = 0
          call ftgkys(funit, 'DATE_END', dateend, comment, errstat)
          if (errstat .ne. 0) then
              context = 'unable to get DATE-END parameter: '//infile
              call wt_ferrmsg(errstat,context)
              return
          endif
      endif

      call ftgkys(funit, 'TIME-END', timeend, comment, errstat)
      if(errstat.ne.0)then
          errstat = 0
          call ftgkys(funit, 'TIME_END', timeend, comment, errstat)
          if (errstat .ne. 0) then
              context = 'unable to get TIME-END parameter: '//infile
              call wt_ferrmsg(errstat,context)
              return
          endif
      endif

C     check whether CALDB access software is to be used
      if ((calfexp(1:5).eq.'CALDB').or.(calfexp(1:5).eq.'caldb')) then
          call caldb_info(chatter,'INST',telescop,instrume,errstat)
          if (errstat.ne.0) then
              context ='CALDB not defined/available'
              call fcecho(context)
              context='task requires CALDB to be both defined '//
     &            '& available in order to run'
              call wt_ferrmsg(errstat,context)
              goto 999
          endif
      endif

C-----------
C     Note: for dataset==MATRIX, there are two caldb files.
C     caldb/data/rosat/pspc/cpf/matrices/pspcb_gain2_256.rmf and
C     pspcb_gain2_34.rmf
C     Hence, use the following condition to extract the _256_ file.
      if (dataset .eq. 'MATRIX') then
          calexpr = 'DETCHANS.eq.256'
      endif
C-----------

C     Access CALDB to extract the file
      call gtcalf(chatter,telescop,instrume, '-', '-',dataset,
     &    dateobs,timeobs,dateend,timeend, calexpr,2,
     &    caldbf, caldbe,caldbl,nret,nfound,errstat)


      calfile = caldbf(1)
      calextno = caldbe(1)
      online = caldbl(1)
      if (errstat.ne.0) then
          context = 'problem obtaining valid  dataset'
          call fcecho(context)
          goto 999
      elseif (nfound .gt. 1) then
          context = 'more than one valid dataset found'
          call fcecho(context)
          context = 'unable to determine which to use -- aborting'
          errstat=1
          call wt_ferrmsg(errstat,context)
          goto 999
      endif


  999 continue
      if (errstat .ne. 0) then
          call fcerrm(errstat)
          errstat = 0
C         stop
      endif
      if ( inopen ) then
          call ftclos(funit,errstat)
          errstat = 0
      endif

      return
      end
