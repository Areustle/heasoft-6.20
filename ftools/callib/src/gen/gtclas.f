C  FTOOLs info $Header: /headas/headas/ftools/callib/src/gen/gtclas.f,v 3.3 2013/05/21 19:08:09 irby Exp $
c
c     SUBROUTINE GTCLAS: From the current HDU, return the HDUCLASn, HDUVERS,
c     and EXTNAME keywords.
c
c     Jeff Guerber, Raytheon ITSS / NASA GSFC code 664, Feb. 1999
c
c     NOTE ON HDUVERS:  Many files have a numbered series of HDUVERSn
c     keywords, giving a version number for each HDUCLASn.  However,
c     HFWG Recommendation R8 proposes a single HDUVERS keyword, and only
c     the version of the highest HDUCLASn can really be significant
c     anyway.  The CALLIB routines and the OGIP memos are being brought
c     into accord with R8.  Therefore, this subroutine will return HDUVERS
c     if found, or if not, then the highest-numbered HDUVERSn keyword.  If
c     neither are found, it will optionally look for the keyword specified
c     by the OLDVERKEY argument, which should be the old extension-type-
c     specific version keyword (eg. 'PHAVERSN', 'RMFVERSN', etc), or blank.
c
c     Blank strings are returned for missing keywords.
c
c     ARGUMENTS:
c         UNIT     (in)  integer  Fitsio unit number
c         OLDVERKEY(in)  char     Optional: If no HDUVERSn, try to read
c                                 a version identifier from this keyword
c                                 (eg, 'PHAVERSN'). Ignored if blank.
c         EXTNAME  (out) char     Value of EXTNAME keyword (' ' if missing)
c         HDUCLASS (out) char     Value of HDUCLASS keyword (' ' if missing)
c         HDUCLASN (out) char(9)  Array of HDUCLASn keywords, potentially
c                                 as many as 9
c         NCLASN   (out) integer  Number of HDUCLASn keywords found (0 - 9)
c         HDUVERS  (out) char     Value of HDUVERS or the highest HDUVERSn
c                                 or OLDVERKEY (' ' if none)
c         STATUS   (out) integer  Fitsio status
c     character(30) should be plenty large enough for the string arguments.
c
c     Modification History:
c     $Log: gtclas.f,v $
c     Revision 3.3  2013/05/21 19:08:09  irby
c     Change character*n to character(n) to silence warnings: "Obsolescent
c     feature: Old-style character length".
c
c     Revision 3.2  1999/03/09 03:25:02  guerber
c     Added argument OLDVERKEY, an optional keyword name of an alternate
c     version identifier to be read if HDUVERS and HDUVERSn are both missing.
c
c     Revision 3.1  1999/02/25 05:18:01  guerber
c     New subroutine.
c
c------------------------------------------------------------------------
c
      subroutine gtclas( unit, oldverkey, extname, hduclass, hduclasn,
     &    nclasn, hduvers, status )
c
c     ARGUMENTS
c
      implicit none
      integer  unit
      character*(*) oldverkey, extname, hduclass, hduclasn(9), hduvers
      integer  nclasn, status
c
c     INTERNAL VARIABLES
c
      character(30)  numvers(9)
      character(80)  comment, message
      integer  i, nfound
c
c-------------------------------------------------------------------------
c
      if (status .ne. 0) return

      nclasn = 0
      nfound = 0
      extname = ' '
      hduclass = ' '
      hduvers = ' '
      do i = 1, 9
          hduclasn(i) = ' '
          numvers(i) = ' '
      enddo
c
c     Read EXTNAME.  Not an error if missing.
c
      call ftgkys( unit, 'EXTNAME', extname, comment, status )
      if (status .eq. 202)  status = 0
      if (status .ne. 0) then
          message = 'gtclass: error reading EXTNAME keyword'
          goto 990
      endif
      call ftupch( extname )
c
c     Read HDUCLASS, HDUCLASn. Not an error if missing.
c
      call ftgkys( unit, 'HDUCLASS', hduclass, comment, status )
      if (status .eq. 202) status = 0
      call ftupch( hduclass )

      call ftgkns( unit, 'HDUCLAS', 1, 9, hduclasn, nclasn, status )
      if (status .ne. 0) then
          message = 'gtclas: error reading HDUCLASS/HDUCLASn keywords'
          goto 990
      endif
      do i = 1, nclasn
          call ftupch( hduclasn(i) )
      enddo
c
c     Try to read HDUVERS.  If not there, read HDUVERSn, take the highest.
c     Should be of form `1.2.3', so no upcasing is needed.
c     If neither is there and oldverkey was specified, try to read that
c     keyword (eg. PHAVERSN = '1992a').  If none are there, just return
c     blank hduvers.  status=202 is KEY_NO_EXIST.  ftgkns doesn't return
c     202 if none found, rather nfound=0.
c
      call ftgkys( unit, 'HDUVERS', hduvers, comment, status )
      if ( status .eq. 202 ) then
          status = 0
          call ftgkns( unit, 'HDUVERS', 1, 9, numvers, nfound, status )
          if ( status .eq. 0 .and. nfound .gt. 0 ) then
              hduvers = numvers(nfound)
          elseif ( status .eq. 0 .and. oldverkey .ne. ' ' ) then
              call ftgkys( unit, oldverkey, hduvers, comment, status )
              if (status .eq. 202) status = 0
          endif
      endif

      if (status .ne. 0) then
          message = 'gtclas: error reading HDUVERS/HDUVERSn/'
     &        // oldverkey
          goto 990
      endif

      return
c
c     Leave more extensive error reporting (eg fitsio stack dump) to the
c     caller.  (May want to rethink this.)  (Also, fcerr reqs. common/task/.)
c
  990 call fcerr(message)
      return
      end
