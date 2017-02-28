      subroutine matchext(lun,keys,numkeys,vals,numvals,hdunum,ierr)

      implicit none
c
c  Search open FITS file for HDU which contains keys with matching
c  value from vals.  keys and vals may contain wildcards.
c
c  I  lun     (i)  Input file lu
c  I  keys    (c)  Keyword search is based on these
c  I  numkeys (i)  Number of keyword candidates
c  I  vals    (c)  Possible matching values for keywords
c  I  numvals (i)  Number of value candidates
c  O  hdunum  (i)  Return the number for matched HDU
c                  If not find return -999.
c  O  ierr    (i)  Error flag (0=OK)
c
      integer lun, numkeys, numvals, hdunum, ierr
      character*(*) keys(*), vals(*)
c
c  Local variables
c
      logical found, match, exact
      integer ikey, ival, status
      integer curhdu, hdutype
      character(80) valbuf, comment
   
      found = .FALSE.
      hdunum = -999
      status = 0
      ierr = 0

c  Save current HDU

      call FTGHDN(lun, curhdu)

c  Loop through HDUs

      call FTMAHD(lun, 1, hdutype, status)

      do while ( status.eq.0 .and. .not.found )

c     Look for keywords

         ikey = 1
         do while ( ikey.le.numkeys .and. .not.found )

            call FTGKYS(lun,keys(ikey),valbuf,comment,status)

c        Check keyword value against list of possible values

            if ( status.eq.0 ) then
               ival = 1
               do while ( ival.le.numvals .and. .not.found ) 

                  call FTCMPS(vals(ival),valbuf,.FALSE.,match,exact)
                  if ( match ) then
                     found = .TRUE.
                     call FTGHDN(lun, hdunum)
                  endif
                  ival = ival + 1
               enddo

c           If key doesn't contain wildcards, go to next key

               call FTTKEY(keys(ikey), status)
               if ( status.eq.0 ) ikey = ikey + 1
            else
               ikey = ikey + 1
            endif
         enddo
         status = 0
         call FTMRHD(lun, 1, hdutype, status)
      enddo


c   Move to matched hdu, or return to original hdu if not found

      status = 0
      if ( hdunum.gt.0 ) then
         write(comment,'(a,i4)') 'Move to HDU#',hdunum
         call XWRITE(comment,25)
         call FTMAHD(lun, hdunum, hdutype, status)
         ierr = status
      else
         call FTMAHD(lun, curhdu, hdutype, status)
         ierr = -1
      endif

      return

      end
