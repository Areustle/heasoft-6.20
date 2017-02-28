      subroutine gt_gtis (Lun, Gtiext, Gtifile, Gtimode, Tmin, Tmax, 
     &                    Gtinum, P_gtistart, P_gtistop, Tontime,
     &                    Status)
      implicit none
      include '../include/io.inc'
      include '../include/maxvals.inc'
      include '../include/dynmem.inc'
c
c This routine distills user entered options, Tmin, Tmax, and
c Gtifile and event file's GTIs into a single list of GTIs, 
c represented as dynamically allocated start and stop arrays.  
c A Gtinum of 0 is returned if no such options have been set.
c
c  I  Lun        (c) Logical unit of open event file
c  I  Gtiext     (c) GTI extension within events file, e.g.
c                    GTI* OR(STDGTI,GTIALL) AND(GTI#)
c  I  Gtifile    (c) File(s) containing GTIs, e.g.
c                    a.gti AND(@list.txt) OR(b.gti,c.gti)
c  I  Gtimode    (c) Mode for merging Gtifile and event GTIs,
c                     (AND, OR, SUB, NONE)
c  I  Tmin       (d) Minimum time
c  I  Tmax       (d) Maximum time
c  O  Gtinum     (i) Number of output GTIs
c  O  P_gtistart (i) Pointer to first GTI start time
c  O  P_gtistop  (i) Pointer to first GTI stop time
c  O  Tontime    (d) Total ontime
c
      integer*4 Lun
      character*(*) Gtiext, Gtifile, Gtimode
      real*8 Tmin, Tmax, Tontime
      integer Gtinum, P_gtistart, P_gtistop, Status
c
c  Local variables
c
      integer i, gtilun, rwmode, svhdunum, extnum, hdunum, hdutype
      character(80) gtfuni, gteuni, extname, msg

      character(4) gmode, mgmode
      integer anum, p_astart, p_astop
      integer bnum, p_bstart, p_bstop
      integer onum, nloop, nmerge
      real*8 ostart(MAX_GTIS), ostop(MAX_GTIS), dd
      character*(MAX_FILELEN) nextfile, ds
      integer slen, gfflag, extflag, nfuncs, tmpstat, LENACT
      logical done, found, match, exact, ISDIGIT

      parameter (nfuncs=2)
      character(3) funcs(nfuncs)
      data funcs /'OR', 'AND'/

      Tontime = 0.
      Gtinum = 0
      anum = 0
      bnum = 0
      rwmode = 0
      gmode = Gtimode(1:4)
      Status = 0

      if ( gmode.eq.'NONE' ) then
         P_gtistart = 0
         P_gtistop = 0
         return
      endif

      if ( Gtifile.ne.' ' ) then
c
c  Read from GTI file(s)
c
         onum = 0
         anum = 0
         bnum = 0
         gfflag = 0
         call matchfunc(Gtifile, funcs, nfuncs, gfflag, Status)
         Status = 0
         call filefromlist(Gtifile, nextfile, MAX_FILELEN, Status)
         call getlun(gtilun)
         nloop = 0
         do while ( Status.eq.0 .and. nextfile.ne.' ' )
c
c         Only copy into gti buffer if merge is necessary
c 
            if ( nloop.eq.1 ) then
               onum = anum
               if ( anum.gt.MAX_GTIS ) then
                  call xwarn(' Maximum GTIs exceeded, truncating...',10)
                  onum = MAX_GTIS
               endif
               do i = 1, onum
                  ostart(i) = memd(p_astart+i-1)
                  ostop(i) = memd(p_astop+i-1)
               enddo
      
               call gtialloc(0, anum, p_astart, p_astop, Status)
            endif
            nloop = nloop + 1
c
c         Open gti file up to specified extension
c
            call ftnopn(gtilun, nextfile, rwmode, Status)
            if ( Status.eq.0 ) then
c
c         Read in gtis
c
               call rd_gti(gtilun,anum,p_astart,p_astop,gtfuni,Status)
               if ( Status.ne.0 ) then
                  write(ZWRite,'(2a)') ' Failed to read GTIs from ',
     &                           nextfile(:LENACT(nextfile))
                  call XWARN(ZWRite,10)
                  if ( gmode.eq.'SUB' ) then
                     call XWARN('Unable to substitute GTIs', 10)
                     gmode = 'AND'
                  endif
                  Status = 0
               endif
               mgmode = funcs(gfflag)
               if ( nloop.eq.1 ) then

                  write(ds, '(2a)') ' GTIFILE: ',
     &                              nextfile(:LENACT(nextfile))
                  call xwrite(ds, 15)

               else
c
c          Merge with previous
c
                  if ( gfflag.le.0 ) then
                     call xwrite(' Using OR to merge GTIFILE list...',
     &                            10)
                     gfflag = 1
                     mgmode = 'OR'
                  endif

                  write(ds, '(2a)') '        : ',
     &                              nextfile(:LENACT(nextfile))
                  ds(6:8) = mgmode(1:3)
                  call xwrite(ds, 15)

                  call gtimerge (mgmode, ostart, ostop, onum, 
     &                           MAX_GTIS, memd(p_astart),
     &                           memd(p_astop), anum, ostart, ostop,
     &                           onum, Status)
                  if ( Status.ne.0 ) then
                     call xwrite(' gt_gtis: Failed to merge GTIFILE',
     &                            10)
                     Status = 0
                  endif
                  call gtialloc(0, anum, p_astart, p_astop, Status)
                  Status = 0
               endif
               call ftclos(gtilun, Status)
            else
               call xwrite(' Failed to open GTI: ', 10)
               call xwrite(nextfile, 10)
               call ftgerr(Status, msg)
               call xwrite(msg, 15)
            endif
            call filefromlist(' ', nextfile, MAX_FILELEN, Status)
         enddo
         call frelun(gtilun)
         if ( Status.ne.0 ) then
            call xwrite(' Error in GTIFILE specification', 10)
            return
         endif
c
c      Save merged gtis into 'a' pointers
c
         if ( nloop.gt.1 ) then
            call gtialloc(1, onum, p_astart, p_astop, Status)
            if ( Status.eq.0 ) then
               anum = onum
               do i = 1, anum
                  memd(p_astart+i-1) = ostart(i)
                  memd(p_astop+i-1) = ostop(i)
               enddo
            else
               anum = 0
            endif
         endif

      endif
c
c  Read GTIs from event file
c
      if ( gtimode.ne.'SUB' ) then
         onum = 0
         extflag = 0
         if ( Gtiext.eq.' ' ) Gtiext = "*GTI*"
         call matchfunc(Gtiext, funcs, nfuncs, extflag, Status)
         Status = 0
         call filefromlist(Gtiext, nextfile, MAX_FILELEN, Status)
         call ftghdn(Lun, svhdunum)
         found = .FALSE.
         nmerge = 0
         do while ( Status.eq.0 .and. nextfile.ne.' ' )
c
c         Open gti file up to specified extension
c
            done = .FALSE.
            call ftmahd(Lun, 1, hdutype, Status)
            do while ( Status.eq.0 .and. .not.done ) 
               match = .FALSE.
               extname = ' '
               call ftgkys(Lun, "EXTNAME", extname, msg, Status)
               Status = 0
               if ( ISDIGIT(nextfile(1:1)) ) then
                  call strnum(nextfile, -4, dd, status)
                  extnum = int(dd)
                  call ftghdn(Lun, hdunum)
                  if ( extnum .eq. hdunum-1 ) then
                     match = .TRUE.
                  endif
               else
                  call ftcmps(nextfile, extname, .FALSE., match, exact)
               endif
               if ( match ) then
                  found = .TRUE.
                  if ( extflag.le.0 ) done = .TRUE.
c
c         Only copy into gti buffer if merge is necessary
c 
                  if ( nmerge.eq.1 ) then
                     onum = bnum
                     if ( bnum.gt.MAX_GTIS ) then
                        call xwarn(
     &                   ' Maximum GTIs exceeded, truncating...', 10)
                        onum = MAX_GTIS
                     endif
                     do i = 1, onum
                        ostart(i) = memd(p_bstart+i-1)
                        ostop(i) = memd(p_bstop+i-1)
                     enddo
                     call gtialloc(0, bnum, p_bstart, p_bstop, Status)
                  endif
c
c         Read in gtis
c
                  call rd_gti(Lun,bnum,p_bstart,p_bstop,gteuni,Status)
                  if ( Status.ne.0 ) then
                     write(ZWRite,'(2a)') ' Failed to read GTIs from ',
     &                              extname(:LENACT(extname))
                     call XWARN(ZWRite,10)
                  endif

                  if ( nmerge.eq.0 ) then

                     write(ds, '(2a)') '  GTIEXT: ',
     &                                 extname(:LENACT(extname))
                     call xwrite(ds, 15)

                  else
c
c          Merge with previous
c
                     mgmode = funcs(extflag)

                     write(ds, '(2a)') '        : ',
     &                                 extname(:LENACT(extname))
                     ds(6:8) = mgmode(1:3)
                     call xwrite(ds, 15)

                     call gtimerge (mgmode, ostart, ostop, onum,
     &                              MAX_GTIS, memd(p_bstart),
     &                              memd(p_bstop), bnum, ostart, ostop, 
     &                              onum, Status)
                     if ( Status.ne.0 ) then
                        call xwrite(
     &                    ' gt_gtis: Failed to merge internal GTIs', 10)
                     endif
                     call gtialloc(0, bnum, p_bstart, p_bstop, Status)
                     Status = 0
                  endif
                  nmerge = nmerge + 1
               endif
               call ftmrhd(Lun, 1, hdutype, Status)
            enddo
c
c          End of file error is ok, as it is a condition to stop loop
c
            if ( Status.eq.107 ) Status = 0
            call filefromlist(' ', nextfile, MAX_FILELEN, Status)
         enddo
         tmpstat = 0
         call ftmahd(Lun, svhdunum, hdutype, tmpstat)
         if ( .not.found ) then
            call xwrite(' No match for GTIEXT specification', 10)
            return
         endif
         if ( Status.ne.0 ) then
            call xwrite(' Error in GTIEXT specification', 10)
            return
         endif
c
c      Save merged gtis into 'b' pointers
c
         if ( nmerge.gt.1 ) then
            call gtialloc(1, onum, p_bstart, p_bstop, Status)
            if ( Status.eq.0 ) then
               bnum = onum
               do i = 1, bnum
                  memd(p_bstart+i-1) = ostart(i)
                  memd(p_bstop+i-1) = ostop(i)
               enddo
            else
               bnum = 0
            endif
         endif
      else
         bnum = 0
      endif

      Status = 0
c
c  Determine composite GTIs
c
      Status = 0
      if ( gmode.eq.' ' ) gmode = 'AND'

      if ( anum.eq.0 ) then
         Gtinum = bnum
         P_gtistart = p_bstart
         P_gtistop = p_bstop
      else if ( bnum.eq.0 ) then
         Gtinum = anum
         P_gtistart = p_astart
         P_gtistop = p_astop
      else
         if ( gtfuni.ne.gteuni ) then
            write(ZWRite,*) ' Incompatible GTI units: ',gtfuni,' ',
     &                      gteuni
            call RMVXBK(ZWRite(2:))
            call XWRITE(ZWRite,5)
            Status = 0
            call gtialloc(0, anum, p_astart, p_astop, Status)
            Status = 0
            call gtialloc(0, bnum, p_bstart, p_bstop, Status)
            Status = -1
            return
         endif

         call gtimerge (gmode, ostart, ostop, onum, MAX_GTIS,
     &                  memd(p_astart), memd(p_astop), anum, 
     &                  memd(p_bstart), memd(p_bstop), bnum, 
     &                  Status)

         Status = 0
         call gtialloc(0, anum, p_astart, p_astop, Status)
         Status = 0
         call gtialloc(0, bnum, p_bstart, p_bstop, Status)
         Status = 0
         call gtialloc(1, onum, P_gtistart, P_gtistop, Status)

         if ( Status.eq.0 ) then
            Gtinum = onum
            do i = 1, Gtinum
               memd(P_gtistart+i-1) = ostart(i)
               memd(P_gtistop+i-1) = ostop(i)
            enddo
         else
            Gtinum = 0
         endif
      endif
c
c Consider Tmin and Tmax
c
      if ( Tmax.gt.Tmin ) then
         if ( Gtinum.eq.0 ) then
            status = 0
            call gtialloc(1, 1, P_gtistart, P_gtistop, status)
            if ( status.eq.0 ) then
               Gtinum = 1
               memd(P_gtistart) = Tmin
               memd(P_gtistop) = Tmax
            endif
         else
            gmode = 'AND'
            onum = 1
            ostart(1) = Tmin
            ostop(1) = Tmax
            call gtimerge (gmode, ostart, ostop, onum, MAX_GTIS,
     &                     memd(P_gtistart), memd(P_gtistop), Gtinum, 
     &                     ostart, ostop, onum, status)
            status = 0
            call gtialloc(0, Gtinum, P_gtistart, P_gtistop, status)

            status = 0
            call gtialloc(1, onum, P_gtistart, P_gtistop, status)

            if ( status.eq.0 ) then
               Gtinum = onum
               do i = 1, Gtinum
                  memd(P_gtistart+i-1) = ostart(i)
                  memd(P_gtistop+i-1) = ostop(i)
               enddo
            else
               Gtinum = 0
            endif
         endif
      endif

      write (ZWRite,'(a,i8)') ' Final GTIs = ', Gtinum
      call RMVXBK(ZWRite)
      call XWRITE (ZWRite,15)

      do i = 0, Gtinum-1
         Tontime = Tontime + (memd(P_gtistop+i) - memd(P_gtistart+i))
      enddo
c
c the sum of the gti is used to calculate exposure in seconds
c if the column start and stop contains time in days the exposure
c need to be multiply by 86400 (this occurs with the COS-B data).
c
      if ( gtimode.eq.'SUB' ) then
         if ( gtfuni(1:1).eq.'D' ) Tontime = Tontime*86400.
      else
         if ( gteuni(1:1).eq.'D' ) Tontime = Tontime*86400.
      endif
     
      call xdstr(Tontime, 16, ds, slen)
      WRITE (ZWRite,99001) ds(1:slen) , Gtinum
      CALL XWRITE(ZWRite,15)

      return
98001 FORMAT (' GTI file ',a,' not found')
98002 FORMAT (' Using GTI file: ',a,' GTI mode: ',a)
99001 FORMAT (' Total gti exposure time ',a,' from',i6,' gti')
      end
