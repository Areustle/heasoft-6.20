      subroutine chmdb(cmdid, status)
      implicit none
c
c Manipulate internal mission database
c
c  I  cmdid    (i)  Command id
c  O  status   (i)  Error flag (0=OK)
c
      integer*4 cmdid, status

      include '../include/maxvals.inc'
      include '../include/io.inc'
      include '../include/startup.inc'
      include '../include/sitedef.inc'
c
c  Local variables
c
      INTEGER*4 argc
      LOGICAL*4 replace, showall, list, copy, add, reset
      character*(MAX_IDSTR) mapid

      character*(MAX_FILELEN) infile, outfile
      character(80) tel, inst, det
      character(80) totel, toinst, todet
      character(10) key
      character(80) val, sval
      character(1) type
      integer*4 itel, i, klen, EDETIDX, DETIDX, LENACT
      integer*4 imtel, iminst, imdet, ival
      logical there
      real*8 dval

      add = .FALSE.
      showall = .FALSE.
      LIST = .FALSE.
      replace = .FALSE.
      infile = ' '
      outfile = ' '
      tel = ' '
      inst = ' '
      det = ' '
      key = ' '
      val = ' '
      copy = .FALSE.
      totel = ' '
      toinst = ' '
      todet = ' '
      reset = .FALSE.

      Status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARL(Cmdid,'SHOWALL',showall,Status)
      CALL GPARL(Cmdid,'LIST',list,Status)
      CALL GPARL(Cmdid,'REPLACE',replace,Status)
      CALL GPARS(Cmdid,'TELESCOP',tel,Status)
      CALL GPARS(Cmdid,'INSTRUME',inst,Status)
      CALL GPARS(Cmdid,'DETNAM',det,Status)
      CALL GPARS(Cmdid,'KEY',key,Status)
      CALL GPARS(Cmdid,'VALUE',val,Status)
      CALL GPARL(Cmdid,'ADD',add,Status)
      CALL GPARL(Cmdid,'COPY',copy,Status)
      CALL GPARS(Cmdid,'TOTELESCOP',totel,Status)
      CALL GPARS(Cmdid,'TOINSTRUME',toinst,Status)
      CALL GPARS(Cmdid,'TODETNAM',todet,Status)
      CALL GPARS(Cmdid,'INFILE',infile,Status)
      CALL GPARS(Cmdid,'OUTFILE',outfile,Status)
      CALL GPARL(Cmdid,'RESET',reset,Status)
      if ( status.ne.0 ) return

      if ( showall ) then
         infile = ' '
         call wrmdb(infile, 0, status)
         return
      endif

      if ( reset ) then
         infile = 'ximage.mdb'
         call PTEND(CXAn,CMAn,infile)
         replace = .TRUE.
      endif

      if ( outfile.ne.' ' ) then
         call qustrip(outfile)
         if ( tel.ne.' ' .and. inst.ne.' ' ) then
            itel = DETIDX(tel,inst,det)
         else
            itel = 0
         endif
         call wrmdb(outfile, itel, status)
         return
      endif

      if ( replace ) then
         if ( infile.eq.' ' ) then
            call XWRITE(' No file given to replace current mdb', 10)
            status = -1
            return
         else
            call initmdb
         endif
      endif

      if ( infile.ne.' ' ) then
         call qustrip(infile)
         call XWRITE(' Reading from mission file... ', 10)
         call XWRITE(infile, 15)
         call rdmdb(infile, status)
         return
      endif

      if ( add ) then
         if ( tel.eq.' ' .or. inst.eq.' ' ) then
            call XWRITE(' TELESCOP and INSTRUME must be specified', 10)
            status = -1
            return
         endif
         itel = EDETIDX(tel, inst, det)
         if ( itel.gt.0 ) then
            call XWARN(' Mission already exists', 10)
            status = -1 
            return
         endif
         call XWRITE(' Adding new mission...', 10)
         call addmdb(tel, inst, det, itel, status)
         if ( status.ne.0 ) return
         if ( val.eq.' ' .and. key.eq.' ' ) return
      endif
c
c  If current mission, assume it for tel, inst, det vals
c    if not already set
c
      if ( .not.list .and. tel.eq.' ' .and. inst.eq.' ' ) then
         mapid = 'CUR'
         call get_itel(mapid, itel)
         if ( itel.gt.0 ) then
            call xwrite(' Assuming current mission for TELESCOP,'//
     &                  ' INSTRUME, DETNAM', 10)
            tel = ZTElescop(itel)
            inst = ZINstrume(itel)
            det = ZDEtnam(itel)
         else
            list = .TRUE.
         endif
      endif
c
c  Copy mission
c
      if ( copy ) then
         if ( tel.eq.' ' .or. inst.eq.' ' .or. 
     &        totel.eq.' ' .or. toinst.eq.' ' ) then
            call XWRITE(' The COPY function requires at least'//
     &          ' TELESCOP, INSTRUME, TOTELESCOP, TOINSTRUME',
     &           10)
            status = -1
            return
         endif
         call cpmdb(tel, inst, det, totel, toinst, todet, status)
         return
      else
         if ( totel.ne.' ' .or. toinst.ne.' ' .or. todet.ne.' ' ) then
            call XWRITE(' TO- qualifiers only used with COPY', 10)
            status = -1
            return
         endif
      endif
c
c  If no key or value, but mission given, show its info
c
      if ( tel.ne.' ' .and. inst.ne.' ' .and. 
     &     key.eq.' ' .and. val.eq.' ' ) then
         infile = ' '
         itel = DETIDX(tel, inst, det)
         if ( itel.le.0 ) then
            call XWRITE(' Mission not found', 10)
            status = -1
            return
         endif
         call wrmdb(infile, itel, status)
         return
      endif

      if ( tel.ne.' ' .and. inst.ne.' ' .and. 
     &     key.ne.' ' .and. val.ne.' ' ) then
         call gmdbtype(key, type, status)
         if ( status.ne.0 ) then
            call XWRITE(' Invalid key', 10)
            return
         endif
         itel = DETIDX(tel, inst, det)
         if ( itel.le.0 ) then
            call XWRITE(' Mission not found', 10)
            call XWRITE(' Use ADD to add new mission', 10)
            return
         endif
         call setmdb(itel, key, val, status)
         call upc(key)
         klen = LENACT(key)
         if ( key(klen-3:klen).eq.'FILE' ) then
            inquire(file=val, exist=there)
            if ( .not.there ) then
               write(ZWRite,'(2a)') 'File does not exist: ', val
               call xwarn(ZWRite, 5)
            endif
         endif
      elseif ( tel.ne.' ' .and. inst.ne.' ' .and. key.ne.' ' ) then
c
c        Return value as Tcl result
c
         call gmdbtype(key, type, status)
         if ( status.ne.0 ) then
            call XWRITE(' Invalid key', 10)
            return
         endif
         itel = DETIDX(tel, inst, det)
         if ( itel.le.0 ) then
            call XWRITE(' Mission not found', 10)
            return
         endif
         if ( type.eq.'i' ) then
            call gmdbi(itel, key, ival, 0, status)
            call tclreti(ival, status)
         elseif ( type.eq.'s' ) then
            call gmdbs(itel, key, sval, 0, status)
            call tclrets(sval, status)
         elseif ( type.eq.'d' ) then
            call gmdbd(itel, key, dval, 0, status)
            call tclretd(dval, status)
         else
            call xwrite(' Bad type', 5)
            status = -1
            return
         endif
      elseif ( tel.ne.' ' .or. inst.ne.' ' .or. 
     &         key.ne.' ' .or. val.ne.' ' ) then
         call XWRITE(' TELESCOP, INSTRUME, KEY, and VALUE must '//
     &               'be specified', 10)
         status = -1
         return
      endif

      if ( list ) then
         call XWRITE(' ', 10)
         write (ZWRite, '(a,i3)' ) ' Detectors defined: ', ZEXpnum
         call XWRITE(ZWRite, 10)
         call XWRITE(' ', 10)
         imtel = 0
         iminst = 0
         imdet = 0
         do i = 1, ZEXpnum
            imtel = MAX(LENACT(ZTElescop(i)),imtel)
            iminst = MAX(LENACT(ZINstrume(i)),iminst)
            imdet = MAX(LENACT(ZDEtnam(i)),imdet)
         enddo
         do i = 1, ZEXpnum
            tel= ZTElescop(i)
            inst= ZINstrume(i)
            det= ZDEtnam(i)
            if ( det.eq.' ' ) then
               write (ZWRite, '(1x,3a)') tel(1:imtel), ' : ',
     &                                   inst(1:iminst)
            else
               write (ZWRite, '(1x,5a)') tel(1:imtel), ' : ', 
     &               inst(1:iminst), ' : ', det(1:imdet)
            endif
            call XWRITE(ZWRite, 10)
         enddo
         call XWRITE(' ', 10)
      endif

      return
      end
