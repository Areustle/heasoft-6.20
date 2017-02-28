*+ADDSPEC
      SUBROUTINE ADDSPC
      IMPLICIT NONE
c 
c Description:
c  Wrapper program to run (spawn) some/all of the following tasks
c     MATHPHA - performs maths on PHA datasets 
c     MARFRMF - combines an ARF with a RMF to make an RSP
c     ADDRMF  - adds RMF/RSP datasets
c such as to facilitate the addition several PHA (and RMF) datasets.
c
c User i/ps required (prompted for):
c  None here, isolated in GP_ADDSPEC (see below), and prompted for as 
c  necessary by the individual tasks.
c
c Called routines
c  subroutine FCECHO           : (FTOOLS) write to standard i/o
c  subroutine FCERR             : (FTOOLS) write to standard error
c  subroutine GP_ADDSPEC       : (below) gets parameters from the par file
c  subroutine DO_ADDSPEC       : (below) the 'main' routine
c  
c Compilation:
c  subroutines require CALLIB, FTOOLS, FITSIO
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1.0.0: 95 Apr 18), original
c  Ian M George     (1.0.1: 95 Apr 20), increased error checking etc
c  Ian M George     (1.1.0: 95 Aug 22), added errmeth & properr params 4 mathpha
c  Ian M George     (1.1.1: 95 Aug 25), added marfrmf capability plus bkgd sub
c  Banashree M Seifert (1.2.0: 97 Mar 27)
c           . change in do_addspec
c             when qaddrmf=no, then donot print that rspfile is created
c             also qaddpha=no
c  Banashree M Seifert (1.3.0: 97 sept 16)
c           . change in do_addspec
c             call to spawn addrmf changed to 
c                     list=@addrmf.ascii 
c             instead of
c                     listfile=addrmf.ascii
c            This change should have been done before when addrmf was modified
c ----------------------------------------------------------------------
      character(7) version
      parameter (version = '1.3.0')
*- 
c Internals 
      character(40) taskname
      integer chatter, schatter, ierr
      real bexpscale
      character(1) alg_units
      character(5) properr
      character(10) errmeth
      character(25) context
      character(80) asciifil, outfil, message
      logical qaddpha, qaddrmf, qsubback
      logical killit

c Initialize
      COMMON/task/taskname
      taskname ='ADDSPEC '//version
      ierr = 0

      message = '** ADDSPEC '//version
      call fcecho(message)

      message = '***** THIS IS A BETA_TEST VERSION '//
     &     '(check your o/p) *****'
      call fcecho(message)

c Get Parameters from the par file
      call gp_addspec(asciifil, outfil, chatter, schatter, 
     &     qaddpha, alg_units, qaddrmf, qsubback, bexpscale,
     &     errmeth, properr, killit, ierr)
      if(ierr.NE.0) goto 926

c Do the nasty deed(s)
      call do_addspec(asciifil, outfil, chatter, schatter, 
     &     qaddpha, alg_units, qaddrmf, qsubback, bexpscale,
     &     errmeth, properr, killit, ierr)

c Inform user of failure, if necessary
 926  if(ierr.NE.0) then
         context = 'Fatal'
         message = '** ADDSPEC '//version//' ERROR : '// context
         call fcecho(message)
         call fcerr(context)
      else
         message = '***** THIS IS A BETA_TEST VERSION '//
     &        '(check your o/p) *****'
         call fcecho(message)
         message = '** ADDSPEC '//version//' Finished'
         call fcecho(message)
      endif

      return
      end



c -------------------------------------------------------------------------
*+GP_ADDSPEC
      subroutine gp_addspec(asciifil, outfil, chatter, schatter, 
     &     qaddpha, alg_units, qaddrmf, qsubback, bexpscale,
     &     errmeth, properr, killit, ierr)
      IMPLICIT NONE
      integer chatter, schatter, ierr
      real bexpscale
      character*(*) asciifil, outfil, alg_units
      character*(*) errmeth, properr
      logical qaddpha, qaddrmf, qsubback, killit
c 
c Description:
c  Gets the parameters required by ADDSPEC from the parameter file
c  NOTE - The par file is assumed to have been opened.
c
c User i/ps required (prompted for):
c  ASCIIFILE      - name of i/p FITS SOURCE file (produced by stwfits)
c ??? incomplete
c
c Origin:
c  Original
c
c Called Routines
c  subroutine FCECHO           : (FTOOLS) writes to standard o/p device
c  subroutine WT_FERRMSG       : (CALLIB) Writes standard FITSIO message etc
c
c Compilation:
c  requires XPI/Host interface etc and CALLIB
c
c Authors/Modification History:
c  Ian M George     (1.0.0: 1995 Apr 18), Original
c  Ian M George     (1.1.0: 1995 Aug 22), added errmeth & properr params 
c  Ian M George     (1.1.1: 1995 Sep 01), cleaned
      character(7) version
      parameter (version = '1.1.1')
*- 
c Internals
      character(20)  param
      character(40) errstr, wrnstr
      character(80) message
c Initialize
      errstr = '** GP_ADDSPEC '//version//' ERROR: '
      wrnstr = '** GP_ADDSPEC '//version//' WARNING: '
      ierr = 0

c Go get 'em coyboy....

c Get the name of the i/p file (from stwfits)
      call uclgst('infil', asciifil, ierr)
      if(ierr.NE.0) then
         message = errstr // 'Getting INFIL parameter'
         call fcecho(message)
         return
      endif

c Get the name of the o/p FITS file in OGIP-standard format
      call uclgst('outfil',outfil, ierr)
      if(ierr.NE.0) then
         message = errstr // 'Getting OUTFIL parameter'
         call fcecho(message)
         return
      endif
      if(outfil.EQ.' ') then
         message = errstr // 'No OUTFIL entered'
         call fcecho(message)
         ierr = 1
         return
      endif



c Get the logicals indicating which tasks are to be spawned
c ... add the PHAs (ie MATHPHA) ?
      call uclgsb('qaddpha',qaddpha, ierr)
      if(ierr.NE.0) then
         message = errstr // 'Getting QADDPHA parameter'
         call fcecho(message)
         return
      elseif(.NOT.qaddpha) then
         message = errstr // 'Supported behaviour'
         call fcecho(message)
         ierr = 1
         return
      endif
      
c Get the units of the algebraic expression (if necessary)
      if(qaddpha) then 
         call uclgst('units',param, ierr)
         if(ierr.NE.0) then
            message = errstr // 'Getting UNITS parameter'
            call fcecho(message)
            message = '... Setting UNITS to C (COUNTS)'
            call fcecho(message)
            ierr = 0
            param = 'C'
         elseif(param.EQ.' ')then
            message = '... Setting UNITS to C (COUNTS)'
            call fcecho(message)
            param = 'C'
         endif
         call crmvblk(param)
         call ftupch(param)
         alg_units = param(1:1)
      endif

c ... add the RMFs (ie ADDRMF) ?
      call uclgsb('qaddrmf',qaddrmf, ierr)
      if(ierr.NE.0) then
         message = errstr // 'Getting QADDRMF parameter'
         call fcecho(message)
         message = ' ... Setting QADDRMF to FALSE'
         call fcecho(message)
         qaddrmf = .false.
         ierr = 0
      endif
c ... Create a background PHA ?
      call uclgsb('qsubback',qsubback, ierr)
      if(ierr.NE.0) then
         message = errstr // 'Getting QSUBBACK parameter'
         call fcecho(message)
         message = ' ... Setting QSUBBACK to FALSE'
         call fcecho(message)
         qsubback = .false.
         ierr = 0
      endif
      
c ... Get the scaling factor for the background file
      if(qsubback) then
         call uclgsr('bexpscale',bexpscale, ierr)
         if(ierr.NE.0) then
            message = errstr // 'Getting BEXPSCALE parameter'
            call fcecho(message)
            message = ' ... Setting BEXPSCALE to 1E6'
            call fcecho(message)
            bexpscale = 1.0e6
            ierr = 0
         endif
      endif

c Get the error prescription to be used bu mathpha
      call uclgst('errmeth',errmeth, ierr)
      if((ierr.NE.0)) then
         message = errstr // 'Getting ERRMETH parameter'
         call fcecho(message)
         message = ' ... setting ERRMETH to POISS-1'
         call fcecho(message)
         errmeth = 'POISS-1'
         ierr = 0
      endif
      call crmvblk(errmeth)
      call ftupch(errmeth)

c Get the error-propagation flag
      call uclgst('properr',properr, ierr)
      if((ierr.NE.0)) then
         message = errstr // 'Getting PROPERR parameter'
         call fcecho(message)
         message = ' ... setting PROPERR to TRUE'
         call fcecho(message)
         properr = 'yes'
         ierr = 0
      endif
      call crmvblk(properr)
      call ftupch(properr)



c Get the chattiness flag
      call uclgsi('chatter',chatter, ierr)
      if(ierr.NE.0) then
         message = errstr // 'Getting CHATTER parameter'
         call fcecho(message)
         ierr = 0 
         message = errstr // 'Setting CHATTER = 10'
         call fcecho(message)
         chatter = 9
      endif      

c Give user info if requested
      if(chatter.GE.20) then      
         message = ' ... using GP_ADDSPEC ' // version
         call fcecho(message)
      endif

c get clobber parameter ...
      call uclgsb('clobber',killit, ierr)
      if(ierr.NE.0) then
         message = errstr // 'Getting CLOBBER parameter'
         call fcecho(message)
         ierr = 0
      endif

c Get the chattiness flag (for spawned tasks)
      call uclgsi('schatter',schatter, ierr)
      if(ierr.NE.0) then
         message = errstr // 'Getting SCHATTER parameter'
         call fcecho(message)
         ierr = 0 
         message = errstr // 'Setting SCHATTER = 10'
         call fcecho(message)
         schatter = 9
      endif      

      return
      end
c -------------------------------------------------------------------------
*+DO_ADDSPEC
      subroutine do_addspec(asciifil, outfil, chatter, schatter, 
     &     qaddpha, alg_units, qaddrmf, qsubback, bexpscale,
     &     errmeth, properr, killit, ierr)


      IMPLICIT NONE
      integer chatter, schatter, ierr
      real bexpscale
      character*(*) asciifil, outfil, alg_units
      character*(*) errmeth, properr
      logical qaddpha, qaddrmf, qsubback, killit
c 
c Description:
c  Program which actually spawns all the necessary standalones
c
c Passed parameters
c  {incomplete}
c
c Origin:
c  Original
c
c Called Routines
c  {incomplete}

c Authors/Modification History:
c  Ian M George     (0.0.9:95 Apr 18), original... Beta-test version
c  Ian M George     (1.0.0:95 Apr 20), more robust + copies made of RMFs
c Banashree M Seifert(Mar 27, 1997) 1.1.0:
c        . condition that when qaddrmf=no, then donot print to the screen
c          that rsp file is created
c Banashree M Seifert(Sept 16, 1997) 1.2.0:
c        . replacing  
c            str(1) = 'listfile = addrmf.ascii'
c          by
c            str(1) = 'list = @addrmf.ascii'
c          while spawning addrmf because due to change in addrmf this was not
c          modified. 
c  James Peachey HEASARC/GSFC/NASA, Raytheon STX (1.2.1: 98 Mar 17)
c     Replaced a call to callib's ppthnm, which parses a full file name
c     into a disk + path + short file name, with an explicit search for
c     the file name portion of a full file name. The callib routine
c     fails to work under certain *very normal* circumstances.
c --------------------------------------------------------------
      character(7) version
      parameter (version = '1.2.1')
*- 
c Internals
      integer status, lspawn, fcstln, clenact, i, ilen, j
      integer maxfiles, nfiles, ilen2
      integer nstrs, lstr(20)
      integer ibacktexp
      integer istart
      parameter (maxfiles=100)
      real backtexp, factor
      real ftsver, exposum, chksum
      real areascal(maxfiles), exposure(maxfiles)
      real backscal(maxfiles)
      real bexp(maxfiles), bbscal(maxfiles)
      character(40) errstr, wrnstr
      character(80) message
      character(2048) str(20), dummy
      character(80) inpha(maxfiles), rmfil(maxfiles), arfil(maxfiles)
      character(80) finalrmf(maxfiles)
      character(80) backfil(maxfiles)
      character(80) dummystr(1)
      character(80) factstr
      character(2048) cbuf
      character(2048) lgstring
      logical qokfil, exist
      logical qarfs, qmiss
c Initialise
      status = 0
      ierr = 0
      ilen=0
      j=0
      errstr = '** DO_ADDSPEC '//version//' ERROR:'      
      wrnstr = '** DO_ADDSPEC '//version//' WARNING:'      
      dummystr(1) = ' '
      exposum = 0.0
      exist = .false.

c Give user info if requested
      if(chatter.GE.20) then
         message = ' ... using DO_ADDSPEC '// version
         call fcecho(message)
         call ftvers(ftsver)
         write(message,'(a,f6.3)')
     &        ' ... using FITSIO Version ', ftsver
         call fcecho(message)
      endif
      if(chatter.ge.5) then
         message = ' ...... searching & checking i/p files'
         call fcecho(message)
      endif

c Check that the o/p file doesn't already exist or is otherwise illegal
      call ck_file(outfil,dummystr, 1, qokfil, killit,chatter)
      if(.NOT.qokfil) then
         message = '... Offending file is OUTFIL: '// outfil(:50)
         call fcecho(message)
         ierr = -1
         goto 482
      endif

c Clean up any 'old' temporary files which might be around
      if(qaddrmf) then
         INQUIRE(FILE='addrmf.ascii',EXIST=exist)
         if(exist) call delfil('addrmf.ascii')
      endif

c Read in the i/p ASCII file
      call rdlist(asciifil, chatter, maxfiles, nfiles,
     &     inpha, ierr)
      if(ierr.NE.0) goto 482

c Check out the PHA files & fill aux info arrays
      do i = 1, nfiles
         call suspha(chatter,inpha(i),
     &        areascal(i), exposure(i), rmfil(i), arfil(i),
     &        backfil(i), backscal(i),
     &        ierr)
         if(ierr.NE.0) goto 482
      enddo


c ========================== THE PHA FILE ============================
c --------------------------- MATHPHA -------------------------
c ... construct the command string and spawn MATHPHA task if necessary
      if(qaddpha) then
         if(nfiles.GT.100) then
            message = errstr//' Too many files for MATHPHA'
            call fcecho(message)
            message = ' ... Max no. is currently 100'
            call fcecho(message)
            ierr = 1
            goto 482
         endif
         lgstring = "'"//inpha(1)//"'"
         call crmvblk(lgstring)
         do i = 2, nfiles 
            ilen = clenact(lgstring)
            lgstring = lgstring(:ilen) // "+'"//inpha(i)//"'"
            call crmvblk(lgstring)
         enddo
         ilen = clenact(lgstring)
         str(1) = '"expr='//lgstring(:ilen)//'"'
         if(ilen.GT.2048) then
            message = errstr//' Spawn String Too long'
            call fcecho(message)
            message = ' ... Max is currently 2048 characters'
            call fcecho(message)
            ierr = 1
            goto 482
         endif
      

         call crmvblk(outfil)
         ilen = clenact(outfil)
         str(2) = '"outfil='//outfil(:ilen)//'.pha"'
         str(3) = 'units = '//alg_units
         str(4) = 'exposure = CALC'
         str(5) = 'properr ='//properr
         str(6) = 'errmeth ='//errmeth
         str(7) = 'areascal = NULL'
         str(8) = 'ncomments = 1'
         str(9) = '"comment1 = Created_by_a_spawn_from_'//
     &        'DO_ADDSPEC_v'// version//'"'
         write(str(10),'(a,i12)') 'chatter=',schatter

         nstrs = 10
         do i = 1, nstrs
            call crmvblk(str(i))
            lstr(i) = fcstln(str(i))
         enddo
         cbuf = 'mathpha'
         lspawn = fcstln(cbuf)
         do i = 1, nstrs
            dummy = str(i)
            cbuf = cbuf(:lspawn) //' '// dummy(:lstr(i))
            lspawn = fcstln(cbuf)
         enddo
         if(lspawn.GT.2048) then
            message = errstr//' Spawn String Too long'
            call fcecho(message)
            message = ' ... Max is currently 2048 characters'
            call fcecho(message)
            ierr = 1
            goto 482
         endif
         message = ' --------- spawning MATHPHA for PHA dataset ... '
         call fcecho(message)
         call fcecho(cbuf)

         status = 0
         call cspawn(cbuf,lspawn,status)
         if(status.NE.0) then
            message = errstr // ' Problem with MATHPHA spawn'
            call fcecho(message)
            write(message,'(a,i12)')
     &           ' ... CSPAWN Error flag = ', status
            call fcecho(message)
            ierr = 1
            goto 482
         endif
         message = ' --------- completed spawn to '//
     &        'MATHPHA for PHA dataset'
         call fcecho(message)
      endif
c ------------------------------------------------------------

c ========================== THE BACKFILE ============================
      if(qsubback) then
         backtexp = 0.0
         do i = 1, nfiles
            call susbak(chatter,backfil(i),
     &           bexp(i), bbscal(i), ierr)
            if(ierr.NE.0) goto 482
            backtexp = backtexp + bexp(i)
         enddo
         backtexp = backtexp*bexpscale

c --------------------------- MATHPHA -------------------------
c ... construct the command string and spawn MATHPHA task if necessary
         if(nfiles.GT.100) then
            message = errstr//' Too many files for MATHPHA'
            call fcecho(message)
            message = ' ... Max no. is currently 100'
            call fcecho(message)
            ierr = 1
            goto 482
         endif

         factor = backscal(1)*bexpscale/bbscal(1)
         write(factstr,'(g12.6)') factor
         lgstring = '('//backfil(1)//'*'//factstr//')'
         call crmvblk(lgstring)
         do i = 2, nfiles 
            factor = backscal(i)*bexpscale/bbscal(i)
            write(factstr,'(g12.6)') factor
            call crmvblk(factstr)
            ilen = clenact(lgstring)
            ilen2 = clenact(factstr)
            lgstring = lgstring(:ilen) // '+('//backfil(i)//
     &           '*'//factstr(:ilen2)//')'
            call crmvblk(lgstring)
         enddo
         ilen = clenact(lgstring)
         str(1) = '"expr='//lgstring(:ilen)//'"'
         if(ilen.GT.2048) then
            message = errstr//' Spawn String Too long'
            call fcecho(message)
            message = ' ... Max is currently 2048 characters'
            call fcecho(message)
            ierr = 1
            goto 482
         endif
      

         call crmvblk(outfil)
         ilen = clenact(outfil)
         str(2) = '"outfil='//outfil(:ilen)//'.bak"'
         str(3) = 'units = '//alg_units
         ibacktexp = INT(backtexp)
         write(factstr,'(I12)') ibacktexp
         ilen2 = clenact(factstr)
         str(4) = 'exposure = '//factstr(:ilen2)
         str(5) = 'properr ='//properr
         str(6) = 'errmeth ='//errmeth
         str(7) = 'areascal = NULL'
         str(8) = 'ncomments = 2'
         str(9) = '"comment1 = Created_by_a_spawn_from_'//
     &        'DO_ADDSPEC_v'// version//'"'
         str(10) = '"comment2 = NOTE - exposure_time_increased'//
     &        '_to_avoid_rounding_errors"'
         write(str(11),'(a,i12)') 'chatter=',schatter

         nstrs = 11
         do i = 1, nstrs
            call crmvblk(str(i))
            lstr(i) = fcstln(str(i))
         enddo
         cbuf = 'mathpha'
         lspawn = fcstln(cbuf)
         do i = 1, nstrs
            dummy = str(i)
            cbuf = cbuf(:lspawn) //' '// dummy(:lstr(i))
            lspawn = fcstln(cbuf)
         enddo
         if(lspawn.GT.2048) then
            message = errstr//' Spawn String Too long'
            call fcecho(message)
            message = ' ... Max is currently 2048 characters'
            call fcecho(message)
            ierr = 1
            goto 482
         endif
         message = ' --------- spawning MATHPHA for BKGD dataset ... '
         call fcecho(message)
         call fcecho(cbuf)

         status = 0
         call cspawn(cbuf,lspawn,status)
         if(status.NE.0) then
            message = errstr // ' Problem with MATHPHA spawn'
            call fcecho(message)
            write(message,'(a,i12)')
     &           ' ... CSPAWN Error flag = ', status
            call fcecho(message)
            ierr = 1
            goto 482
         endif
         message = ' --------- completed spawn to '//
     &        'MATHPHA for BKGD dataset'
         call fcecho(message)
      endif
c ------------------------------------------------------------

c ========================== THE RMF/RSP FILES ==========================
c ------------------ Rescale the RMFs ------------------------------
      if(.NOT.qaddrmf) goto 124
c First make a copy of the RMFs so as not to bugger them up 
      message = ' ... Making copies of RMFs to prevent corruption'
      call fcecho(message)
      do i = 1, nfiles
         call crmvblk(rmfil(i))
         ilen = clenact(rmfil(i))
         istart = 1
         do j = ilen, j, -1
            if(rmfil(i)(j:j) .eq. '/') then
               istart = j + 1
               goto 137
            endif
         enddo
 137     continue
         dummystr(1) = rmfil(i)(istart:)
         ilen = clenact(dummystr(1))
         dummystr(1) = dummystr(1)(:ilen)//'_tmp'
         call copy(rmfil(i),dummystr(1),chatter,ierr)
         if(ierr.NE.0) goto 482      
         rmfil(i) = dummystr(1)
      enddo
      dummystr(1) = ' '

c Now we have to fix up all the RMFs by multiplying through by the 
c value of the AREASCAL keyword in the corresponding PHA file.
      do i = 1, nfiles
         if(areascal(i).EQ.1.0) goto 123
         str(1) = 'add=yes'
         call crmvblk(rmfil(i))
         ilen = clenact(rmfil(i))
         str(2) = '"fitsfile ='//rmfil(i)(:ilen)//'[1]"'
         str(3) = 'keyword = TSCAL6'
         write(str(4),'(a,g12.6)') 'value=',areascal(i)
         nstrs = 4
         do j = 1, nstrs
            call crmvblk(str(j))
            lstr(j) = fcstln(str(j))
         enddo
         cbuf = 'fparkey'
         lspawn = fcstln(cbuf)
         do j = 1, nstrs
            dummy = str(j)
            cbuf = cbuf(:lspawn) //' '// dummy(:lstr(j))
            lspawn = fcstln(cbuf)
         enddo
         if(i.EQ.1) then
            message = ' --------- spawning FPARKEY '//
     &           '(on copies of RMFs)... '
            call fcecho(message)
         endif
         if(lspawn.lt.80) then
            message = cbuf(:lspawn)
         else
            message = cbuf(:70) // ' (etc...)'
         endif
         call fcecho(message)

         status = 0
         call cspawn(cbuf,lspawn,status)
         if(status.NE.0) then
            message = errstr // ' Problem with FPARKEY spawn'
            call fcecho(message)
            write(message,'(a,i12)')
     &           ' ... CSPAWN Error flag = ', status
            call fcecho(message)
            ierr = 1
            goto 482
         endif
         if(i.EQ.nfiles) then
            message = ' --------- completed spawn to FPARKEY'
            call fcecho(message)
         endif
 123  enddo
c ------------------ Multiple by ARFs (if possible) -------------------------
c ... First loop through arfil values and check we know what's up
      qarfs = .false.
      qmiss = .false.
      do i = 1, nfiles
         if(arfil(i).NE.'NONE') then
            qarfs = .true.
         else
            qmiss = .true.
         endif
      enddo
      if((qarfs).AND.(qmiss)) then
         message = errstr//' Problem with ARFs'
         call fcecho(message)
         message = ' ...... Some of the i/p PHAs have ARFs specified,'
     &        //' but others dont'
         call fcecho(message)
         message = ' ... Unable to determine what to do'
         call fcecho(message)
         ierr = 99
         goto 482
      endif
c ... Now loop through the files running MARFRMF if applicable
      if(.NOT.qarfs) then
         do i = 1, nfiles
            finalrmf(i) = rmfil(i)
         enddo
      else
         do i = 1, nfiles
            call crmvblk(rmfil(i))
            ilen = clenact(rmfil(i))
            str(1) = 'rmfil ='//rmfil(i)(:ilen)
            finalrmf(i) = rmfil(i)(:ilen)//'_rsp'
            INQUIRE(FILE=finalrmf(i),EXIST=exist)
            if(exist) call delfil(finalrmf(i))
            str(2) = 'ebfil = %'
            ilen = clenact(arfil(i))
            str(3) = 'arfil ='//arfil(i)(:ilen)
            ilen = clenact(finalrmf(i))
            str(4) = 'outfil ='//finalrmf(i)(:ilen)
            str(5) = 'qdivide = no'
c         write(str(6),'(a,g12.6)') 'value=',areascal(i)
c           nstrs = 6
            nstrs = 5
            do j = 1, nstrs
               call crmvblk(str(j))
               lstr(j) = fcstln(str(j))
            enddo
            cbuf = 'marfrmf'
            lspawn = fcstln(cbuf)
            do j = 1, nstrs
               dummy = str(j)
               cbuf = cbuf(:lspawn) //' '// dummy(:lstr(j))
               lspawn = fcstln(cbuf)
            enddo
            if(i.EQ.1) then
               message = ' --------- spawning MARFRMF '//
     &              '(on copies of RMFs)... '
               call fcecho(message)
            endif
            if(lspawn.lt.80) then
               message = cbuf(:lspawn)
            else
               message = cbuf(:70) // ' (etc...)'
            endif
            call fcecho(message)

            status = 0
            call cspawn(cbuf,lspawn,status)
            if(status.NE.0) then
               message = errstr // ' Problem with MARFRMF spawn'
               call fcecho(message)
               write(message,'(a,i12)')
     &              ' ... CSPAWN Error flag = ', status
               call fcecho(message)
               ierr = 1
               goto 482
            endif
            if(i.EQ.nfiles) then
               message = ' --------- completed spawn to MARFRMF'
               call fcecho(message)
            endif
         enddo
      endif

c ----------- Write ASCII file listing RMFs & their weightings --------------
c This is in preparation for the spawning of ADDRMF
      exposum = 0.0
      do i = 1, nfiles
         exposum = exposum + exposure(i)
      enddo
      if(chatter.GE.5) then
         write(message,'(a,g12.6,a)') 
     &        ' ... Total exposure in o/p file: ', exposum,
     &        ' seconds'
         call fcecho(message)
      endif
      do i = 1, nfiles
         exposure(i) = exposure(i)/exposum
         chksum = chksum + exposure(i)
      enddo
      if(ABS(chksum-1.0).GT.1e-3) then
         write(message,'(a,a,g12.6)')
     &        wrnstr,' Checksum: ', chksum
         call fcecho(message)
      endif
c Write the ASCII guys
      message = ' ... Writing Temporary ASCII file'
      call fcecho(message)
      call wtlist('addrmf.ascii', chatter, nfiles,
     &     finalrmf, exposure, ierr)
      if(ierr.NE.0) then
         goto 482
      endif
      
c --------------------------- ADDRMF -------------------------
c ... construct the command string and spawn ADDRMF task 
ccc      str(1) = 'listfile = addrmf.ascii'
      str(1) = 'list = @addrmf.ascii'
      call crmvblk(outfil)
      ilen = clenact(outfil)
      str(2) = '"rmffile='//outfil(:ilen)//'.rsp"'
      nstrs = 2
      do j = 1, nstrs
         call crmvblk(str(j))
         lstr(j) = fcstln(str(j))
      enddo
      cbuf = 'addrmf'
      lspawn = fcstln(cbuf)
      do j = 1, nstrs
         dummy = str(j)
         cbuf = cbuf(:lspawn) //' '// dummy(:lstr(j))
         lspawn = fcstln(cbuf)
      enddo
      message = ' --------- spawning ADDRMF ... '
      call fcecho(message)
      if(lspawn.lt.80) then
         message = cbuf(:lspawn)
      else
         message = cbuf(:70) // ' (etc...)'
      endif
      call fcecho(message)

      status = 0
      call cspawn(cbuf,lspawn,status)
      if(status.NE.0) then
         message = errstr // ' Problem with ADDRMF spawn'
         call fcecho(message)
         write(message,'(a,i12)')
     &        ' ... CSPAWN Error flag = ', status
         call fcecho(message)
         ierr = 1
         goto 482
      endif
      message = ' --------- completed spawn to ADDRMF'
      call fcecho(message)
c ------------------------------------------------------------
c Now fix-up the PHA file created by MATHPHA as necessary
      message = 
     &     ' --------- spawning FPARKEY (to fix o/p files)... '
      call fcecho(message)
c .... Fix up the RESPFILE keyword value if the RSPs are being added
      call crmvblk(outfil)
      ilen = clenact(outfil)
      str(1) = '"fitsfile ='//outfil(:ilen)//'.pha[1]"'
      str(2) = 'keyword = RESPFILE'
      str(3) = '"value='//outfil(:ilen)//'.rsp"'
      nstrs = 3
      do j = 1, nstrs
         call crmvblk(str(j))
         lstr(j) = fcstln(str(j))
      enddo
      cbuf = 'fparkey'
      lspawn = fcstln(cbuf)
      do j = 1, nstrs
         dummy = str(j)
         cbuf = cbuf(:lspawn) //' '// dummy(:lstr(j))
         lspawn = fcstln(cbuf)
      enddo
      if(chatter.GE.10) then
         if(lspawn.lt.80) then
            message = cbuf(:lspawn)
         else
            message = cbuf(:70) // ' (etc...)'
         endif
         call fcecho(message)
      endif
         
      status = 0
      call cspawn(cbuf,lspawn,status)
      if(status.NE.0) then
         message = errstr // ' Problem with FPARKEY spawn'
         call fcecho(message)
         write(message,'(a,i12)')
     &        ' ... CSPAWN Error flag = ', status
         call fcecho(message)
         ierr = 1
         goto 482
      endif
c .... Fix up the BACKFILE keyword value if BACKFILE has been created
      if(qsubback) then
         call crmvblk(outfil)
         ilen = clenact(outfil)
         str(1) = '"fitsfile ='//outfil(:ilen)//'.pha[1]"'
         str(2) = 'keyword = BACKFILE'
         str(3) = '"value='//outfil(:ilen)//'.bak"'
         nstrs = 3
         do j = 1, nstrs
            call crmvblk(str(j))
            lstr(j) = fcstln(str(j))
         enddo
         cbuf = 'fparkey'
         lspawn = fcstln(cbuf)
         do j = 1, nstrs
            dummy = str(j)
            cbuf = cbuf(:lspawn) //' '// dummy(:lstr(j))
            lspawn = fcstln(cbuf)
         enddo
         if(chatter.GE.10) then
            if(lspawn.lt.80) then
               message = cbuf(:lspawn)
            else
               message = cbuf(:70) // ' (etc...)'
            endif
            call fcecho(message)
         endif

         status = 0
         call cspawn(cbuf,lspawn,status)
         if(status.NE.0) then
            message = errstr // ' Problem with FPARKEY spawn'
            call fcecho(message)
            write(message,'(a,i12)')
     &           ' ... CSPAWN Error flag = ', status
            call fcecho(message)
            ierr = 1
            goto 482
         endif
c .... Fix up the BACKSCAL keyword values 
         call crmvblk(outfil)
         ilen = clenact(outfil)
         str(1) = '"fitsfile ='//outfil(:ilen)//'.pha[1]"'
         str(2) = 'keyword = BACKSCAL'
         str(3) = '"value=1.0"'
         nstrs = 3
         do j = 1, nstrs
            call crmvblk(str(j))
            lstr(j) = fcstln(str(j))
         enddo
         cbuf = 'fparkey'
         lspawn = fcstln(cbuf)
         do j = 1, nstrs
            dummy = str(j)
            cbuf = cbuf(:lspawn) //' '// dummy(:lstr(j))
            lspawn = fcstln(cbuf)
         enddo
         if(chatter.GE.10) then
            if(lspawn.lt.80) then
               message = cbuf(:lspawn)
            else
               message = cbuf(:70) // ' (etc...)'
            endif
            call fcecho(message)
         endif

         status = 0
         call cspawn(cbuf,lspawn,status)
         if(status.NE.0) then
            message = errstr // ' Problem with FPARKEY spawn'
            call fcecho(message)
            write(message,'(a,i12)')
     &           ' ... CSPAWN Error flag = ', status
            call fcecho(message)
            ierr = 1
            goto 482
         endif
c .... Fix up the BACKSCAL keyword values 
         call crmvblk(outfil)
         ilen = clenact(outfil)
         str(1) = '"fitsfile ='//outfil(:ilen)//'.bak[1]"'
         str(2) = 'keyword = BACKSCAL'
         str(3) = '"value=1.0"'
         nstrs = 3
         do j = 1, nstrs
            call crmvblk(str(j))
            lstr(j) = fcstln(str(j))
         enddo
         cbuf = 'fparkey'
         lspawn = fcstln(cbuf)
         do j = 1, nstrs
            dummy = str(j)
            cbuf = cbuf(:lspawn) //' '// dummy(:lstr(j))
            lspawn = fcstln(cbuf)
         enddo
         if(chatter.GE.10) then
            if(lspawn.lt.80) then
               message = cbuf(:lspawn)
            else
               message = cbuf(:70) // ' (etc...)'
            endif
            call fcecho(message)
         endif
         
         status = 0
         call cspawn(cbuf,lspawn,status)
         if(status.NE.0) then
            message = errstr // ' Problem with FPARKEY spawn'
            call fcecho(message)
            write(message,'(a,i12)')
     &           ' ... CSPAWN Error flag = ', status
            call fcecho(message)
            ierr = 1
            goto 482
         endif
      endif
c ------------------------------------------------------------
      message = ' --------- completed spawns to FPARKEY'
      call fcecho(message)
c ------------------------------------------------------------
c Clean up the temporary RMF files we made
      message = ' ...  Deleting temporary copies of RMFs'
      call fcecho(message)
      do i = 1, nfiles
         call delfil(finalrmf(i))
      enddo

 124  continue

c Check of errors
 482  if(ierr.ne.0) then
         message = errstr // ' Fatal'
         call fcecho(message)
      else
         message = ' ... Following files created:'
         call fcecho(message)
         if(qaddpha) then
            if(outfil(1:1) .eq. '!')then
               message = ' ...... Source PHA: '//outfil(2:ilen)//'.pha'
            else
               message = ' ...... Source PHA: '//outfil(:ilen)//'.pha'
            endif
         endif
         call fcecho(message)
         if(qsubback) then
            message = ' ...... Backgd PHA: '//outfil(:ilen)//'.bak'
            call fcecho(message)
         endif
         if(qaddrmf) then
            message = ' ...... Source RSP: '//outfil(:ilen)//'.rsp'
            call fcecho(message)
         endif
      endif

      return
      end
c -----------------------------------------------------------
c -------------------------------------------------------------------------
*+SUSPHA
      subroutine suspha(chatter,phaexp,
     &     areascal, exposure, rmfil, arfil, 
     &     backfil,backscal, 
     &     ierr)

      IMPLICIT NONE
      integer ierr, chatter
      real areascal, exposure 
      real backscal
      character*(*) phaexp
      character*(*) rmfil, arfil, backfil

c Description
c  Opens a PHA file, finds the correct/specified extension, and reads a bunch 
c of keywords from the header which will be required latter on in ADDSPEC.
c  Note the file is closed again on completion
c
c Passed Parameters
c  CHATTER          i   : Chattiness flag - o/p only if >40
c  incomplete
c  IERR               o : Error flag (zero if everything OK)
c 
c Called Routines
c  subroutine CGETLUN   : (CALLIB) Gets a free logical unit
c  subroutine FCECHO      : (FTOOLS) Writes to standard o/p
c  subroutine FCPARS    : (FTOOLS) Parses filename[extn#] string
c  subroutine FNDEXT      : (CALLIB) Finds xtens based on EXTNAME value
c  subroutine FNDHDU      : (CALLIB) Finds xtens based on HDUCLASn values
c  subroutine FTMAHD      : (FITSIO) Move to an absolute xtens no.
c  subroutine FTMRHD      : (FITSIO) Move a relative no. xtens rel to CDU
c  subroutine FTOPEN      : (FITSIO) Open a FITS file
c  subroutine RDPHA1    : (CALLIB) reads a PHA dataset
c  subroutine WT_FERRMSG: (CALLIB) Writes standard FITSIO error message
c
c Author/Modification History
c  Ian M George (1.0.0:95 Apr 19) Original
c  Ian M George (1.1.0:95 Aug 25) Add backfil parameter as passed
      character(7) version
      parameter (version='1.1.0')
*-
c Max arrays
      integer maxextn
      parameter (maxextn=99)
c Internals 
      integer i, jj, kk, imove, clenact
      integer extn, iunit, htype, status, block
      integer ninstr, nsearch, nfound
      integer next(maxextn)
      character(20) outhdu(9,maxextn), outver(9,maxextn)
      character(20) extnam(maxextn)
      character(20) instr(9)
      character(30) errstr, wrnstr, comm
      character(80) message
      character(80) phafil

c Initialize
      ierr = 0
      extn = 0
      status = 0
      errstr = '** SUSPHA '//version//' ERROR:'
      wrnstr = '** SUSPHA '//version//' WARNING:'

c ... give user info if requested
      if(chatter.GT.40) then
         message = ' ... using SUSPHA '// version
         call fcecho(message)
      endif
      
c Parse the supplied filenames, stripping off incld extension numbers
      call fcpars(phaexp,phafil,extn,status)
      if(status.NE.0) then
         message = wrnstr // ' Problem parsing the expression:'
         call fcecho(message)
         message = ' ......    '//phaexp(:MIN(50,clenact(phaexp)))
         call fcecho(message)
         message = ' ...... will search all extensions'
         call fcecho(message)
         extn = -99
      endif

c Open i/p file
      status = 0
      call cgetlun(iunit)
      call ftopen(iunit,phafil,0,block,status)
      IF (status.NE.0) THEN
         message = errstr//' opening file: '//phafil(:20)
         call wt_ferrmsg(status,message)
         ierr = 1
         return
      ENDIF

c Find the SPECTRUM extension in the PHA file 
c - Extension number NOT given as part of phaexp (search for HDUCLAS/EXTNAM)
      if(extn.LT.0) then
         ninstr = 1
         instr(1) = 'SPECTRUM'
         nsearch = maxextn
         call fndhdu(chatter, iunit, ninstr, instr,
     &        nsearch, nfound, next, outhdu, outver, extnam, ierr)
c     ... check for old-style EXTNAME values if no OK HDUCLASn values found
         if(nfound.LE.0) then
            message = wrnstr//
     &           ' Ext w/ allowed HDUCLASn keywrds not found'
            call fcecho(message)
            message = ' ... offending file: '
     &           //phafil(:MIN(len(message)-21,clenact(phafil)))
            call fcecho(message)
            message =
     &           ' ... searching for extension with EXTNAME = SPECTRUM'
            call fcecho(message)
            call fndext(chatter, iunit, 'SPECTRUM',
     &           nsearch, nfound, next, outhdu, outver, extnam, ierr)
            if(nfound.EQ.1) then
               message = ' ... located acceptable extension'
               call fcecho(message)
            endif
         endif
c  - Extension number IS given as part of phafil 
      else
         call ftmahd(iunit,extn+1,htype,status)
         message = wrnstr // ' Problem moving to specified xtens'
         call wt_ferrmsg(status, message)
c     ... grab the HDUCLAS values for reference
         ninstr = 1
         instr(1) = '*'
         nsearch = 1
         call fndhdu(MIN(chatter,20), iunit, ninstr, instr,
     &        nsearch, nfound, next, outhdu, outver, extnam, ierr)
         nfound = 1
         next(1) = 0
      endif

c - sort out what we've got
 369  if(nfound.GT.1) then
         do i = 1, nfound
            if(outhdu(2,i) .EQ.'DETECTOR') then
               if(i.NE.nfound) then
                  do kk = i,nfound
                     do jj = 1, 3
                        outhdu(jj,kk) = outhdu(jj,kk+1)
                     enddo
                     next(kk) = next(kk+1)      
                     extnam(kk) = extnam(kk+1)
                  enddo              
               endif
               nfound = nfound - 1
               goto 369
            endif
         enddo
         message = errstr//' PHA file contains >1 PHA dataset'
         call fcecho(message)
         write(message,'(a,i12,a)') ' ... ',nfound,
     &        ' extensions found:'
         call fcecho(message)
         do i = 1, nfound
            write(message,'(a,i12,a)') ' ...... Ext ',next(i),':'      
            call fcecho(message)
            write(message,'(6X,a,a)') 'EXTNAME  = ', extnam(i)
            call fcecho(message)
            do jj = 1, 3
               write(message,'(6X,a,i1,2a)') 
     &              'HDUCLAS',jj,' = ',outhdu(jj,i)
               call fcecho(message)
            enddo
         enddo
         message = 
     &        ' ... Extension # must be specified '//
     &        ' via "file[ext#]" in i/p expression'
         call fcecho(message)
         ierr = 2
         goto 482
      elseif(nfound.LE.0) then
         message = wrnstr//
     &        ' Unable to locate a SPECTRUM extension'
         call fcecho(message)
         ierr = 2
         goto 482
      endif

c Move to the Extension if not already there
      if(next(1).GT.0) then
         imove = next(1)
         status = 0
         call ftmrhd(iunit,imove,htype,status)
         message = wrnstr // ' Problem moving to SPECTRUM xtens'
         call wt_ferrmsg(status, message)
      endif

c ------------------------ KEYWORDS --------------------------
c Read in the required keywords 
c ...... areascal
      call ftgkye(iunit,'AREASCAL',areascal,comm,status)
      if(status.NE.0) then
         message = errstr//' reading AREASCAL keyword'
         call wt_ferrmsg(status,message)
         ierr = -1
         goto 482
      endif
c ...... exposure
      call ftgkye(iunit,'EXPOSURE',exposure,comm,status)
      if(status.NE.0) then
         message = errstr//' reading EXPOSURE keyword'
         call wt_ferrmsg(status,message)
         ierr = -1
         goto 482
      endif
c ...... rmf
      call ftgkys(iunit,'RESPFILE',rmfil,comm,status)
      if(status.NE.0) then
         message = errstr//' reading RESPFILE keyword'
         call wt_ferrmsg(status,message)
         ierr = -1
         goto 482
      endif
c ...... arf
      call ftgkys(iunit,'ANCRFILE',arfil,comm,status)
      if(status.NE.0) then
         message = errstr//' reading ANCRFILE keyword'
         call wt_ferrmsg(status,message)
         ierr = -1
         goto 482
      endif
c ...... backfile
      call ftgkys(iunit,'BACKFILE',backfil,comm,status)
      if(status.NE.0) then
         message = errstr//' reading BACKFILE keyword'
         call wt_ferrmsg(status,message)
         ierr = -1
         goto 482
      endif
c ...... backscal
      call ftgkye(iunit,'BACKSCAL',backscal,comm,status)
      if(status.NE.0) then
         message = errstr//' reading BACKSCAL keyword'
         call wt_ferrmsg(status,message)
         ierr = -1
         goto 482
      endif

c -------------------------------------------------------------

c Fix up the bizarre values
      if(rmfil.EQ.'%match%') then
         rmfil = phafil
         jj = clenact(rmfil)
         if(rmfil(jj-3:jj).EQ.'.pha') then
            rmfil = rmfil(:jj-4)//'.rsp'
         endif         
      endif
      if(arfil.EQ.'%match%') then
         arfil = phafil
         jj = clenact(arfil)
         if(arfil(jj-3:jj).EQ.'.pha') then
            arfil = arfil(:jj-4)//'.arf'
         endif         
      endif
      if((arfil.EQ.'NONE').OR.(arfil.EQ.'none')
     &     .OR.(arfil.EQ.' ')) then
         arfil = 'NONE'
      endif
      if((backfil.EQ.'NONE').OR.(backfil.EQ.'none')
     &     .OR.(backfil.EQ.' ')) then
         backfil = 'NONE'
      endif
      if(backscal.LE.0.0) then
         backscal = -99
         backfil = 'NONE'
      endif


c Check for errors
 482  if(ierr.ne.0) then
         message = errstr // ' Unable to recover'
         call fcecho(message)
      endif
c     Close the FITS file
      call ftclos(iunit, status) 
      
      return
      end
c 
c -------------------------------------------------------------------------
*+SUSBAK
      subroutine susbak(chatter,phaexp,
     &     exposure, bbscal,
     &     ierr)

      IMPLICIT NONE
      integer ierr, chatter
      real exposure, bbscal
      character*(*) phaexp

c Description
c  Opens a PHA file, finds the correct/specified extension, and reads a bunch 
c of keywords from the header which will be required latter on in ADDSPEC.
c  Note the file is closed again on completion
c
c Passed Parameters
c  CHATTER          i   : Chattiness flag - o/p only if >40
c  incomplete
c  IERR               o : Error flag (zero if everything OK)
c 
c Called Routines
c  subroutine CGETLUN   : (CALLIB) Gets a free logical unit
c  subroutine FCECHO      : (FTOOLS) Writes to standard o/p
c  subroutine FCPARS    : (FTOOLS) Parses filename[extn#] string
c  subroutine FNDEXT      : (CALLIB) Finds xtens based on EXTNAME value
c  subroutine FNDHDU      : (CALLIB) Finds xtens based on HDUCLASn values
c  subroutine FTMAHD      : (FITSIO) Move to an absolute xtens no.
c  subroutine FTMRHD      : (FITSIO) Move a relative no. xtens rel to CDU
c  subroutine FTOPEN      : (FITSIO) Open a FITS file
c  subroutine RDPHA1    : (CALLIB) reads a PHA dataset
c  subroutine WT_FERRMSG: (CALLIB) Writes standard FITSIO error message
c
c Author/Modification History
c  Ian M George (1.0.0:95 Aug 25) Original
      character(7) version
      parameter (version='1.0.0')
*-
c Max arrays
      integer maxextn
      parameter (maxextn=99)
c Internals 
      integer i, jj, kk, imove, clenact
      integer extn, iunit, htype, status, block
      integer ninstr, nsearch, nfound
      integer next(maxextn)
      character(20) outhdu(9,maxextn), outver(9,maxextn)
      character(20) extnam(maxextn)
      character(20) instr(9)
      character(30) errstr, wrnstr, comm
      character(80) message
      character(80) phafil

c Initialize
      ierr = 0
      extn = 0
      status = 0
      errstr = '** SUSBAK '//version//' ERROR:'
      wrnstr = '** SUSBAK '//version//' WARNING:'

c ... give user info if requested
      if(chatter.GT.40) then
         message = ' ... using SUSBAK '// version
         call fcecho(message)
      endif

c Parse the supplied filenames, stripping off incld extension numbers
      call fcpars(phaexp,phafil,extn,status)
      if(status.NE.0) then
         message = wrnstr // ' Problem parsing the expression:'
         call fcecho(message)
         message = ' ......    '//phaexp(:MIN(50,clenact(phaexp)))
         call fcecho(message)
         message = ' ...... will search all extensions'
         call fcecho(message)
         extn = -99
      endif

c Open i/p file
      status = 0
      call cgetlun(iunit)
      call ftopen(iunit,phafil,0,block,status)
      IF (status.NE.0) THEN
         message = errstr//' opening file: '//phafil(:20)
         call wt_ferrmsg(status,message)
         ierr = 1
         return
      ENDIF

c Find the SPECTRUM extension in the PHA file 
c - Extension number NOT given as part of phaexp (search for HDUCLAS/EXTNAM)
      if(extn.LT.0) then
         ninstr = 1
         instr(1) = 'SPECTRUM'
         nsearch = maxextn
         call fndhdu(chatter, iunit, ninstr, instr,
     &        nsearch, nfound, next, outhdu, outver, extnam, ierr)
c     ... check for old-style EXTNAME values if no OK HDUCLASn values found
         if(nfound.LE.0) then
            message = wrnstr//
     &           ' Ext w/ allowed HDUCLASn keywrds not found'
            call fcecho(message)
            message = ' ... offending file: '
     &           //phafil(:MIN(len(message)-21,clenact(phafil)))
            call fcecho(message)
            message =
     &           ' ... searching for extension with EXTNAME = SPECTRUM'
            call fcecho(message)
            call fndext(chatter, iunit, 'SPECTRUM',
     &           nsearch, nfound, next, outhdu, outver, extnam, ierr)
            if(nfound.EQ.1) then
               message = ' ... located acceptable extension'
               call fcecho(message)
            endif
         endif
c  - Extension number IS given as part of phafil 
      else
         call ftmahd(iunit,extn+1,htype,status)
         message = wrnstr // ' Problem moving to specified xtens'
         call wt_ferrmsg(status, message)
c          ... grab the HDUCLAS values for reference
         ninstr = 1
         instr(1) = '*'
         nsearch = 1
         call fndhdu(MIN(chatter,20), iunit, ninstr, instr,
     &        nsearch, nfound, next, outhdu, outver, extnam, ierr)
         nfound = 1
         next(1) = 0
      endif

c - sort out what we've got
 369  if(nfound.GT.1) then
         do i = 1, nfound
            if(outhdu(2,i) .EQ.'DETECTOR') then
               if(i.NE.nfound) then
                  do kk = i,nfound
                     do jj = 1, 3
                        outhdu(jj,kk) = outhdu(jj,kk+1)
                     enddo
                     next(kk) = next(kk+1)      
                     extnam(kk) = extnam(kk+1)
                  enddo              
               endif
               nfound = nfound - 1
               goto 369
            endif
         enddo
         message = errstr//' PHA file contains >1 PHA dataset'
         call fcecho(message)
         write(message,'(a,i12,a)') ' ... ',nfound,
     &        ' extensions found:'
         call fcecho(message)
         do i = 1, nfound
            write(message,'(a,i12,a)') ' ...... Ext ',next(i),':'      
            call fcecho(message)
            write(message,'(6X,a,a)') 'EXTNAME  = ', extnam(i)
            call fcecho(message)
            do jj = 1, 3
               write(message,'(6X,a,i1,2a)') 
     &              'HDUCLAS',jj,' = ',outhdu(jj,i)
               call fcecho(message)
            enddo
         enddo
         message = 
     &        ' ... Extension # must be specified '//
     &        ' via "file[ext#]" in i/p expression'
         call fcecho(message)
         ierr = 2
         goto 482
      elseif(nfound.LE.0) then
         message = wrnstr//
     &        ' Unable to locate a SPECTRUM extension'
         call fcecho(message)
         ierr = 2
         goto 482
      endif

c Move to the Extension if not already there
      if(next(1).GT.0) then
         imove = next(1)
         status = 0
         call ftmrhd(iunit,imove,htype,status)
         message = wrnstr // ' Problem moving to SPECTRUM xtens'
         call wt_ferrmsg(status, message)
      endif

c ------------------------ KEYWORDS --------------------------
c Read in the required keywords 
c ...... exposure
      call ftgkye(iunit,'EXPOSURE',exposure,comm,status)
      if(status.NE.0) then
         message = errstr//' reading EXPOSURE keyword'
         call wt_ferrmsg(status,message)
         ierr = -1
         goto 482
      endif
c     ...... backscal
      call ftgkye(iunit,'BACKSCAL',bbscal,comm,status)
      if(status.NE.0) then
         message = errstr//' reading BACKSCAL keyword'
         call wt_ferrmsg(status,message)
         ierr = -1
         goto 482
      endif

c Check for errors
 482  if(ierr.ne.0) then
         message = errstr // ' Unable to recover'
         call fcecho(message)
      endif
c Close the FITS file
      call ftclos(iunit, status) 

      return
      end
c 
c -------------------------------------------------------------------------
*+RDLIST
      subroutine rdlist(asciifil, chatter, maxfiles, nfiles,
     &     inpha, ierr)

      IMPLICIT NONE
      integer chatter, ierr
      integer maxfiles, nfiles
      character*(*) asciifil, inpha(maxfiles)
c 
c Description:
c  Subroutine to open & read the i/p ASCII file and fill the array 
c of PHA filenames
c
c Passed parameters
c  ASCIIFIL            i   : Name of ASCII file contains PHA filenames
c  CHATTER            i   : Chatter flag
c  MAXFILES            i   : Max no. PHA files possible
c  NFILES              o : Actual no. PHA files
c  INPHA              o : Array of PHA filenames
c  IERR                    o : Error flag (zero = OK)
c
c Origin:
c  Original - loosely based on Keith Arnaud's rdlstf.f subroutine 
c supplied as part of the ftools/caltools/addrmf.
c
c Called Routines
c  subroutine CGETLUN      :(CALLIB) Gets free FORTRAN unit
c  subroutine FCECHO    :(FTOOLS) Writes to STDOUT
c  subroutine CRMVLBK   :(CALLIB) Removes leading blanks from string
c
c Authors/Modification History:
c  Ian M George     (1.0.0:95 Apr 18), original
      character(7) version
      parameter (version = '1.0.0')
*- 
c Internals
      integer iunit, i
      character(40)  errstr, wrnstr
      character(80) string
      character(80)  message
c     Initialize
      errstr = '** RDLIST '//version//' ERROR: '
      wrnstr = '** RDLIST '//version//' WARNING: '
      ierr = 0
      nfiles = 0

c Give a bit of banter to the keen ones...
      if(chatter.GE.20) then
         message = ' ... using RDLIST ' // version
         call fcecho(message)
      endif

c Get a free FORTRAN unit & open up the bugger
      call cgetlun(iunit)
      open(iunit,file=asciifil,status='old',iostat=ierr)
      if(ierr.NE.0) then
         message = errstr//' Unable to open ASCII file'
         call fcecho(message)
         return
      endif
      
c Read in the first line of the asciifil
      read(iunit,'(a)',iostat=ierr) string
      if(ierr.NE.0) then
         message = errstr//' Unable to read ASCII file'
         call fcecho(message)
         if(chatter.GT.20) then
            message = ' ...... Failed on First line'
            call fcecho(message)   
         endif
         return            
      endif
c Lob out infor for debugging user
      if(chatter.GT.20) call fcecho('...Found:')

c Parse it, then start loading the rest
      do while(ierr.EQ.0)
c ........ check whether we've exceeded the buffers
         if(nfiles.GE.maxfiles) then
            message = errstr//'#files buffer exceeded'
            call fcecho(message)
            ierr = -9
            return
         else
            nfiles = nfiles+1
         endif
c ........ clean up the filename
         call crmvlbk(string)
         i = 1
         do while(string(i:i).NE.' ')
            i = i + 1
         enddo
         inpha(nfiles) = string(:i-1)
         if(chatter.GT.20) then
            message = ' ......'//inpha(nfiles)
            call fcecho(message)
         endif
c ........ try and read the next record
         read(iunit,'(a)',iostat=ierr) string
      enddo

c Close the ASCII i/p file, freeing the unit
      close(iunit)

c Check that we actually found something
      if(nfiles.EQ.0) then
         message = errstr//' No files found'
         call fcecho(message)
         ierr = 1
         return
      else
         ierr = 0
         if(chatter.GT.5) then
            write(message,'(a,i12)') 
     &           ' ...... No. PHAs found in ASCII file: ', nfiles
            call fcecho(message)
         endif
      endif

      return
      end
c -------------------------------------------------------------------------
*+WTLIST
      subroutine wtlist(outfil, chatter, nfiles,
     &     rmfil, weight, ierr)

      IMPLICIT NONE
      integer chatter, ierr
      integer nfiles
      real weight(*)
      character*(*) outfil, rmfil(*)
c 
c Description:
c  Subroutine to open & write the o/p ASCII file required for ADDRMF 
c
c Passed parameters
c  OUTFIL            i   : Name of o/p ASCII file 
c  CHATTER            i   : Chatter flag
c  NFILES            i   : Actual no. RMF files
c  RMFIL             i   : Array of RMF filenames
c  WEIGHT            i   : Array of weighting factors
c  IERR                    o : Error flag (zero = OK)
c
c Origin:
c  Original - loosely based on Keith Arnaud's rdlstf.f subroutine 
c supplied as part of the ftools/caltools/addrmf.
c
c Called Routines
c  subroutine CGETLUN      :(CALLIB) Gets free FORTRAN unit
c  subroutine FCECHO    :(FTOOLS) Writes to STDOUT
c  subroutine CRMVLBK   :(CALLIB) Removes leading blanks from string
c
c Authors/Modification History:
c  Ian M George     (1.0.0:95 Apr 19), original
      character(7) version
      parameter (version = '1.0.0')
*- 
c Internals
      integer ounit, i, ilen, clenact
      character(40)  errstr, wrnstr
      character(80) string
      character(80)  message
c Initialize
      errstr = '** WTLIST '//version//' ERROR: '
      wrnstr = '** WTLIST '//version//' WARNING: '
      ierr = 0
      
c Give a bit of banter to the keen ones...
      if(chatter.GE.20) then
         message = ' ... using WTLIST ' // version
         call fcecho(message)
      endif

c Get a free FORTRAN unit & open up the bugger
      call cgetlun(ounit)
      open(ounit,file=outfil,status='new',iostat=ierr)
      if(ierr.NE.0) then
         message = errstr//' Unable to open ASCII file'
         call fcecho(message)
         return
      endif

c Read in the first line of the asciifil
      do i = 1, nfiles
         call crmvblk(rmfil(i))
         ilen = clenact(rmfil(i))
         write(string,'(a,a,g12.6)')
     &        rmfil(i)(:MIN(ilen,len(string)-13)), ' ', weight(i)
         write(ounit,'(a)',iostat=ierr) string
         if(ierr.NE.0) then
            message = errstr//' Unable to write ASCII file'
            call fcecho(message)
            if(chatter.GT.20) then
               write(message,'(a,i12)') 
     &              ' ...... Failed on line: ', i
               call fcecho(message)   
            endif
            return            
         endif
      enddo

c Close the ASCII o/p file, freeing the unit
      close(ounit)

      return
      end
c -------------------------------------------------------------------------
