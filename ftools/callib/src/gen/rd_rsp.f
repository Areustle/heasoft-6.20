*+RD_RSP
      subroutine rd_rsp(rspfil, chatter, maxhist, nk_hist, hist, 
     &                  maxcomm, nk_comm, Comment, telescop, instrume,
     &                  filter, area, maxchan, ichan, maxne, ienerg, 
     &                  energ_lo, energ_hi, imaxgrp, ngrp, F_chan, 
     &                  N_chan, matrix, iebound, e_min, e_max, errstat)
      IMPLICIT NONE
      integer maxhist, maxcomm
      integer maxchan, maxne, imaxgrp
      integer chatter, errstat
      integer nk_hist, nk_comm
      integer ipha, ichan, ienerg, iebound
      integer ngrp(imaxgrp)
      integer F_chan(maxne,imaxgrp), N_chan(maxne,imaxgrp)
      real area
      real energ_lo(maxne), energ_hi(maxne), matrix(maxchan,maxne)
      real e_min(maxchan), e_max(maxchan)
      character*(*) telescop, instrume, filter
      character*(*) rspfil
      character*(*) hist(maxhist), Comment(maxcomm)
c
c Description:
c  Reads old-style (SF) RSP matrices based on the (old) RDRSP program, via
c  the hacked BBRSP program to convert BBXRT detector response matrices
c  from SF to FITS format (credits given below).
c  The structure of the RDRSP program has been left fairly intact, but
c  a number of "old" SF routines have been copied, inserted & (slightly)
c  rewritten.
c  Messy, and extremely unsophisticated, but moderately straightforward
c  !!! NOTE !!! This routine uses XPI, and assumes an XPI par file
c               has been defined & opened (via TBLDSTAND)
c
c Passed parameters
c  RSPFIL        i   : name of the SF RSP file to be read in
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  MAX_HIST       i   : max no. of history strings allowed
c  NK_HIST         o : No. of history keyword strings found
c  HIST            o : Array of history keyword strings
c                       (mainly History Records from RSP file)
c  MAX_COMM       i   : max no. of comment strings allowed
c  NK_COMM         o : No. comments keyword strings
c  COMMENT         o : Array of comment keyword strings
c                       (All other terminal o/p originally produced by RDRSP)
c  TELESCOP        o : Attempt at finding Telescope/Mission Name
c                       (written to par file & user prompted as check)
c  INSTRUME        o : Attempt at finding Instrument/Detector Name
c                       (written to par file & user prompted as check)
c  FILTER          o : Filter in use - currently assumed to be 'NONE'
c                       (written to par file & user prompted as check)
c  AREA            o : Response Effective Area
c  MAXCHAN       i   : max no. of PHA channels allowed
c  ICHAN           o : Actual Number of PHA channels in full matrix
c  MAXNE         i   : max no. of energy bins allowed
c  IENERG          o : Actual Number of energy bins in full matrix
c  ENERG_LO        o : Array containing min energy for each Energy bin
c  ENERG_HI        o : Array containing max energy for each Energy bin
c  IMAXGRP       i/o : Max no. grps in any given row
c                  (must be set to max allowed on i/p)
c  NGRP            o : Array containing no. Channel subsets for each Energy
c  F_CHAN          o : Array containing number of the 1st chan in each
c                       Channel subset for each energy
c  N_CHAN          o : Array containing number of channels in each
c                       Channel subset for each energy
c  MATRIX          o : Array containing the ** full ** response matrix
c  IEBOUND         o : Number of channels in E_MIN & E_MAX arrays
c  E_MIN           o : Array containing Lower energy bound for each channel
c  E_MAX           o : Array containing Upper energy bound for each channel
c  ERRSTAT         o : Error Flag (0 if everything OK)
c
c User i/ps required (prompted for from par file):
c  None
c
c Include files
c  None - responsesf.inc (version 16-OCT-1992) has been hardwired in
c         (for SF response file package definitions)
c
c Called Routines:
c  subroutine FCECHO     : (FTOOLS) Writes to standard o/p device
c  subroutine CGETLUN    : (CALLIB) Gets free i/o unit number
c  subroutine CNXPKSF    : (CALLIB) Decodes SF package header
c  subroutine COPNRSF    : (CALLIB) Opens SF RSP file
c  subroutine PCLGST     : (XPI) Gets string from parameter file
c  subroutine PCLPST     : (XPI) Puts string to parameter file
c  subroutine CRMVLBK    : (CALLIB) Removes leading blanks from a string
c  subroutine CRSUBSF    : (CALLIB) Reads a single subsidiary SF file record
c
c Compilation & Linking
c  link with CALLIB, FTOOLS & XPI
c
c Origin:
c  RDRSP & BBRSP as described above & below
c
c Authors/Modification History:
c  M A Sweeny       (1986 May 02), RDRSP original
c  R A Shafer          (1986 May 28), unknown RDRSP modifications
c  Alan Smale       (Sept/Oct 92), original BBRSP version
c  Ian M George     (1.0.0:1992 Oct 13), tidied-up version
c  Ian M George     (1.1.0:1993 Feb 18), fixed i/o
c  Ian M George     (1.1.1:1993 May 19), fixed stupid string probs
c  Ian M George     (1.1.2:1993 Jun 21), fixed inst/det decoupling
c  Ian M George     (1.1.3:1993 Jul 19), fixed I*2 nightmare
c  Ian M George     (1.1.4:1993 Aug 10), changes to COPNRSF
c  Ian M George     (1.2.0:1994 May 10), fixed bug wioth energ_lo(1)
c  Jeff Guerber (1.2.1: 1998-02-17) Made char args (esp rspfil) assumed-size
c
      character(7) version
      parameter (version = '1.3')
*-

c Max Array Sizes
      integer*4 maxsize, maxchanen, mxsize4
      parameter (maxchanen=4096,maxsize=32768,mxsize4=8192)
c Internals
      integer iunit, ierrsf, irec, lr, lc
      integer i, j, l, ie, igrp
      integer lenbuf, length, len, lent, k, fcstln
      integer nhist, index, nsubs, infoar(4)
      integer binrng(2,maxchanen)
      real inresp(maxchanen)
      logical qdone
      character*(12) pkgtyp
      character(80) header, tmplte, message
      character(255) wrtstr, cbuf
      character(80) txtstr
      character(30)  errstr, wrnstr

c buffers used to read packages

      character(1) buffer(maxsize)
      real*4 rbuffer(mxsize4)
      integer*4 ibuffer(mxsize4)
      equivalence (buffer, rbuffer, ibuffer)

      errstr = '** RD_RSP '//version//' ERROR: '
      wrnstr = '** RD_RSP '//version//' WARNING: '

c START MAIN
      errstat = 0

c Give user info if requested
      if(chatter.GE.20) then
         message = ' ... using RD_RSP '// version
         call fcecho(message)
      endif

c Open the rsp file
      call cgetlun(iunit)
      if(iunit.LE.0) then
         message = errstr// ' Fatal'
         call fcecho(message)
         errstat = 3
         return
      endif

      call copnrsf(rspfil, iunit, 'XSPEC rspnse', header, tmplte, nhist,
     &             ierrsf)
      if(ierrsf.NE.0) then
         errstat = 1
         go to 986
      endif

c Dump initial info into history array
      write(hist(1),'(a)') ' RSP file (SF) history records'
      write(txtstr,'(2a)') ' - RSP file header:',
     &                        header(27:fcstln(header))
      hist(2) = txtstr
      write(txtstr,'(a,i12)') ' - No. of history records: ', nhist
      hist(3) = txtstr(1:70)
      nk_hist=3

c Dump (old) SF history records to the user & into (new) history array
c --- This code taken almost directly from PTXTSF -------------------------
      if(nhist.NE.0) then
         nk_hist = nk_hist + nhist
         irec = 0
         do while(irec.NE.nhist)
            read(iunit) lr,(buffer(i),i=1,min(maxsize,lr))
            if(lr.gt.0) then
               irec=irec+1
               lc=min(maxsize,lr)
               if(lr.gt.lc) then
                  if(chatter.GE.10) then
                     write(message,'(2a,i12,a)') wrnstr,
     &                              'Record too full:',lr,
     &                              'some text lost'
                     call fcecho(message)
                  endif
               endif
               cbuf = ' '
               do i = 1, lc
                  write(cbuf(i+1:i+1),'(a1)') buffer(i)
               enddo
               write(wrtstr,'(2a)') ' -',cbuf(:lc+1)
               hist(3+irec) = wrtstr(:70)
            else
               irec = nhist
               if(chatter.GE.10) then
                  write(message,'(2a)') wrnstr,
     &                              'Badly terminated package'
                  call fcecho(message)
               endif
            endif
         enddo
      endif
c -------------------------------------------------------------------------

c Give user info if requested
      if(chatter.GE.10) then
         do i = 1, nk_hist
            write(message,'(a)') hist(i)
            call fcecho(message)
         enddo
      endif

c Write additional history string concerning this programme
      write(txtstr,'(2a)') ' RSP file read by RD_RSP ',
     &                        version
      nk_hist = nk_hist + 1
      hist(nk_hist) = txtstr(:70)

c Start reading in the packages from the SF RSP file
      qdone = .false.
      comment(1) = ' '
      write(comment(2),'(a)') ' RSP file (SF) package headers'
      nk_comm = 2
      do while ( .NOT.qdone)
         ierrsf = 0
         lenbuf = maxsize
         pkgtyp = ' '
         index = 0
         nsubs = 0
         call cnxpksf(iunit, pkgtyp, index, nsubs,
     &                infoar, buffer, lenbuf, .FALSE., ierrsf)
         qdone = ierrsf.EQ.7
         if( .NOT.qdone) then
            IF(pkgtyp.EQ.'detect info ') then
c -- Detector Info package
               instrume = ' '
               do i = 1, 16
                  instrume(i:i) = buffer(i)
               enddo
               ipha  = ibuffer(5)
               ichan = ibuffer(6)
               area = rbuffer(7)
               ienerg = ibuffer(8)
               energ_lo(1) = rbuffer(9)
c ... check everything is hunky
               if(ipha.GT.ichan) then
                  write(message,'(2a,i12,a)') errstr,
     &                'No. stored chans > max allowed (', ichan,')'
                  call fcecho(message)
                  errstat = 2
                  return
               endif
c ... check arrays are not too large
               if(ienerg.GT.maxne) then
                  write(message,'(2a,i12,a)') errstr,
     &                'No. energy bins > max allowed (', maxne,')'
                  call fcecho(message)
                  errstat = 2
                  return
               elseif(ichan.GT.maxchan) then
                  write(message,'(2a,i12,a)') errstr,
     &                'No. PHA channels > max allowed (', maxchan,')'
                  call fcecho(message)
                  errstat = 2
                  return
               endif
c ... write o/p in same style as RDRSP produced into comment array
               write(txtstr,'(4a)')' ***** Package "',pkgtyp,
     &                     '" *****', '  (read successfully)'
               if(chatter.GE.10) call fcecho(txtstr)
               comment(nk_comm+1) = txtstr
               write(comment(nk_comm+2),'(2a)')
     &                    ' Detector identity      :',instrume
               write(comment(nk_comm+3),'(a,i12)')
     &                    ' No. of pha entries     :', ipha
               write(comment(nk_comm+4),'(a,i12)')
     &                    ' No. of pha channels    :', ichan
               write(comment(nk_comm+5),'(a,f10.4)')
     &                    ' Response effective area:',area
               write(comment(nk_comm+6),'(a,i12)')
     &                    ' No. of energy ranges   :', ienerg
               write(comment(nk_comm+7),'(a,1pg11.5)')
     &                    ' Matrix low energy limit:  ',energ_lo(1)
               nk_comm = nk_comm + 7
c ...    Give user info if requested
               if(chatter.GE.10) then
                  do i = 1, nk_comm
                     message = comment(i)
                     call fcecho(message)
                  enddo
               endif
            ELSEIF(pkgtyp.EQ.'chanel bound') then
c -- Channel Boundaries
               write(txtstr,'(4a)')' ***** Package "',pkgtyp,
     &                     '" *****', '  (read successfully)'
               if(chatter.GE.10) call fcecho(txtstr)
               nk_comm = nk_comm + 1
               comment(nk_comm) = txtstr
               iebound = lenbuf/8
               do j = 1, iebound
                  e_min(j) = rbuffer(2*j-1)
                  e_max(j) = rbuffer(2*j)
               enddo
            ELSEIF(pkgtyp.EQ.'response   ') then
c -- Response Matrix
               write(txtstr,'(4a)')' ***** Package "',pkgtyp,
     &                     '" *****', '  (read successfully)'
               if(chatter.GE.10) call fcecho(txtstr)
               nk_comm = nk_comm + 1
               comment(nk_comm) = txtstr
               ienerg = nsubs/3
               length = 12
               imaxgrp = 1
               do ie = 1, ienerg
                  call crsubsf(iunit, buffer, length, ierrsf)
                  ngrp(ie) = ibuffer(2)
                  imaxgrp = MAX(imaxgrp,ngrp(ie))
                  len = max(8*ngrp(ie),1)
                  lent= max(4*ibuffer(3),1)
                  call crsubsf(iunit, binrng, len, ierrsf)
                  call crsubsf(iunit, inresp, lent, ierrsf)
c ... The energies
                  if(ie.GT.1) then
                     energ_lo(ie) = energ_hi(ie-1)
                  endif
                  energ_hi(ie) = rbuffer(1)
c ... The grouping & matrix

                  k = 0
                  do igrp = 1, ngrp(ie)
                     F_chan(ie,igrp) = binrng(1,igrp)
                     N_chan(ie,igrp) = binrng(2,igrp)
     &                          - binrng(1,igrp) + 1
                     do l = F_chan(ie,igrp),binrng(2,igrp)
                        k = k+1
                        matrix(l,ie) = inresp(k)
                     enddo
                  enddo
               enddo
            else
               write(txtstr,'(4a)')' ***** Package "',pkgtyp,
     &                     '" *****', '  (ignored)'
               nk_comm = nk_comm + 1
               comment(nk_comm) = txtstr
               if(chatter.GE.10) then
                  call fcecho(txtstr)
                  txtstr = '     (skipping it)'
                  call fcecho(txtstr)
               endif
            endif
         endif
      enddo

c Close the RSP file
      close(iunit)

c Fix a few sillies
c - Break up the RSP-supplied instrument name into 2 halves, in an
c   attempt to decouple the telescope/mission from the detector
      call crmvlbk(instrume)
      lc = fcstln(instrume)
      if(lc.GT.0) then
         do i = 1, lc
            if(instrume(i:i).EQ.' ') then
               lc = i-1
               goto 765
            endif
         enddo
765      telescop = instrume(1:lc)
         instrume = instrume(lc+1:)
         call crmvlbk(instrume)
      else
         telescop = ' '
         instrume = ' '
      endif

986   if(errstat.ne.0) then
      message = errstr // 'Incomplete execution'
      call fcecho(message)
      endif

      return
      end
