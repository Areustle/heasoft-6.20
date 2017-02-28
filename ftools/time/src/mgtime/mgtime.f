C Ftools info: $Header: /headas/headas/ftools/time/src/mgtime/mgtime.f,v 3.27 2015/09/25 19:48:46 peachey Exp $
C******************************************************************************
C SELECTOR TASK:
C      mgtime
C
C FILE:
C      mgtime.f
C
C DESCRIPTION:
C      Merges two Good Time Interval files, using two modes:
C      AND - output GTI file contains time intervals common to both files,
C      OR  - output GTI file combines time intervals from both files.
C
C AUTHOR:
C      Janice Tarrant  4/1/92
C
C MODIFICATION HISTORY:
C       February, 1993 J. Ingham modified to make fimerg work for .OR.
C       2/22/93 EAG - allow for double precision time
C       5/11/93 EAG - allow for no start time checking
C       10/31/1996 (B. Seifert) - maxgti changed to 150 from 50
C       2/3/1997 ((B. Seifert) - maxgti changed to 400
C                                maxcl changed to 10 suggetsed by
C                                Bill pence
C       4/14/1997 Jeff Guerber - In fimerg, remove 0-length GTIs from the list
C       8/1/1997 Jeff Guerber - In fimerg, fix bug in OR section
C       9-10/1997 Jeff Guerber - 3.0: Replaced routine fimerg, which had lots
C            of bugs, with call to library routine gtimerge (gtilib.f).  Merged
C            fmgtkw.  Extensive reorganization, to make it simpler, easier to
C            follow, and less redundant (but more could be done!).  Handle
C            TIMEZERO keywords.  Lots of minor bug fixes.
C       8/16/2001 (MJT) Fixed bug where non-existent TIMEZERO keyword yields
C             random value for timezero variable.
C
C $Log: mgtime.f,v $
C Revision 3.27  2015/09/25 19:48:46  peachey
C Update by hand the version number this tool reports to match what will now be the latest.
C
C Revision 3.26  2015/09/25 19:32:59  peachey
C Increase maximum number of rows in gti to 2.5^6 to accommodate Astro-H data.
C Clean up code a little to silence warnings.
C
C Revision 3.25  2013/05/21 19:08:41  irby
C Change character*n to character(n) to silence warnings: "Obsolescent
C feature: Old-style character length".
C
C Revision 3.24  2009/09/03 16:19:37  irby
C Increase maximum number of rows in GTI files to 200000 following request
C from Lorella.
C
C Revision 3.23  2007/12/11 19:52:44  miket
C added precision to 'read' format to match xftgkys (g40.17)
C
C Revision 3.22  2007/12/04 18:50:06  miket
C Increased precision of output MJDREF keyword
C
C Revision 3.21  2002/12/26 18:27:47  irby
C Split data statements out of variable declarations for f90 compatibility.
C
C Revision 3.20  2001/08/16 19:09:43  miket
C fixed bug where timezero variable gets garbage when TIMEZERO kwd not present
C
C Revision 3.19  1998/10/29 21:33:59  ngan
C Write the  ref. date keyword with more digits(More precise?).
C
c Revision 3.18  1998/10/29  19:56:27  ngan
c Change the format of the refdate.
c
c Revision 3.17  1998/10/14  20:39:27  miket
c Added HDUCLAS? keywords to output GTI
c
C Revision 3.16  1998/10/06 01:24:38  guerber
C Warn user when empty GTI lists are ignored.
C
C Revision 3.15  1998/09/24 04:49:10  guerber
C Make sure ndate/ntime set even if there was only one good file, else crash
C in ftpkys.  Set istarts(first) if nstarts=1, since first is not necessarily
C 1 (same with istops).
C
C Revision 3.14  1998/07/02 01:56:44  guerber
C fmgkyw: y2k - determine julian with fcisjul(), which knows both date fmts
C fimgti: call ftmahd not ftmrhd due to recent fitsio changes
C
C Revision 3.13  1998/03/12 01:35:15  guerber
C Increased string sizes for most of the parameters, esp. ingtis.
C
C Revision 3.12  1998/03/11 01:35:00  guerber
C Check that INDATES, INTIMES, INSTARTS, INSTOPS agree w/number of input files
C
C Revision 3.11  1997/10/22 19:10:10  guerber
C Sun f77 didn't catch: negflag decl. twice; need neqv not ne in julian compare
C
C Revision 3.10  1997/10/22 00:51:53  guerber
C Use gtimerge (gtilib.f) for merge operations instead of buggy routine fimerg.
C Handle TIMEZEROs.  Extensive reorganization, should make (better) sense now.
C Many bug/inconsistency fixes.
C
C
C NOTES:
C      mgtime supported in IRAF and HOST environments
C
C      output reference-date/time keyword names are the *first* ones
C         from indates/intimes parameters, even if that file wasn't used.
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      ingtis   - input GTI file names
C      instarts - names of columns for input start times
C      instops  - names of columns for input stop times
C      indates  - input observation start date keywords
C      intimes  - input observation start time keywords
C      outgti   - output GTI file name
C      outstart - name of column for output start time
C      outstop  - name of column for output stop time
C      mode     - merge mode
C
C CALLED ROUTINES:
C      subroutine gmgtie - gets parameters from environment
C      subroutine fimgti - merge the GTI files
C
C******************************************************************************
        subroutine mgtime
        implicit none
        character(512) ingtis
        character(160) outgti, instarts, instops, indates, intimes,
     &               outstart, outstop
        character(10) mode
        character(40) taskname
        common /task/ taskname
        character(70) rcsid
        data rcsid
     &       /'$Id: mgtime.f,v 3.27 2015/09/25 19:48:46 peachey Exp $'/

        taskname = 'mgtime3.27'
        ingtis = ' '
        outgti = ' '
        mode = ' '

C  get the parameters from the par file
        call gmgtie(ingtis,instarts,instops,indates,intimes,outgti,
     &              outstart,outstop,mode)

C  read in the GTI files and write out the merged GTI file
        call fimgti(ingtis,instarts,instops,indates,intimes,outgti,
     &              outstart,outstop,mode)

        return
        end

C******************************************************************************
C SUBROUTINE:
C      gmgtie
C
C DESCRIPTION:
C      Get parameters from parameter file
C
C AUTHOR:
C      Janice Tarrant  4/1/92
C
C MODIFICATION HISTORY:
C      Jeff Guerber 9-10/1997. Check mode here not in fimgti.
C
C NOTES:
C      gmgtie uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gmgtie(ingtis,instarts,instops,indates,intimes,outgti,outstart,
C                  outstop,mode)
C
C ARGUMENTS:
C      ingtis   - input GTI file names
C      instarts - names of columns for input start times
C      instops  - names of columns for input stop times
C      indates  - input observation start date keywords
C      intimes  - input observation start time keywords
C      outgti   - output GTI file name
C      outstart - name of column for output start time
C      outstop  - name of column for output stop time
C      mode     - merge mode
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C      subroutine uclgst - get string parameter
C
C******************************************************************************
        subroutine gmgtie(ingtis,instarts,instops,indates,intimes,
     &                    outgti,outstart,outstop,mode)

        implicit none
        character*(*) ingtis, instarts, instops, indates, intimes,
     &               outgti, outstart, outstop, mode

        character(80) context
        integer status

C  initialize variables
        status = 0

C  get the names of the input GTI files
        call uclgst('ingtis',ingtis,status)
        if (status .ne. 0) then
            context = 'could not get INGTIS parameter'
            call fcerr(context)
            goto 999
        endif

C  get the name of the output GTI file
        call uclgst('outgti',outgti,status)
        if (status .ne. 0) then
            context = 'could not get OUTGTI parameter'
            call fcerr(context)
            goto 999
        endif

C  get & check the merge mode
        call uclgst('merge',mode,status)
        if (status .ne. 0) then
            context = 'could not get MERGE parameter'
            call fcerr(context)
            goto 999
        endif
        call ftupch(mode)
        if ((mode .ne. 'AND') .and. (mode .ne. 'OR')) then
            context = 'merge mode must be AND or OR'
            call fcerr(context)
            goto 999
        endif

C  get the input start times column names
        call uclgst('instarts',instarts,status)
        if (status .ne. 0) then
            context = 'could not get INSTARTS parameter'
            call fcerr(context)
            goto 999
        endif

C  get the input stop times column names
        call uclgst('instops',instops,status)
        if (status .ne. 0) then
            context = 'could not get INSTOPS parameter'
            call fcerr(context)
            goto 999
        endif

C  get the input start date keywords
        call uclgst('indates',indates,status)
        if (status .ne. 0) then
            context = 'could not get INDATES parameter'
            call fcerr(context)
            goto 999
        endif

C  get the input start time keywords
        call uclgst('intimes',intimes,status)
        if (status .ne. 0) then
            context = 'could not get INTIMES parameter'
            call fcerr(context)
            goto 999
        endif

C  get the output start times column name
        call uclgst('outstart',outstart,status)
        if (status .ne. 0) then
            context = 'could not get OUTSTART parameter'
            call fcerr(context)
            goto 999
        endif

C  get the output stop times column name
        call uclgst('outstop',outstop,status)
        if (status .ne. 0) then
            context = 'could not get OUTSTOP parameter'
            call fcerr(context)
            goto 999
        endif

999     continue
        if (status .ne. 0)  call fcerrm(status)

        return
        end

C******************************************************************************
C SUBROUTINE:
C      fimgti
C
C DESCRIPTION:
C      Reads the GTI files and writes the merged GTI to a new file
C
C AUTHOR:
C      Janice Tarrant  4/1/92
C
C MODIFICATION HISTORY:
C      Jeff Guerber, 9-10/1997. Extensive reorganization. Call gtimerge instead
C        of local buggy fimerg. Handle TIMEZERO. Lots of bug/inconsistency
C        fixes (esp for >2 files). Write filenames (not numbers) in error msgs.
C      Jeff Guerber, 1998-07-01. Call ftmahd not ftmrhd due to fitsio changes
C        (may not really be necessary now).
C
C NOTES:
C      fimgti uses FITSIO calls to read FITS file
C      vector elements in tables not supported
C
C USAGE:
C      call fimgti(ingtis,instarts,instops,indates,intimes,outgti,outstart,
C                  outstop,mode)
C
C ARGUMENTS:
C      ingtis   - input GTI file names
C      instarts - names of columns for input start times
C      instops  - names of columns for input stop times
C      indates  - input observation start date keywords
C      intimes  - input observation start time keywords
C      outgti   - output GTI file name
C      outstart - name of column for output start time
C      outstop  - name of column for output stop time
C      mode     - merge mode
C
C PRIMARY LOCAL VARIABLES:
C      maxcl    - maximum number of columns
C      maxgti   - maximum number of GTI files
C      maxsz    - maximum number of rows in GTI files
C      infile   - list of input files
C      nfile    - number of input files
C      filename - name of FITS file
C      extnum   - FITS file extension number
C      ftstatus - FITSIO error number
C      context  - error message
C      istarts  - list of start time keywords
C      nstarts  - number of start time keywords
C      istops   - list of stop time keywords
C      nstops   - number of stop time keywords
C      idates   - list of observation date keywords
C      ndates   - number of observation date keywords
C      itimes   - list of observation time keywords
C      ntimes   - number of observation time keywords
C      start    - merged GTI start times
C      stop     - merged GTI stop times
C      mnrows   - current number of times in merged GTI list
C
C CALLED ROUTINES:
C      subroutine fccmpl - compare two lists for a subset
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - report an error number to terminal
C      subroutine fcgcls - get column list based on parameter
C      subroutine fcpars - parse off filename and extension number
C      subroutine fctofs - get the observation and GTI time offsets
C      subroutine fighst - get the history records
C      subroutine fipdtm - put a date and time keyword into the CHU
C      subroutine ftadef - define ASCII extension
C      subroutine ftbdef - define binary extension
C      subroutine ftclos - close a FITS file
C      subroutine ftcrhd - create a new header
C      subroutine ftgbnh - get BINARY table header
C      subroutine ftgcno - get column number associated with name
C      subroutine xftgkys - get string keyword value (like ftgkys, but checks
C                              for KEYI/KEYF pairs first; from misc.for)
C      subroutine ftgtbh - get ASCII table header
C      subroutine ftinit - create a new FITS file
C      subroutine ftmahd - absolute move to FITS header
C      subroutine ftopen - open a FITS file
C      subroutine ftphbn - put binary table header keywords into CHU
C      subroutine ftpcle - put real column values
C      subroutine ftpdef - define structure of primary array
C      subroutine ftphis - put history keyword record
C      subroutine ftphpr - put primary header keywords into CHU
C      subroutine ftphtb - put ASCII table header keywords into CHU
C      subroutine gtimerge - merge two GTI lists (from gtilib.f)
C
C******************************************************************************
        subroutine fimgti(ingtis,instarts,instops,indates,intimes,
     &                    outgti,outstart,outstop,mode)
        implicit none
        character*(*) ingtis, instarts, instops, indates, intimes,
     &               outgti, outstart, outstop, mode

        integer maxcl, maxgti, maxsz
ccc        parameter (maxcl=512)
ccc        parameter (maxgti=50)
        parameter (maxcl=10)
        parameter (maxgti=400)
        parameter (maxsz = 2500000)
        character(160) infile(maxgti), filename(2)
        character(80) context,
     &               istarts(maxgti), istops(maxgti), idates(maxgti),
     &               itimes(maxgti), history(maxgti)
        character(70) ttype1(maxcl), ttype2(maxcl), tform1(maxcl),
     &               tform2(maxcl), tunit1(maxcl), tunit2(maxcl),
     &               extname(2), date(2), time(2), ndate, ntime,
     &               comment,mtype(2),mform(2),munit(2),mextname,
     &               mhduclass,mhduclas1,mhduclas2
        logical inopen(2), outopen, exact, negflag, goodlist, simple,
     &      extend, dates, julian, julian2, anyf, flagvals(maxsz)
        integer j,i,ftstatus,iunit(2),ounit,nfiles,extnum(2), block,
     &          htype(2),rowlen(2),nrows(2),tfields(2),tbcol1(maxcl),
     &          tbcol2(maxcl), varidat, nstarts,nstops,colnum(2,2),
     &          ndates, ntimes, mnrows, mrowlen, mfields, mbcol(2),
     &          nhists, bitpix, naxis, naxes(99), pcount,
     &          gcount, length,fcstln, first, gtistatus
        double precision offs(2), start(maxsz), stop(maxsz),
     &      start2(maxsz), stop2(maxsz), timezero
        
	integer is_string
	double precision  num_ref_date

C  initialize variables
        gtistatus = 0
        ftstatus = 0
        iunit(1) = 15
        iunit(2) = 16
        ounit = 17
        inopen(1) = .false.
        inopen(2) = .false.
        outopen = .false.
        negflag = .false.
        exact = .false.
        first = 1
        mnrows = 0

C  get the input GTI file names and first file's extension number
        call fcgcls(ingtis,infile,nfiles,negflag)
        if (nfiles .gt. maxgti) then
            context = 'maximum number of input files is 400'
            call fcerr(context)
            goto 999
        endif
        if (nfiles .le. 1) then
            context = ' minimum number of input files is 2'
            call fcerr (context)
            goto 999
        endif

C       Split input reference date/time keyword name list
C       check for no requested indate
        if ((indates .eq. ' ') .or. (indates .eq. '-')) then
            dates = .false.
            ndates = 1
            ntimes = 1
        else
            dates = .true.
            call fcgcls(indates,idates,ndates,negflag)
            if (ndates .gt. maxgti) then
                context = 'maximum number of dates is 400'
                call fcerr(context)
                goto 999
            endif
            if ( (ndates .gt. 1) .and. (ndates .ne. nfiles) ) then
                context='INDATES parameter doesn''t match number of '
     &              // 'input files'
                call fcerr(context)
                goto 999
            endif

            call fcgcls(intimes,itimes,ntimes,negflag)
            if (ntimes .gt. maxgti) then
                context = 'maximum number of times is 400'
                call fcerr(context)
                goto 999
            endif
            if ( (ntimes .gt. 1) .and. (ntimes .ne. nfiles) ) then
                context='INTIMES parameter doesn''t match number of '
     &              // 'input files'
                call fcerr(context)
                goto 999
            endif
        endif

C       Parse and check INSTARTS, INSTOPS parameters
        call fcgcls(instarts,istarts,nstarts,negflag)
        if (nstarts .gt. maxgti) then
            context = 'maximum number of start times is 400'
            call fcerr(context)
            goto 999
        endif
        if ( (nstarts .ne. 1) .and. (nstarts .ne. nfiles) ) then
            context = 'INSTARTS parameter doesn''t match'
     &          // ' number of input files'
            call fcerr(context)
            goto 999
        endif

        call fcgcls(instops,istops,nstops,negflag)
        if (nstops .gt. maxgti) then
            context = 'maximum number of stop times is 400'
            call fcerr(context)
            goto 999
        endif
        if ( (nstops .ne. 1) .and. (nstops .ne. nfiles) ) then
            context = 'INSTOPS parameter doesn''t match'
     &          // ' number of input files'
            call fcerr(context)
            goto 999
        endif

C       COME BACK HERE IF TABLE IN FIRST FILE IS EMPTY
    5   call fcpars(infile(first),filename(1),extnum(1),ftstatus)

C EAG 8/25/93 default to 1st extension
        if (extnum(1) .eq. -99) extnum(1) = 1

C  exit if extension is zero
        if (extnum(1) .eq. 0) then
            context = 'primary extension not supported, '//infile(first)
            call fcerr(context)
            goto 999
        endif

C  open first input GTI file
        call ftopen(iunit(1),filename(1),0,block,ftstatus)
        if (ftstatus .ne. 0) then
            context = 'unable to open first GTI file, '//filename(1)
            call fcerr(context)
            goto 999
        endif
        inopen(1) = .true.

C  move to the extension in the input file
        call ftmahd( iunit(1), extnum(1)+1, htype(1), ftstatus )
        if (ftstatus .ne. 0) then
           context = 'error moving to extension in first GTI file, '
     &          //filename(1)
           call fcerr(context)
           goto 999
        endif

C  get the header depending on the extension type (1=ASCII, 2=binary)
        if (htype(1) .eq. 1) then
            call ftghtb(iunit(1),maxcl,rowlen(1),nrows(1),tfields(1),
     &          ttype1,tbcol1,tform1,tunit1,extname(1),ftstatus)
        else
            call ftghbn(iunit(1),maxcl,nrows(1),tfields(1),ttype1,
     &          tform1,tunit1,extname(1),varidat,ftstatus)
        endif

C       Check size of GTI files. If empty, loop back to try the next one.
C       If we run out, go straight to output section (label 55) (mnrows=0).
        if (nrows(1) .gt. maxsz) then
            context = 'GTI file larger than allowed max, '//filename(1)
            call fcerr(context)
            goto 999
        endif
        if (nrows(1) .le. 0) then
            context = 'mgtime: Warning, ignoring empty GTI list in ' //
     &          infile(first)
            call fcecho(context)
            first = first + 1
            if (nfiles .lt. first) goto 55
            call ftclos (iunit(1), ftstatus)
            inopen(1) = .false.
            goto 5
        endif

C       get column names and positions in the GTI file
        if (nstarts .eq. 1)  istarts(first) = istarts(1)
        if (nstops .eq. 1)  istops(first) = istops(1)
        call fccmpl(1,tfields(1),istarts(first),ttype1,negflag,goodlist)
        if (.not. goodlist) then
            context = 'error in GTI file column names, '//filename(1)
            call fcerr(context)
            goto 999
        endif
        call fccmpl(1,tfields(1),istops(first),ttype1,negflag,goodlist)
        if (.not. goodlist) then
            context = 'error in GTI file column names, '//filename(1)
            call fcerr(context)
            goto 999
        endif
        call ftgcno(iunit(1),exact,istarts(first),colnum(1,1),ftstatus)
        call ftgcno(iunit(1),exact,istops(first),colnum(1,2),ftstatus)

C       get keyword values and determine reference time system
        if (dates) then
            if (ndates .eq. 1) idates(first) = idates(1)
            if (ntimes .eq. 1) itimes(first) = itimes(1)
            call fmgtkw (iunit(1), idates(first), itimes(first),
     &           filename(1), date(1), time(1), julian, ftstatus)
            if (ftstatus .ne. 0) goto 999
            ndate = date(1)
            ntime = time(1)
        endif

        timezero = 0.d0
        call xftgkyd(iunit(1), 'TIMEZERO', timezero, comment, ftstatus)
c       MUST RESET timezero variable to zero as well as status!
        if (ftstatus .eq. 202) then
           ftstatus = 0
           timezero = 0.d0
        endif

C       Read the data from this file
C       Note: we leave the first file open in case we have to copy the
C       whole HDU to the output file.

        call ftgcfd(iunit(1),colnum(1,1),1,1,nrows(1),start,
     &      flagvals,anyf,ftstatus)
        call ftgcfd(iunit(1),colnum(1,2),1,1,nrows(1),stop,
     &      flagvals,anyf,ftstatus)
        if (ftstatus .ne. 0) then
            context = 'error reading file '//filename(1)
            call fcerr(context)
            goto 999
        endif

        do j = 1, nrows(1)
            start(j) = start(j) + timezero
            stop(j) = stop(j) + timezero
        enddo
        mnrows = nrows(1)

C       add a history keyword
        call fighst(filename(1),nhists,maxgti,history)

C       set various keywords for the merged GTI file (formerly subr. figmkw)
C       (can this move to the output section?)
        mrowlen = rowlen(1)
        mfields = 2
        mtype(1) = outstart
        mtype(2) = outstop
        mbcol(1) = tbcol1(colnum(1,1))
        mbcol(2) = tbcol1(colnum(1,2))
        mform(1) = tform1(colnum(1,1))
        mform(2) = tform1(colnum(1,2))
        munit(1) = 's'
        munit(2) = 's'
        mextname = 'STDGTI'
        mhduclass = 'OGIP'
        mhduclas1 = 'GTI'
        mhduclas2 = 'ALL'

C------------------------------------------------------------------
C       LOOP ON REMAINING FILES

        do 10 i = first+1, nfiles
C  get the second file's name and extension number
            call fcpars(infile(i),filename(2),extnum(2),ftstatus)

C EAG 8/25/93 default to 1st extension
            if (extnum(2) .eq. -99) extnum(2) = 1

C  exit if extension is zero
            if (extnum(2) .eq. 0) then
                context = 'primary extension not supported: '//infile(i)
                call fcerr(context)
                goto 999
            endif

C  open next GTI file
            call ftopen(iunit(2),filename(2),0,block,ftstatus)
            if (ftstatus .ne. 0) then
                context = 'unable to open GTI file: '//filename(2)
                call fcerr(context)
                goto 999
            endif
            inopen(2) = .true.

C  move to the extension in the input file
            call ftmahd( iunit(2), extnum(2)+1, htype(2), ftstatus )
            if (ftstatus .ne. 0) then
                context = 'error moving to extension in GTI file: '
     &              //filename(2)
                call fcerr(context)
                goto 999
            endif

C  check that extension types are the same
            if (htype(1) .ne. htype(2)) then
                context = 'header types are not the same: file '
     &              //filename(2)
                call fcerr(context)
                goto 999
            endif

C  get the header depending on the extension type
            if (htype(2) .eq. 1) then
                call ftghtb(iunit(2),maxcl,rowlen(2),nrows(2),
     &              tfields(2),ttype2,tbcol2,tform2,tunit2,extname(2),
     &              ftstatus)
            else
                call ftghbn(iunit(2),maxcl,nrows(2),tfields(2),ttype2,
     &              tform2,tunit2,extname(2),varidat,ftstatus)
            endif

C  check size of GTI files
            if (nrows(2) .gt. maxsz) then
                context = 'GTI larger than allowed max: file '
     &              //filename(2)
                call fcerr(context)
                goto 999
            endif

C if this file doesn't contain any GTIs, skip it
            if (nrows(2) .le. 0) then
                context = 'mgtime: Warning, ignoring empty GTI list in '
     &              //  infile(i)
                call fcecho(context)
                goto 9
            endif

C  get column names and positions in the second GTI file
            if (nstarts .eq. 1)  istarts(i) = istarts(1)
            if (nstops .eq. 1)  istops(i) = istops(1)
            call fccmpl(1,tfields(2),istarts(i),ttype2,negflag,goodlist)
            if (.not. goodlist) then
                context = 'error in GTI column names: file '
     &              //filename(2)
                call fcerr(context)
                goto 999
            endif
            call fccmpl(1,tfields(2),istops(i),ttype2,negflag,goodlist)
            if (.not. goodlist) then
                context = 'error in GTI column names: file '
     &              //filename(2)
                call fcerr(context)
                goto 999
            endif
            call ftgcno(iunit(2),exact,istarts(i),colnum(2,1),ftstatus)
            call ftgcno(iunit(2),exact,istops(i),colnum(2,2),ftstatus)

C           Get the GTI observation date and time.  fctofs requires that both
C           have same date format (JD vs. yyyy-mm-dd or dd/mm/yy).

            if (dates) then
               if (ndates .eq. 1)  idates(i) = idates(1)
               if (ntimes .eq. 1)  itimes(i) = itimes(1)
               call fmgtkw(iunit(2), idates(i), itimes(i),
     &             filename(2),date(2),time(2),julian2,ftstatus)
               if (ftstatus .ne. 0) goto 999
               if (julian .neqv. julian2) then
                   context = 'ref date has different format, file ' //
     &                 filename(2)
                   call fcerr(context)
                   goto 999
               endif

C  Get the time offset for the GTI observations (min of the ref times,
C  and the difference between them)
               call fctofs(date(1),time(1),date(2),time(2),ndate,ntime,
     &                  offs(1),offs(2), julian, ftstatus)
            else
                offs(1) = 0.D0
                offs(2) = 0.D0
            endif

            timezero = 0.d0
            call xftgkyd(iunit(2),'TIMEZERO',timezero,comment,ftstatus)
c     MUST RESET timezero variable to zero as well as status!
            if (ftstatus .eq. 202) then
               ftstatus = 0
               timezero = 0.d0
            endif

C           Read the new gti

            call ftgcfd(iunit(2),colnum(2,1),1,1,nrows(2),start2,
     &          flagvals,anyf,ftstatus)
            call ftgcfd(iunit(2),colnum(2,2),1,1,nrows(2),stop2,
     &          flagvals,anyf,ftstatus)
            if (ftstatus .ne. 0) then
                context = 'error reading file '//filename(2)
                call fcerr(context)
                goto 999
            endif

C           Add the observation time offsets to the GTI times

            do j = 1, mnrows
                start(j) = start(j) + offs(1)
                stop(j) = stop(j) + offs(1)
            enddo
            do j = 1, nrows(2)
                start2(j) = start2(j) + offs(2) + timezero
                stop2(j) = stop2(j) + offs(2) + timezero
            enddo

C           Merge new GTI into the current list (updates start, stop, mnrows)

            call gtimerge(mode, start, stop, mnrows, maxsz, start, stop,
     &          mnrows, start2, stop2, nrows(2), ftstatus )
            if (ftstatus .ne. 0) then
                context = 'error merging file '//filename(2)
                call fcerr(context)
                goto 999
            endif

C  set the history keywords
            call fighst(filename(2),nhists,maxgti,history)

C  update values, close ith file
            date(1) = ndate
            time(1) = ntime

C           Come here if no gti in this file
    9       call ftclos(iunit(2),ftstatus)
            inopen(2) = .false.
 10     continue

C--------------------------------------------------------------------
C       OUTPUT NEW GTI

C  open the new GTI file
 55     call ftinit(ounit,outgti,block,ftstatus)
        if (ftstatus .ne. 0) then
            context = 'unable to open outgti'
            call fcerr(context)
            goto 999
        endif
        outopen = .true.

C  construct a simple primary header for the merged file
        simple = .true.
        bitpix = 8
        naxis = 0
        extend = .true.
        pcount = 0
        gcount = 1
        call ftphpr(ounit,simple,bitpix,naxis,naxes,pcount,gcount,
     &              extend,ftstatus)

C       Create a new extension
C       If any merged times were found, write them out
C       for AND mode, if only one file was input, no output is needed

C       NOTE: output reference-date/time keyword names are the *first* ones
C       from indates/intimes parameters, even if that file wasn't used.

        if ((mnrows .gt. 0) .or. (mode .eq. 'AND')) then
            if (htype(1) .eq. 1) then
                call ftitab(ounit,mrowlen,mnrows,mfields,mtype,mbcol,
     &              mform,munit,mextname,ftstatus)
            else
                call ftibin(ounit,mnrows,mfields,mtype,mform,munit,
     &              mextname,varidat,ftstatus)
            endif
            do 20 i = 1, nhists
                length=fcstln(history(i))
                call ftphis(ounit,history(i)(1:length),ftstatus)
   20       continue
            if ((dates) .and. (mnrows .gt. 0)) then
                context = 'Reference date'
		is_string = index(ndate,'/')
		if(is_string .ne. 0) then
            call ftpkys (ounit, idates(1), ndate, context, ftstatus)
		else  
		    is_string = index(ndate,'-') 
		    if(is_string.ne.0) then 
            call ftpkys (ounit, idates(1), ndate, context, ftstatus)
		    else 
			read(ndate,931)num_ref_date 
931                     format(g40.17)
                        call ftpkyd (ounit, idates(1), num_ref_date, 
     *			15, context, ftstatus)
		     
		    endif
		endif
                context = 'Reference time'
                if (.not. julian) call ftpkys (ounit, itimes(1), ntime,
     &              context, ftstatus)
            endif
            call ftpkyf(ounit, 'TIMEZERO', 0.0, 1,
     &          'offset to be applied to times given in the data',
     &          ftstatus)
            call ftpkys (ounit, 'HDUCLASS', mhduclass,
     &           'format conforms to OGIP/GSFC standards', ftstatus)
            call ftpkys (ounit, 'HDUCLAS1', mhduclas1,
     &           'Extension contains Good Time Intervals', ftstatus)
            call ftpkys (ounit, 'HDUCLAS2', mhduclas2, ' ', ftstatus)
C           leave 2 extra keyword slots for checksums:
            call fthdef(ounit, 2, ftstatus)

            if (ftstatus .ne. 0) then
                context = ' Error in writing output header'
                call fcerr (context)
                goto 999
            endif

C           write the merged Good Time Intervals
            if (mnrows .gt. 0) then
                call ftpcld(ounit,1,1,1,mnrows,start,ftstatus)
                call ftpcld(ounit,2,1,1,mnrows,stop,ftstatus)
                if (ftstatus .ne. 0) then
                    context = ' Error in writing intervals to file'
                    call fcerr (context)
                    goto 999
                endif
            endif

C       If no rows were found, one or zero good files were input.  Only OR mode
C       gets to this section, and just copy the input file HDU to the output.
C       (NOTE: Currently doesn't come here if only 1 file, with rows, is
C       found.  Should it??)
C
        else
            call ftcrhd (ounit, ftstatus)
            call ftcopy (iunit(1), ounit, 2, ftstatus)
        endif

C       add checksums
        call ftpcks(ounit, ftstatus)

C  close the input file and the output file
        call ftclos(iunit(1),ftstatus)
        call ftclos(ounit,ftstatus)

C  close files and return on error
 999    continue
        if (ftstatus .ne. 0) then
            call fcerrm(ftstatus)
            ftstatus = 0
            if (inopen(1))  call ftclos(iunit(1),ftstatus)
            if (inopen(2))  call ftclos(iunit(2),ftstatus)
            if (outopen)  call ftclos(ounit,ftstatus)
        endif

        return
        end


C******************************************************************************
C SUBROUTINE:
C      fighst
C
C DESCRIPTION:
C      Add the given filename to the history records.
C
C AUTHOR/DATE:
C      Janice Tarrant 4/10/92
C
C MODIFICATION HISTORY:
C     Jeff Guerber 9-10/1997.  Add single filename on each call.
C
C NOTES:
C
C USAGE:
C      call fighst(filename(1),nhists,maxgti,history)
C
C ARGUMENTS:
C      filename - file name
C      nhists   - number of history records
C      maxgti   - size of history array
C      history  - history records
C
C PRIMARY LOCAL VARIABLES:
C      fcount   - call count
C
C CALLED ROUTINES:
C      function fcstln - return length of character string (integer)[misc.for]
C
C******************************************************************************
        subroutine fighst(filename,nhists,maxgti,history)

        implicit none
        integer maxgti
        character*(*) filename, history(maxgti)
        integer fcount, nhists,length,fcstln,length2
        data fcount/0/
        save fcount

        if (fcount .eq. 0) then
            nhists = 1
            length2 = fcstln(filename)
            history(1) = 'TASK:MGTIME on files '//filename(1:length2)
        else
            length = fcstln(history(nhists))
            history(nhists) = history(nhists)(1:length)//', '
            length = fcstln(history(nhists))
            length2 = fcstln(filename)
            if ((length + length2) .lt. 70) then
                history(nhists) = history(nhists)(1:length)
     &              //filename(1:length2)
            else
                nhists = nhists + 1
                history(nhists) = filename
            endif
        endif
        fcount = fcount + 1

        return
        end

C******************************************************************************
C SUBROUTINE:
C      fmgtkw
C
C DESCRIPTION:
C      get the date and time keywords and determine the reference
C       time system (JD or date)
C
C AUTHOR/DATE:
C      Emily A. Greene
C       Hughes STX
C       5/11/93
C
C MODIFICATION HISTORY:
C    Jeff Guerber 9-10/1997.  Moved fcgcls calls out, so it's now useful for
C       every input file, not just first. (idate, itime are now inputs)
C    Jeff Guerber, 1998-07-01.  Call fcisjul, which checks both old (dd/mm/yy)
C       and new (yyyy-mm-dd) style dates.
C
C NOTES:
C
C USAGE:
C               call fmgtkw (iunit, idate, itime,
C                   filename, date, time, julian, status)
C
C ARGUMENTS:
C       iunit - input unit number
C       idate  - ref-date keyword to read
C       itime  - ref-time keyword to read
C       filename - name of the current input file
C       date    - the date value returned
C       time    - the time values returned
C       julian - true if the values represent julian days
C                  false for dd/mm/yy or yyyy-mm-dd
C       status   - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      keyword - name of a keyword
C      comment - keyword comment field
C
C CALLED ROUTINES:
C      subroutine xftgkys - get string keyword value (like ftgkys but
C           checks for KEYI/KEYF pair first; in misc.for)
C      function fcisjul - does date look like it's Julian or calendar?
C
C******************************************************************************
      subroutine fmgtkw (iunit, idate, itime,
     &    filename, date, time, julian, status)

      implicit none
      character*(*) idate, itime, date, time, filename
      integer iunit, status
      logical julian, fcisjul

      character(70) context

      call xftgkys(iunit, idate, date, context, status)
      if (status .ne. 0) then
          context = 'error getting reference date, file '//filename
          call fcerr(context)
          goto 999
      endif

C     determine if we have julian day or dd/mm/yy format
      julian = fcisjul( date )

      if (.not. julian) then
C         need to get the time for yyyy-mm-dd or dd/mm/yy format values

          call xftgkys(iunit, itime, time, context, status)
          if (status .ne. 0) then
              context = 'error getting reference time, file '//filename
              call fcerr(context)
              goto 999
          endif
      endif

  999 return
      end
