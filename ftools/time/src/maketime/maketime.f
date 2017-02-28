C***************************************************************************
C SELECTOR TASK:
C      maketime
C
C FILE:
C      maketime.f
C
C DESCRIPTION:
C      Creates a GTI FITS file based on a selection expression applied 
C      to a House Keeping FITS file
C
C AUTHOR/DATE:
C      Kent Blackburn  6/08/92
C
C MODIFICATION HISTORY:
C       3/11/93 EAG - Changed fcecho - fcerr, and Task name output
C       11/28/95 Srilal - TDISP keywords will not be copied - xmktie deleted
C v2.2a 06Feb98 (MJT) prevent writing rows where nstart = nstop
C v2.3  02Mar98 (PDW) Support expression files of unlimited length
C v2.4  18Mar98 (toliver) Replaced call to obsolete fitsio routine 'ftgbnh'
C                         with call to new cfitsio function 'ftghbn'
C v2.5  26May98 (PDW) Ported code to use new CFITSIO parser
C v2.6  06Jul98 (PDW) Restore acceptable warning for duplicate time stamps
C v2.6a 28Jul98 (PDW) Change names of parser routines
C v2.6b 31Jul98 (PDW) Set initial number of rows in GTI to zero
C v2.6c 05Aug98 (PDW) For expr history... allow lines longer than 80 chars
C v2.6d 13Nov98 (PDW) Restore acceptable warning for out-of-order time stamps
C                     and handle unordered times more intelligently
C v2.7  25Oct99 (NG)  If the compact = no, the name/value parameter will not
C                     be prompted.
C v2.8  04Jan00 (PDW) Initialize oldtime to initial time stampe
C
C
C NOTES:
C      maketime supported in IRAF and HOST environments
C
C USAGE:
C      HOST: call fggti
C      IRAF: task fggti
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infile  - input HK FITS file and extension number
C      outfile - output GTI FITS file
C      expr    - expression which evaluates to a boolean
C      name    - Column in infile which contains names of parameters
C      value   - Column in infile which contains values of parameters
C      time    - Column in infile which contains times for parameter values
C      hkstart - Column in outfile which will contain start times
C      hkstop  - Column in outfile which will contain stop times
C      compact - boolean determining the HK FITS file format
C      copykw  - copy all other keywords flag
C      histkw  - print history keyword flag
C      prefr   - pre time interval factor between [0,1]
C      postfr  - post time interval factor between [0,1]
C
C CALLED ROUTINES:
C      subroutine gmktie - gets parameters from parameter file
C      subroutine figgti - generates the GTI FITS file 
C
C***************************************************************************

        subroutine makete
        implicit none
        character(256)  infile, outfile
        character(80)   name, value, time, hkstart, hkstop 
        character(2048) expr
        logical        compact,histkw,copykw
        double precision prefr,postfr
        character(40) taskname
        common /task/ taskname

        taskname = 'maketime2.9'
        infile  = ' '
        outfile = ' '
        expr    = ' '
        name    = ' '
        value   = ' '
        time    = ' '
        hkstart = ' '
        hkstop  = ' '
        prefr   = -1.0
        postfr  = -1.0

C  get parameters from parameter file
        call gmktie(infile,outfile,expr,name,value,time,
     &              hkstart,hkstop,compact,
     &              copykw,histkw,prefr,postfr)

C  extract data to new FITS file
        call figgti(infile,outfile,expr,name,value,time,
     &              hkstart,hkstop,compact,
     &              copykw,histkw,prefr,postfr)

        return
        end


C*************************************************************************** 
C SUBROUTINE:
C      gmktie
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C AUTHOR:  
C      Kent Blackburn  6/08/92
C
C MODIFICATION HISTORY:
C
C NOTES:
C       gmktie uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gmktie(infile,outfile,expr,name,value,time,
C    &             hkstart,hkstop,compact,
C    &             copykw,histkw,prefr,postfr)
C
C ARGUMENTS:
C      infile  - input HK FITS file and extension number
C      outfile - output GTI FITS file
C      expr    - expression which evaluates to a boolean
C      name    - Column in infile which contains names of parameters
C      value   - Column in infile which contains values of parameters
C      time    - Column in infile which contains times for parameter values
C      hkstart - Column in outfile which will contain start times
C      hkstop  - Column in outfile which will contain stop times
C      compact - boolean determining the HK FITS file format
C      copykw  - copy all other keywords flag
C      histkw  - print history keyword flag
C      prefr   - pre time interval factor between [0,1]
C      postfr  - post time interval factor between [0,1]
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine uclgst - get string parameter
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C
C*************************************************************************** 

        subroutine gmktie(infile,outfile,expr,name,value,time,
     &                    hkstart,hkstop,compact,
     &                    copykw,histkw,prefr,postfr)
        implicit none

        character*(*) infile,outfile,expr,name,value,time,
     &                hkstart,hkstop
        logical       compact,histkw,copykw
        double precision prefr,postfr
        character(80) context
        integer status

C  initialize variables
        status = 0

C  get the name of the input FITS file
        call uclgst('infile',infile,status)
        if (status .ne. 0) then
            context = 'could not get infile parameter'
            call fcerr(context)
            goto 999
        endif

C  get the name of the output FITS file
        call uclgst('outfile',outfile,status)
        if (status .ne. 0) then
            context = 'could not get outfile parameter'
            call fcerr(context)
            goto 999
        endif

C  get the boolean expression 
        call uclgst('expr',expr,status)
        if (status .ne. 0) then
            context = 'could not get expr parameter'
            call fcerr(context)
            goto 999
        endif

C  get the compact flag
        call uclgsb('compact',compact,status)
        if (status .ne. 0) then
            context = 'could not get compact flag'
            call fcerr(context)
            goto 999
        endif
        
C       if not compact, skip the name/value column 
        if (.not.compact) then 
             go to 111
        endif

C  get the name column
        call uclgst('name',name,status)
        if (status .ne. 0) then
            context = 'could not get name column'
            call fcerr(context)
            goto 999
        endif

C  get the value column
        call uclgst('value',value,status)
        if (status .ne. 0) then
            context = 'could not get value column'
            call fcerr(context)
            goto 999
        endif
111     continue

C  get the time column
        call uclgst('time',time,status)
        if (status .ne. 0) then
            context = 'could not get time column'
            call fcerr(context)
            goto 999
        endif

C  get the hkstart column
        call uclgst('start',hkstart,status)
        if (status .ne. 0) then
            context = 'could not get start column'
            call fcerr(context)
            goto 999
        endif

C  get the hkstop column
        call uclgst('stop',hkstop,status)
        if (status .ne. 0) then
            context = 'could not get stop column'
            call fcerr(context)
            goto 999
        endif

C  get the compact flag
C        call uclgsb('compact',compact,status)
C        if (status .ne. 0) then
C            context = 'could not get compact flag'
C            call fcerr(context)
C            goto 999
C        endif

C  get the copykeyword flag
        call uclgsb('copykw',copykw,status)
        if (status .ne. 0) then
            context = 'could not get copy all other keyword flag'
            call fcerr(context)
            goto 999
        endif

C  get the history flag
        call uclgsb('histkw',histkw,status)
        if (status .ne. 0) then
            context = 'could not get history keyword flag'
            call fcerr(context)
            goto 999
        endif

C  get pre-time interval factor
        call uclgsd('prefr',prefr,status)
C       If the user has not entered a value on the command line, the default
C       prefr=-1, which is not allowed (status=200), so reset status to 0.
C       In figgti, the value for prefr will either be taken from the PREFR
C       keyword (if available) or set to a default of 0.5.
        if (status .eq. 200) then
            status = 0
            call RESETHEASTATUS
        endif
        if (status .ne. 0) then
            context = 'could not get pre time interval factor'
            call fcerr(context)
            goto 999
        endif

C  get post-time interval factor
C       If the user has not entered a value on the command line, the default
C       postfr=-1, which is not allowed (status=200), so reset status to 0.
C       In figgti, the value for postfr will either be taken from the POSTFR
C       keyword (if available) or set to a default of 0.5.
        call uclgsd('postfr',postfr,status)
        if (status .eq. 200) then
            status = 0
            call RESETHEASTATUS
        endif
        if (status .ne. 0) then
            context = 'could not post time interval factor'
            call fcerr(context)
            goto 999
        endif

  999   continue
        if (status .ne. 0) then
            call fcerrm(status)
            stop
        endif

        return
        end


C***************************************************************************
C SUBROUTINE:
C      figgti
C
C DESCRIPTION:
C      select times from an HK FITS file extension based on boolean expression 
C
C AUTHOR/DATE:
C      J. Kent Blackburn 6/08/92
C
C MODIFICATION HISTORY:
C      PDW 03/02/98 -- Alter handling of expr to allow file expressions of
C                      unlimited length... pass filename to parser
C      PDW 05/26/98 -- Ported to new CFITSIO parser.  Use dynamic memory
C                      to hold time column and expression results
C      PDW 07/28/98 -- Change name of parser routines
C      PDW 07/31/98 -- Initialize NAXIS2 of GTI extension to zero
C      PDW 08/05/98 -- For expr history... allow lines longer than 80 chars
C
C NOTES:
C       
C
C USAGE:
C      call figgti(infile,outfile,expr,name,value,time,
C    &             hkstart,hkstop,compact,
C    &             copykw,histkw,prefr,postfr)
C
C ARGUMENTS:
C      infile  - input HK FITS file and extension number
C      outfile - output GTI FITS file
C      expr    - expression which evaluates to a boolean
C      name    - Column in infile which contains names of parameters
C      value   - Column in infile which contains values of parameters
C      time    - Column in infile which contains times for parameter values
C      hkstart - Column in outfile which will contain start times
C      hkstop  - Column in outfile which will contain stop times
C      compact - boolean determining the HK FITS file format
C      copykw  - copy all other keywords flag
C      histkw  - print history keyword flag
C      prefr   - pre time interval factor between [0,1]
C      postfr  - post time interval factor between [0,1]
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C      status  - error number
C      fname   - input fits file name
C      errstr  - concatenated error message
C      comment - comment string found in FITS file
C      clname  - column name found in FITS index
C      history - history string 
C      maxcl   - maximum number of columns supported by software
C      simple  - FITS primary header flag
C      extend  - FITS extension header flag
C      exact   - FITS keyword match flag
C      inopen  - input file open flag
C      outopen - output file open flag
C      eopen   - expression file open flag
C      bool    - expression evaluates to boolean flag
C      extnum  - extension number in input FITS file
C      ftstat  - fitsio library call return status
C      iunit   - input file unit number
C      ounit   - output file unit number
C      eunit   - optional expression file unit number
C      filexpr - optional filename which contains an expression
C      record  - string containing contents of one line from eunit file
C      temp    - temporary storage string
C      rlen    - length of record string
C      tlen    - length of temporary string
C      block   - fits file block size
C      htype   - fits file header type
C      bitpix  - number of bits per pixel in primary array
C      naxis   - number of dimensions of in array
C      naxes   - number of points along each dimension 
C      pcount  - value of pcount keyword
C      gcount  - value of gcount keyword
C      rowlen  - length of FITS table in bytes
C      nrows   - number of records in FITS table
C      tfields - total number of columns in FITS table
C      varidat - size in bytes of variable data area 
C      clnum   - column number
C      berror  - boolean error number
C      ttype   - array of column names
C      tform   - array of column formats
C      tunit   - array of column units
C      tbcol   - column number of first char in each field
C      extname - extension name
C
C CALLED ROUTINES:
C      subroutine fcpars - parse filename and extension from infile
C      subroutine ftopen - open a FITS file
C      subroutine ftinit - initialize a FITS file
C      subroutine ftmrhd - relative move to header in FITS file
C      subroutine ftgtbh - get ascii table header keywords
C      subroutine ftghbn - get binary table header keywords
C      subroutine ftgkys - get character string keyword
C      subroutine ftgkyj - get integer keyword
C      subroutine ftgprh - get primary header in FITS file
C      subroutine ftphpr - put primary header in FITS file
C      subroutine ftcrhd - create header in FITS file
C      subroutine ftphtb - put ascii table header in FITS file
C      subroutine ftphbn - put binary table header in FITS file
C      subroutine ftphis - put history keyword in FITS file
C      subroutine ftclos - close a FITS file
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C      subroutine fcstln - length of string once padded spaces removed
C
C***************************************************************************

        subroutine figgti(infile,outfile,expr,name,value,time,
     &                    hkstart,hkstop,compact,
     &                    copykw,histkw,prefr,postfr)
        implicit none

        character*(*) infile,outfile,expr,name,value,time,
     &                hkstart,hkstop
        logical       compact,histkw,copykw
        double precision prefr,postfr
        integer maxcl
        parameter ( maxcl = 999 )
        character(256) fname
        character(80) context,comment,errstr
        character(256) filexpr
        character(2048) temp,history
        logical simple,extend,exact,inopen,outopen
        logical eopen,bool,mem_alloc
        integer extnum,ftstat,iunit,ounit,block,htype,bitpix,rlen
        integer naxis,naxes(99),pcount,gcount,rowlen,nrows,tlen
        integer tfields,varidat,eunit,tmpstat
        character(70) ttype(maxcl),tform(maxcl),tunit(maxcl)
        character(70) extname
        integer fcstln,tbcol(maxcl),flen,elen
        integer colprm,colval,coltime
        integer p_times,p_flags,ntimes,n_good

C************************************************************************
      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD
C************************************************************************
        
C   initialize variables
        ftstat = 0
        iunit = 15
        ounit = 16
        eunit = 17
        block = 0
        colprm = 0
        colval = 0
        coltime = 0
        exact = .false.
        inopen = .false.
        eopen = .false.
        outopen = .false.
        context = ' '
        comment = ' '
        errstr = ' '
        mem_alloc =.false.

C   get the filename and extension
        call fcpars(infile,fname,extnum,ftstat)
        flen = fcstln(fname)

C EAG 8/23/93 default to 1st extension
        if (extnum .eq. -99) extnum = 1

C   if the extension is 0 then give an error and exit
        if (extnum .lt. 1) then
          context = 'Primary extension not supported'
          call fcerr(context)
          goto 999
        endif

C   open the input FITS file
        call ftopen(iunit,fname,0,block,ftstat)
        if ( ftstat .ne. 0 ) then
          context = 'Unable to open infile'
          call fcerr(context)
          goto 999
        endif 
        inopen = .true.

C   read in the primary array header required keywords
        call ftgprh(iunit,simple,bitpix,naxis,naxes,
     &              pcount,gcount,extend,ftstat)

C   move to the extension in the input file
        call ftmrhd(iunit,extnum,htype,ftstat)
        if ( ftstat .ne. 0 ) then
          errstr = 'Error moving to extension number '
          write(context,1000) errstr, extnum
 1000     format(A34,I3)
          call fcerr(context)
          goto 999
        endif

C   get extension header's keywords depending on extension type
        if ( htype .eq. 1 ) then
          call ftgtbh(iunit,rowlen,nrows,tfields,ttype,tbcol,
     &              tform,tunit,extname,ftstat)
        else if ( htype .eq. 2 ) then
          call ftghbn(iunit,maxcl,nrows,tfields,ttype,tform,tunit,
     &              extname,varidat,ftstat)
        else 
          context = 'Extension type not supported'
          call fcerr(context)
          goto 999
        endif

C   if compact then verify the name, value, and time columns to be present
        if (compact) then
          call ftgcno(iunit,exact,name,colprm,ftstat)
          if ((colprm .eq. 0).or.(ftstat .ne. 0)) then
            context = 'Name column not found'
            call fcerr(context)
            goto 999
          endif
          call ftgcno(iunit,exact,value,colval,ftstat)
          if ((colval .eq. 0).or.(ftstat .ne. 0)) then
            context = 'Value column not found'
            call fcerr(context)
            goto 999
          endif
        endif
        call ftgcno(iunit,exact,time,coltime,ftstat)
        if ((coltime .eq. 0).or.(ftstat .ne. 0)) then
          context = 'Time column not found'
          call fcerr(context)
          goto 999
        endif

C   If prefr and postfr values were not given on the command line,
C   look for PREFR & POSTFR keywords.  Otherwise, use the default
C   value of 0.5.

        if ( prefr .lt. 0.0 ) then
          call ftgkyd(iunit,'PREFR',prefr,context,ftstat)
          if ( ftstat .ne. 0 ) then
            ftstat = 0
            prefr = 0.5
            context = 'PREFR keyword not found, using prefr = 0.5'
            call fcecho(context)
          else
            comment = 'PREFR keyword found in header, using prefr = '
            write(context,1002) comment, prefr
            call fcecho(context)
          endif
        endif

 1002   format(A47,F3.1)

        if ( postfr .lt. 0.0 ) then
          call ftgkyd(iunit,'POSTFR',postfr,context,ftstat)
          if ( ftstat .ne. 0 ) then
            ftstat = 0
            postfr = 0.5
            context = 'POSTFR keyword not found, using postfr = 0.5'
            call fcecho(context)
          else
            comment = 'POSTFR keyword found in header, using postfr = '
            write(context,1002) comment, postfr
            call fcecho(context)
          endif
        endif

C   Count the number of time elements and allocate arrays
        call fitime(iunit, compact, coltime, ntimes, ftstat)
        if (ftstat .ne. 0) then
           context = 'unable to read input time column'
           call fcerr(context)
           goto 999
        endif

        mem_alloc = .false.
        p_times = 0
        call udmget(ntimes, 7, p_times, ftstat)
        if (ftstat .ne. 0) then
           context = 'unable to allocate time array'
           call fcerr(context)
           goto 999
        endif

        p_flags = 0
        call udmget(ntimes, 1, p_flags, ftstat)
        if (ftstat .ne. 0) then
           tmpstat = 0
           call udmfre(p_times, 7, tmpstat)
           context = 'unable to allocate flag array'
           call fcerr(context)
           goto 999
        endif
        mem_alloc = .true.

C   If not compact file, read time column into time array
        if( .not. compact ) then
           call ftgcvd(iunit,coltime,1,1,ntimes,0.D0,
     &          MEMD(p_times),bool,ftstat)
           if (ftstat .ne. 0) then
              context = 'unable to read time column'
              call fcerr(context)
              goto 999
           endif
        endif

C   open the output FITS file
        call ffinit(ounit,outfile,ftstat)
        if (ftstat .ne. 0) then
            context = 'unable to open outfile'
            call fcerr(context)
            goto 999
        endif
        outopen = .true.

C   construct simple primary header for OUTPUT file
        simple = .true.
        naxis = 0
        pcount = 0
        gcount = 1
        call ftphpr(ounit,simple,bitpix,naxis,naxes,pcount,
     &              gcount,extend,ftstat)

C   if copy all other keywords flag is true then do so
        if (copykw) then
          call xcopynoscale(iunit,ounit,ftstat)
        endif

C   create a new extension in the output FITS file
        call ftcrhd(ounit,ftstat) 

C   Initialize Header Keywords for the GTI FITS file
        tfields = 2
        ttype(1) = hkstart
        ttype(2) = hkstop
        if ( htype .eq. 1 ) then
          tform(1) = 'E23.15'
          tform(2) = 'E23.15'
          tbcol(1) = 1
          tbcol(2) = 24
          rowlen = 46
        else
          tform(1) = '1D'
          tform(2) = '1D'
        endif
        tunit(1) = 'sec'
        tunit(2) = 'sec'
        extname = 'STDGTI'

C   write extension header's keywords depending on the table type
        if ( htype .eq. 1 ) then
          call ftphtb(ounit,rowlen,0,tfields,ttype,tbcol,
     &                tform,tunit,extname,ftstat)
          if (copykw) call xcopynoscale(iunit,ounit,ftstat)
        else if ( htype .eq. 2 ) then
          call ftphbn(ounit,0,tfields,ttype,tform,tunit,
     &                extname,varidat,ftstat)
          if (copykw) call xcopynoscale(iunit,ounit,ftstat)
        endif

C Write the new HDUCLASS keywords
        comment = 'format conforms to OGIP/GSFC conventions'
        call ftukys(ounit,'HDUCLASS','OGIP',comment,ftstat)
        comment = 'Extension contains Good Time Intervals'
        call ftukys(ounit,'HDUCLAS1','GTI ',comment,ftstat)
        call ftukys(ounit,'HDUCLAS2','STANDARD',comment,ftstat)

C Write the new PREFR & POSTFR keywords
        comment = 'For maketime: time interval before a row'
        call ftukyd(ounit,'PREFR',prefr,1,comment,ftstat)
        comment = 'For maketime: time interval after a row'
        call ftukyd(ounit,'POSTFR',postfr,1,comment,ftstat)

C Write the history keyword
        if (histkw) then
           history = 'TASK: MAKETIME on FILENAME: '//fname(1:flen)
           call ftphis(ounit,history,ftstat)
           call timestamp(ounit)
           elen = fcstln(expr)
           history = 'Expression: '//expr(1:elen)
           call ftphis(ounit,history,ftstat)
           if ( expr(1:1).eq.'@' ) then
              filexpr = expr(2:)
              history = ' '
              rlen = 0
              open(unit=eunit,file=filexpr,err=30,status='old')
              eopen = .true.
 10           continue
                read(eunit,1001,end=20) temp
                tlen = fcstln(temp)
                if ( tlen+rlen.ge.2048 ) then
                   call ftphis(ounit,history,ftstat)
                   history = temp
                   rlen = tlen
                else
                   history(rlen+1:) = temp
                   rlen = rlen + tlen + 1
                endif
              goto 10
 1001         format(a2047)
 20           close(eunit)
              eopen = .false.
              call ftphis(ounit,history,ftstat)
              goto 40
 30           context = 'Unable to open expression file: '//filexpr
              call fcerr(context)
              goto 999
 40           continue
           endif
        endif

C   Evaluate expression for the given file type
        if( compact ) then
           call ftfrwc(iunit,expr,time,name,value,ntimes,
     &          MEMD(p_times),MEMB(p_flags),ftstat)
        else
           call ftfrow(iunit,expr,1,ntimes,n_good,MEMB(p_flags),
     &          ftstat)
        endif
        if( ftstat.ne.0 ) then
           context = 'Failed to parse expression'
           call fcerr(context)
           goto 999
        endif

C   fill start, stop columns based on result of evaluation
        call fisscl(ounit, ntimes, MEMD(p_times), MEMB(p_flags),
     &       prefr, postfr, ftstat)

  999   continue
        if ( ftstat .ne. 0 ) then
          call fcerrm(ftstat)
          ftstat = 0
        endif
        if ( mem_alloc ) then
           call udmfre(p_times, 7, ftstat)
           call udmfre(p_flags, 1, ftstat)
        endif
        if ( inopen ) then
          ftstat = 0
          call ftclos(iunit,ftstat)
        endif
        if ( outopen ) then
          ftstat = 0
          call ftclos(ounit,ftstat)
        endif
        if ( eopen ) then
          close(eunit)
        endif
        return
        end


C***************************************************************************
C SUBROUTINE:
C      fitime
C
C DESCRIPTION:
C      count up the number of distinct time stamps in column
C
C AUTHOR/DATE:
C      J. Kent Blackburn 6/10/92
C
C MODIFICATION HISTORY:
C       22 Jan 1993 EAG   E99 too small for vax, changed to D38
C       26 May 1998 PDW   Add error checking for bad time column
C       06 Jul 1998 PDW   Add warning for duplicate time stamp (but accept it)
C       12 Nov 1998 PDW   Add warning for outoforder time stamp (but accept it)
C       04 Jan 2000 PDW   Initialize oldtime to initial time stampe
C
C NOTES:
C
C
C USAGE:
C      call fitime(iunit,compact,colnum,count,ftstat) 
C
C ARGUMENTS:
C      iunit  - input FITS file unit number
C      compact- Is file in compacted format?
C      colnum - column number containing time stamps
C      count  - number of distinct time stamps
C      ftstat - error reporting integer
C
C PRIMARY LOCAL VARIABLES:
C      ptime   - double array of time stamps found
C      context - error message
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutind ftgcvd - get double array from FITS table
C
C***************************************************************************
        subroutine fitime(iunit,compact,colnum,count,ftstat)
        implicit none

        integer count,ftstat
        logical exact,anyf,compact
        integer colnum,felem,nelem
        integer maxcl, maxsize
        parameter ( maxcl = 999 )
        parameter (maxsize = 1024)
        integer i,iunit, nrows, irow, remain
        double precision ptime(maxsize),oldtime,nullval
        character(70) context
        
        i = 0
        exact = .false.
        ftstat = 0
        count = 0
        felem = 1
        nullval = 0.0D0
ceag    oldtime = -9.9999999999e-99   !too small for vax
cpdw    oldtime = -1.D-38             !screws up for negative timestamps
	oldtime =0.0d0

        call ftgkyj(iunit,'NAXIS2',nrows,context,ftstat)
        remain = nrows
        irow = 1

5       if (remain .gt. maxsize) then
           nelem = maxsize
        else
           nelem = remain
        endif

        call ftgcvd(iunit,colnum,irow,felem,nelem,nullval,
     &       ptime,anyf,ftstat) 
C     PDW 1/4/00: Initialize oldtime to first entry - epsilon
        if ( irow.eq.1 ) then
           oldtime = ptime(1)
        endif
        if ( anyf ) then
           context = 'NULL found in time column'
           call fcerr(context)
           ftstat = 435
           goto 999
        else if ( ftstat.ne.0 ) then
           context = 'trouble reading time column'
           call fcerr(context)
           goto 999
        else
           do 10 i = 1, nelem
C PDW 1/4/00: Add test for initial time stamp
              if ( ptime(i) .gt. oldtime .or. (irow+i).eq.2 ) then
                 count = count + 1
                 oldtime = ptime(i)
C PDW 7/6/98: Add warning for (but accept) duplicate time stamps
              else if ( .not.compact .and. ptime(i).eq.oldtime) then
                 count = count + 1
                 context = 'Duplicate time stamps encountered'
                 call fcecho(context)
              else if ( ptime(i).lt.oldtime) then
C PDW 11/12/98: Add warning for (but accept) out-of-order time stamps
                 count = count + 1
                 oldtime = ptime(i)
                 context = 'Time column not in order'
                 call fcecho(context)
              endif
 10        continue
        endif
        
        remain = remain - nelem
        irow = irow + nelem
        if (remain .gt. 0) goto 5
      
 999    continue
        if( ftstat.ne.0 ) then
           call fcerrm(ftstat)
        endif

        return
        end

C***************************************************************************
C SUBROUTINE:
C      fisscl
C
C DESCRIPTION:
C      supply values to START STOP columns based on expression's
C      boolean value
C
C AUTHOR/DATE:
C      J. Kent Blackburn 6/15/92
C
C MODIFICATION HISTORY:
C
C   6Feb98 (MJT) added check to prevent writing
C                any row for which nstart = nstop
C  26May98 (PDW) Updated for new CFITSIO parser
C
C NOTES:
C
C
C USAGE:
C      call fisscl(ounit,ntimes,times,flags,prefr,postfr,ftstat)
C
C ARGUMENTS:
C      ounit   - output file unit number
C      ntimes  - Number of time stamps in array
C      times   - The time stamp array
C      flags   - True/False result of evaluation expression on file
C      prefr   - pre time interval factor between [0,1]
C      postfr  - post time interval factor between [0,1]
C      ftstat  - FITSIO error number
C
C PRIMARY LOCAL VARIABLES:
C      frow    - first row in range
C      nrows   - current number of rows stored in (start,stop) arrays
C      ttlrows - total number of (start,stop) rows found
C      maxrows - maximum number of rows in a range
C      nstart  - new start value
C      nstop   - new stop value
C      gtistart- GTI start values array
C      gtistop - GTI stop values array
C      pair    - "found a (start,stop) pair in the HK file" flag
C
C CALLED ROUTINES:
C      subroutine fndssp - get next (start,stop) pair
C
C***************************************************************************

        subroutine fisscl(ounit, ntimes, times, flags,
     &     prefr, postfr, ftstat)
        implicit none

        integer ounit,ntimes,ftstat
        integer frow,nrows,ttlrows,row_no
        logical pair,flags(ntimes)
        integer maxrows
        parameter ( maxrows = 512 )
        double precision nstart,nstop,prefr,postfr,times(ntimes)
        double precision gtistart(maxrows),gtistop(maxrows)

C   loop thru in chunks of maxrows to keep array size small
        frow = 1
        ttlrows = 0
        nrows = 0
        row_no = 1
        
   20   continue
   
C   determine the next (start,stop) time stamp pair 
        call fndssp(ntimes,times,flags,prefr,postfr,row_no,
     &       nstart,nstop,pair)

C   if we have a pair then register it
        if (pair) then
C   6Feb98 (MJT) don't want pairs where start = stop
           if (nstart .ne. nstop) then
              ttlrows = ttlrows + 1
              nrows = nrows + 1
              gtistart(nrows) = nstart
              gtistop(nrows) = nstop
           endif
        endif

C   write the current set of nrows to the output GTI FITS file
        if (( nrows .eq. maxrows) .or. row_no.ge.ntimes) then
           call ftpcld(ounit,1,frow,1,nrows,gtistart,ftstat)
           call ftpcld(ounit,2,frow,1,nrows,gtistop,ftstat)
           frow = frow + nrows
           nrows = 0
        endif

        if (row_no.lt.ntimes) goto 20
        
C   modify the number of records keyword in the output FITS file
        call ftmkyj(ounit,'NAXIS2',ttlrows,'&',ftstat)

        return
        end


C*************************************************************************** 
C SUBROUTINE:
C      fndssp
C
C DESCRIPTION: 
C      Find next start and stop gti times
C
C AUTHOR:  
C      Peter Wilson 05/26/98
C
C MODIFICATION HISTORY:
C      PDW 11/13/98: Test for out-of-order times when calculating GTIs
C
C USAGE:
C      subroutine fndssp(ntimes, times, flags, prefr, postfr,
C                        row_no, start, stop, pair)
C
C ARGUMENTS:
C      ntimes  - Number of time stamps in array
C      times   - The time stamp array
C      flags   - True/False result of evaluation expression on file
C      prefr   - pre time interval factor between [0,1]
C      postfr  - post time interval factor between [0,1]
C      row_no  - row number at which to start the search
C      start   - Start of good gti time
C      stop    - End of good gti time
C      pair    - Returned flag indicating a match was found
C
C*************************************************************************** 
      subroutine fndssp(ntimes, times, flags, prefr, postfr,
     &     row_no, start, stop, pair)
      implicit none

      integer ntimes, row_no
      logical pair, flags(ntimes)
      double precision times(ntimes), prefr, postfr
      double precision start, stop, prevtime
      integer i

      pair = .false.

C     skip initial false rows
      do 10 i=row_no,ntimes
         if( flags(i) ) goto 15
 10   continue
      goto 900

 15   row_no = i
      if( i.eq.1 ) then
         start = times(i)
      else
C          Add test for out-of-order times (PDW 11/13/98)
         if( times(i-1).gt.times(i) ) then
            start = times(i)
         else
            start = times(i) - prefr*( times(i)-times(i-1) )
         endif
      endif
      pair = .true.

C     Find all the true rows
      prevtime = times(i)
      do 20 i=row_no,ntimes
C          Add test for out-of-order times (PDW 11/13/98)
         if( .not. flags(i) .or. prevtime.gt.times(i) ) goto 25
         prevtime = times(i)
 20   continue

 25   if( i.gt.ntimes ) then
         stop = times(ntimes)
      else
C          Test whether we terminated this GTI due to an
C          out of order time.  Use last good time if so. (PDW 11/12/98)
         if( times(i).le.times(i-1) ) then
            stop = times(i-1)
         else
            stop = times(i-1) + postfr*( times(i)-times(i-1) )
         endif
      endif

C     Return the last row number examined
 900  row_no = i
      return
      end
