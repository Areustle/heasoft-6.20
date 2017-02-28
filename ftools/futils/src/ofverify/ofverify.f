C******************************************************************************
C TASK:
C      fverify (the standalone fortran version is called verifits)
C
C DESCRIPTION:
C       read a FITS file and verify that it conforms to the FITS standard
C
C AUTHOR/DATE:
C       William D Pence 30 June 1994
C
C MODIFICATION HISTORY:
C       9/12/94 EAG 1.3 Use FAOPEN
C       9/22/94 EAG 1.4 Don't die if extension is specified
C      10/18/94 WDP 1.5 many changes
C      12/30/94 WDP 1.6 check table column names for uniqueness and legal chars
C      03/01/95 WDP 1.7 check TNULLn usage
C      05/12/95 WDP 1.8 removed ft0001, ft002 common blocks for portability
C      11/30/95 JRG 1.9 in TSTTBL, changed TTYPE to char*40, but only output
C          (1:16); does not affect TSTTYP, which still checks uniqueness of
C          the first 16 chars.
C      02/23/98 PDW 2.0 in TSTNCL, allow for rows in variable-length columns
C                       to have zero-length
C      03/17/98 PDW 2.0a Synch-up printed version number with task version
C      05/29/98 PDW 2.0b Update parameter list for ftghad routine
C
C******************************************************************************
C  XXXX comment out the following statement in the standalone VERIFITS version
        subroutine ofvery

C	Read a FITS file and verify that it conforms to the FITS standard.
C       Written by William Pence, HEASARC, NASA/GSFC,  June 1994

        integer iunit,status,exttyp,block,extnum,endbyt
        character fname*160,infile*160,numext*3,cwarn*4,cerror*4
        character bytend*10
        logical prhdr,verdat,movhdu

C       tofile indicates whether output goes to a file or to user's screen
        logical tofile
        common /out/tofile

C       taskname is required in the FTOOLS environment
        character(40) taskname
        common /task/ taskname

C       nerror and nwarn store the number of issued error and warning messages
        integer nerror,nwarn
        common/messag/nerror,nwarn

        taskname = 'ofverify 2.0b'

        status=0
        iunit=15
        movhdu=.true.

C       clear the fitsio error message stack
        call ftcmsg
        nwarn=0
        nerror=0

C       get input parameters
        call gtparm(fname,prhdr,verdat,status)
        if (status .gt. 0)go to 999

        call writit(' ')
        call writit('****  FITS File Verifier, Version 2.0  ****')
        call writit('File = '//fname(1:70))
        call writit(' ')
        call writit('---------------------------------------'//
     &              '---------------------------------------' )

C  XXXX In FTOOLS environment, check if an extension number was specified
        extnum=-99
        call fcpars(fname,infile,extnum,status)
        if (extnum .ne. -99) then
            call writit(' OFVERIFY checks the entire FITS file')
            call writit(' Ignoring specified extension number...')
            call writit(' ')
            fname=infile
        end if

C	open the existing FITS file
        call ftopen(iunit,fname,0,block,status)

C       check for error
        if (status .eq. 252)then
C           major error; this is probably not a FITS format file
            call writit('Error reading the primary array.'
     &     //'  Did not find the SIMPLE keyword.')
            go to 900
        else if (status .ge. 200 .and. prhdr)then
C           If the header has a bad format, try printing the header anyway
            call writit('Primary array'//
     &       ' has fatal header keyword error:')
            call writit(' ')
C           dump the FITSIO error message stack
            call dmpstk(status)
C           try printing out the header records, so user can see whats wrong
            movhdu=.false.
            call dmphdr(iunit,movhdu)
            nerror=nerror+1
            status=0
            go to 900
        else if (status .gt. 0)then
C           some other error opening the file
            go to 900
        end if
        extnum=0
        exttyp=0
        call writit('    Primary Array')

C       test the header by reading and printing every keyword
10      call tsthdr(iunit,prhdr,status)
        if (status .gt. 0)go to 900

C       test the data by reading every value; compute and print statistics
        call tstdat(iunit,exttyp,verdat,status)
        if (status .gt. 0)go to 900

C       attempt to move to next extension
	call ftmrhd(iunit,1,exttyp,status)

        extnum=extnum+1
        write(numext,1000)extnum
1000    format(i3)
        call writit(' ')
        call writit('---------------------------------------'//
     &              '---------------------------------------' )

	if (status .eq. 0)then
          if (exttyp .eq. 0)then
            call writit('    Extension #'//numext//': IMAGE extension:')
          else if (exttyp .eq. 1)then
            call writit('    Extension #'//numext//': TABLE extension:')
          else if (exttyp .eq. 2)then
            call writit('    Extension #'//numext//
     &                 ': BINTABLE extension:')
          end if
            go to 10

	else if (status .eq. 107)then
C           normal end of file error; reset status to zero
            status=0

C           check for any extraneous bytes at the end of the file that
C           FITSIO my have ignored.
            call tstend(iunit,endbyt)
            if (endbyt .ne. 0)then
                write(bytend,1002)endbyt
1002            format(i10)
                call writit('*** Warning: The FITS file contains '
     &          //'extraneous byte(s) at the end of the file.')
                call writit('    The last known HDU in the FITS file'
     &          //' ends at byte '//bytend//'.')
                nwarn=nwarn+1

            else
                call writit('    < End-of-File >')
            end if

        else if (status .eq. 252)then
          call writit('*** Error trying to read extension '//numext
     &     //':  Did not find the XTENSION keyword.')
          nerror=nerror+1
          if (extnum .eq. 1)then
            call writit('    Unexpected data follows the  '//
     &                'primary array.')
          else
            call writit('    Unexpected data follows the  '//
     &                'last extension.')
          end if
          call writit(' ')

        else if (status .ge. 200 .and. prhdr)then
C           bad header format
            call writit('*** Extension '//numext//
     &       ' has fatal header keyword error:')
            call writit(' ')
C           dump the FITSIO error message stack
            call dmpstk(status)
C           try printing out the header records, so user can see what's wrong
            call dmphdr(iunit,movhdu)
            nerror=nerror+1
            status=0
	end if

900     call ftclos(iunit,status)

C       if an error was detected print out the FITSIO error message stack
        if (status .ne. 0)then
                call dmpstk(status)
                nerror=nerror+1
        end if

        write(cwarn,1001)nwarn
        write(cerror,1001)nerror
1001    format(i4)
        call writit(' ')
        call writit('****  End of Verification: '//cwarn//' warning(s)'
     &  //' and '//cerror//' error(s) were reported. ****')

C       close the output file if there is one
        if (tofile)then
                 close(17)
        end if

C       clear the FITSIO error message stack of any extraneous messages
        call ftcmsg

999     continue
        end
C---------------------------------------------------------------------------
      subroutine gtparm(infile,prhead,verdat,status)

C     this version of gtparm is for use in the FTOOLS environment

      character*(*) infile
      character(160) outfil
      logical prhead,verdat
      character(80) context
      integer status
      logical tofile
      common /out/ tofile

C ---   Get the name of the input FITS file ---
      call uclgst('infile',infile,status)
      if ( status .ne. 0 ) then
         context = 'Could not get INFILE parameter'
         call wrterr(context)
         goto 999
      endif

C ---   Get the name of the output FITS file ---
      call uclgst('outfile',outfil,status)
      if ( status .ne. 0 ) then
         context = 'Could not get OUTFILE parameter'
         call wrterr(context)
         goto 999
      endif
      tofile=.false.
      if (outfil .ne. ' ' .and. outfil .ne. 'STDOUT')then
         call faopen (17, outfil, 2, 0, status)
         if (status .ne. 0) then
 110        context = 'error: output file already exists? ' // outfil
            call wrterr(context)
            status=1
            goto 999
         else
            tofile=.true.
         endif
      end if

C       get the print header flag
      call uclgsb('prhead',prhead,status)
      if (status .ne. 0) then
         context = 'could not get PRHEAD flag'
         call wrterr(context)
         goto 999
      endif

C       get the testdata flag
      call uclgsb('testdata',verdat,status)
      if (status .ne. 0) then
         context = 'could not get TESTDATA flag'
         call wrterr(context)
      endif

 999  continue
      end
C---------------------------------------------------------------------------
        subroutine writit(string)
C       print out message to user or to output file
        character*(*) string

        logical tofile
        common /out/tofile

        if (tofile)then
             write(17,1000)string
1000         format(a)
        else
C  XXXX uncomment the appropriate output statement, depending on the version
C           fcecho is used in the FTOOLS version of this program
            call fcecho(string)

C  XXXX     a write statement is used in the standalone version of this program
C           Must use PRINT on VAX/VMS, otherwise 1st character is lost
C           Must use WRITE on DECstation, otherwise extra space is inserted

C            write(*,1000)string
C            print *,string
        end if
        end
C---------------------------------------------------------------------------
        subroutine wrterr(string)
C       print out error message to user
        character*(*) string

C       write out error message.  Write the messages to the output file
C       (if specified) AND to the user.

        logical tofile
        common /out/tofile

        if (tofile)then
             write(17,1000)string
1000         format(a)
        end if

C  XXXX fcerr is used in the FTOOLS version of this program
        call fcerr(string)

C       a write statement is used in the standalone version of this program
C       Must use PRINT on VAX/VMS, otherwise 1st character is lost
C       Must use WRITE on DECstation, otherwise extra space is inserted

C        write(*,1000)string
C        print *,string
        end
C---------------------------------------------------------------------------
        subroutine dmpstk(status)

C       print out the error status number and error message stack

        integer status
        character errtxt*30,errnum*4,record*80

        if (status .gt. 0)then
            call ftgerr(status,errtxt)
            write(errnum,2001)status
2001        format(i4)
            call wrterr('*** Error: '//errtxt//' (status ='//errnum//
     &        ')  ***')
        end if

10      call ftgmsg(record)
        if (record .ne. ' ')then
                call writit(record)
                go to 10
        end if
        call writit(' ')
        end
C---------------------------------------------------------------------------
        subroutine dmphdr(iunit,movhdu)

C       try to dump the keywords from the header that has a fatal format error

        integer iunit
        logical movhdu

        call writit('HDU header has fatal format error.')
        call writit('Unable to open and read header keywords.')
        end
C---------------------------------------------------------------------------
        subroutine tstend(iunit,endbyt)

C       try to read a record following the last known extension in the file.
C       If this succeeds, then return flag indicating there are extra
C       bytes at the end of the file

        integer iunit,endbyt
C        integer status,curhdu,curdata,nxthdu

        endbyt=0

C   as of 7/27/00, the ftghad routine is no longer generally support
C   in the Fortran wrappers to CFITSIO, therefore, disable checking
C   for extra bytes beyond the end of the last valid HDU  (WDP)

C        status=0
C        call ftghad(iunit,curhdu,curdata,nxthdu,status)
C        if (status .gt. 0) then
C            call writit('FTGHAD could not read HDU address')
C            goto 900
C        end if

C        status=0
C       first move to a previous record in the file (we're currently at the end)
C        call ftmbyt(iunit,curhdu,.true.,status)

C        status=0
C       move to non-existent extension to test for extra bytes at end of file
C        call ftmbyt(iunit,nxthdu,.false.,status)

C       return the end of the last extension as an error flag
C        if (status .eq. 0)endbyt=nxthdu

 900    continue
        end
C---------------------------------------------------------------------------
        subroutine tsthdr(iunit,prhdr,status)

C       test the FITS header by parsing each keyword

        integer iunit,status
        integer nkeys,nmore,i,ival,numerr
        character keybuf*80,value*70,comm*72,dtype*1,sval*8,numkey*6
        character keynam*8
        logical lval,prhdr
        double precision dval

        integer nerror,nwarn
        common/messag/nerror,nwarn

        numerr=0

C	find out the number of keywords in the header
        call ftghsp(iunit,nkeys,nmore,status)
        if (status .gt. 0)then
             call writit('*** Error getting the number of keywords '//
     &       'in header ***')
             nerror=nerror+1
             return
        end if

        write(numkey,1000)nkeys
1000    format(i6)
        call writit('    Header contains'//numkey//' keywords.')
        call writit(' ')

C       print warning if SIMPLE = F
        call ftgrec(iunit,1,keybuf,status)
        if (status .gt. 0)then
             call writit('*** Error reading first header keyword ***')
             nerror=nerror+1
             numerr=numerr+1
             call dmpstk(status)
C            try resetting status and continuing...
             status=0

        else if (keybuf(1:29) .eq. 'SIMPLE  =                    ')then
            if (keybuf(30:30) .ne. 'T')then
                call writit('*** Warning: SIMPLE = F indicates '//
     &          'file may not conform to the FITS standard.')
                nwarn=nwarn+1
            end if
        end if

C	test and print out each keyword
	do 10 i=1,nkeys
C               get the header card; tests keyword name for illegal chars
		call ftgrec(iunit,i,keybuf,status)

C               print out the header record, if requested to do so
                if (prhdr)then
                    call writit(keybuf)
                end if

C               test that the keyword name contains only legal characters
                call fttkey(keybuf(1:8),status)

C               test that value/comment field contains only legal characters
                call fttrec(keybuf(9:80),status)

C               parse the value and comment fields from the record
                call ftpsvc(keybuf,value,comm,status)
C               convert the value string (if it exists) to it's native datatype
                if (value .ne. ' ')then
                    call ftc2x(value,dtype,ival,lval,sval,dval,status)
                end if
                if (status .gt. 0)then
                    write(numkey,1000)i
                    call writit('*** Error reading header keyword'//
     &                           numkey//' ***')
C                   don't duplicate listing, if is has already been printed
                    if (.not. prhdr)call writit(keybuf)
                    nerror=nerror+1
                    numerr=numerr+1
C                   print out the error message stack
                    call dmpstk(status)
C                   reset status and try to continue if less than max no. errors
                    if (numerr .le. 50)then
                            status=0
                    else
                     call writit('*** Too many header keyword errors!'
     &               //'  Verification stopped. ***')
                     return
                    end if
                else
C                   check for suspicious keywords
                    if (value .eq. ' ')then
                      keynam=keybuf(1:8)
                      if (keynam .ne. 'COMMENT ' .and.
     &                    keynam .ne. 'HISTORY '  .and.
     &                    keynam .ne. '        ' .and.
     &                    keynam .ne. 'CONTINUE' .and.
     &                    keynam .ne. 'END     ' )then

                          write(numkey,1000)i
                          call writit('*** Caution: keyword'//numkey
     &                    //' is interpreted as a comment; '
     &                    //'no ''= '' in cols. 9-10')
                          call writit(keybuf)
                          nwarn=nwarn+1
                      end if
                    end if
                end if
10	continue
        if (prhdr)then
            call writit('END     ')
            call writit(' ')
        end if

        if (numerr .eq. 0)then
            call writit('    All keywords were read successfully and'//
     &      ' conform to required FITS syntax.')
        else
         call writit('*** Errors occurred while reading keywords. ***')
        end if

C       check for duplicate keyword names
        call keydup(iunit,nkeys,status)
        if (status .gt. 0)return

C       check if the "long string keyword convention" is used
        call ftgkys(iunit,'CONTINUE',value,comm,status)
        if (status .eq. 202 .or. value .ne. ' ')then
C           file doesn't use the continuation convention
            status=0
        else
C           check that the LONGSTRN keyword is also present
            call ftgkys(iunit,'LONGSTRN',value,comm,status)
            if (status .eq. 202)then
       call writit('*** Warning: The local convention for continuing '
     &        //'the value of a string')
       call writit('        keyword over multiple header keywords is '
     &        //'used (CONTINUE keywords)')
       call writit('        without also including the recommended '
     &      //'LONGSTRN keyword.')
       call writit(' ')
              status=0
              nwarn=nwarn+1
            end if
        end if

C       now check the Header fill area to make sure it is all blank
        call ftchfl(iunit,status)
        if (status .gt. 0)then
            nerror=nerror+1
            call dmpstk(status)
C           reset status since this is not a fatal error
            status=0
        else
            call writit('    Header space following END keyword '//
     &     'is correctly filled with ASCII blanks.')
        end if
        call writit(' ')
        end
C----------------------------------------------------------------------
        subroutine keydup(iunit,nhead,status)

C       check for duplicate keyword names in the current header

C       iunit   i  fortran unit number
C       nhead   i  number of keywords
C       status  i  output error status

        integer iunit,nhead,status
        character(16) keynam
        common/nambuf/keynam(1000)
        integer keypos
        common/intbuf/keypos(1000)
        integer nkey,i,j,ndup
        character rec*80, cpos1*6, cpos2*6
        integer nerror,nwarn
        common/messag/nerror,nwarn

        if (status .gt. 0)return

C       read in the header keywords, up to maximum that will fit in the
C       buffer.  Exclude COMMENT, HISTORY, and blank keywords

	nkey=0
        do 10 i=1,nhead
          call ftgrec(iunit,i,rec,status)
          if (status .gt. 0)return

          if (rec(1:8) .eq. 'COMMENT ' .or. rec(1:8) .eq.
     &         'HISTORY '  .or. rec(1:8) .eq. '        ' )then
          else if (rec(9:10) .ne. '= ')then
C              exclude 'comment-type' keywords that don't have a value
          else if (nkey .eq. 1000)then
C              buffer is full, so cannot check any more keywords
               call writit('*** Note: only the first 1000'
     &         // ' keywords checked for duplicate names.')
               go to 20
          else
               nkey=nkey+1
               keynam(nkey)=rec(1:8)
               keypos(nkey)=i
          end if
10      continue

20      continue
C       now compare every keyword to look for duplicates

        ndup=0
        do 40 i=1,nkey-1
          if (keynam(i) .ne. ' ')then
            do 30 j=i+1,nkey
                if (keynam(i) .eq. keynam(j))then
C                   found a duplicate name
                    ndup=ndup+1
                    write(cpos1,1000)keypos(i)
                    write(cpos2,1000)keypos(j)
1000                format(i6)
                    call writit('*** Caution: Keyword '//keynam(i)(1:8)
     &             //' is repeated in header records'//cpos1//' and'
     &              //cpos2//':')
                    nwarn=nwarn+1
                    call ftgrec(iunit,keypos(i),rec,status)
                    call writit(rec)
                    call ftgrec(iunit,keypos(j),rec,status)
                    call writit(rec)
C                   erase the duplicate keyword, to eliminate multiple warnings
                    keynam(j)=' '
                end if
30          continue
          end if
40      continue
        if (ndup .eq. 1)then
             call writit(' ')
        else if (ndup .gt. 1)then
             write(cpos1,1000)ndup
             call writit('*** Total of '//cpos1(3:6)//' duplicate'
     &      //' keyword pairs were found in the header.')
             call writit(' ')
        else if (ndup .eq. 0)then
             call writit('    No duplicate keywords were found.')
        end if
        end
C---------------------------------------------------------------------------
        subroutine tstdat(iunit,exttyp,verdat,status)

C       test the FITS data by reading each value, if verdat is true
C       Otherwise just test that the fill bytes are correct
C       Also verify the checksum value if the CHECKSUM keyword exists

        integer iunit,exttyp,status
        logical verdat
        integer nerror,nwarn,dataok,hduok
        common/messag/nerror,nwarn

        if (exttyp .eq. 0)then
C             test the primary array or image extension values
              call tstimg(iunit,verdat,status)
        else
C             test the ASCII or BINTABLE values
              call tsttbl(iunit,exttyp,verdat,status)
        end if

        if (status .gt. 0)return

C       test that the fill area is filled correctly
        call ftcdfl(iunit,status)
        if (status .gt. 0)then
            call dmpstk(status)
            nerror=nerror+1
C           reset status since this is (usually) not a fatal error
            status=0
        else
            call writit(' ')
            call writit('    Empty space (if any) following last'//
     &      ' data record is correctly filled.')
        end if

C       compute the checksum for the data and the entire HDU
        call ftvcks(iunit,dataok,hduok,status)
        if (status .gt. 0)return

        if (dataok .eq. 0 .and. hduok .eq. 0)return
        call writit(' ')

        if (dataok .eq. 1)then
               call writit('    The data record checksum (given'//
     &         ' by the DATASUM  keyword) is correct.')
        else if (dataok .eq. -1)then
                call writit('*** Warning: computed checksum'//
     &         ' is not consistent with the DATASUM keyword.')
               nwarn=nwarn+1
        end if

       if (hduok .eq. 1)then
            call writit('    The checksum value for the entire HDU'//
     &     ' is correct ( = 0).')
        else if (dataok .eq. 1 .and. hduok .eq. -1)then
            call writit('*** Warning: Data records are OK, but the '//
     &         'header has incorrect checksum value.')
            call writit('    The header has'//
     &         ' been modified or corrupted.')
            nwarn=nwarn+1
        else if (hduok .eq. -1)then
            call writit('*** Warning: computed HDU checksum'//
     &     ' is not consistent with the CHECKSUM keyword.')
             call writit('    The header and/or data records have'//
     &         ' been modified or corrupted.')
            nwarn=nwarn+1
        end if
        end
C---------------------------------------------------------------------------
        subroutine tstimg(iunit,verdat,status)

C       test the image data (primary array or image extension)

        integer maxpix
        parameter (maxpix=1000)
        integer iunit,status
        integer maxdim,bitpix,naxis,naxes(99),pcount,gcount,npix,i
        integer nleft, ntodo,fpixel,numnul,igrp
        double precision dvals(maxpix),dmin,dmax
        logical simple,extend,fgvals(maxpix),anyflg,groups,verdat
        character cnpix*9,cnnull*9,cmin*15,cmax*15,emin*9,emax*9
        character caxis*4,caxes*9,cnaxis*8,cgrp*9,comm*20
        integer nerror,nwarn
        common/messag/nerror,nwarn


C       get the required keywords
        maxdim=99
        call ftghpr(iunit,maxdim,simple,bitpix,naxis,naxes,pcount,
     &              gcount,extend,status)

        if (status .gt. 0)return

        if (naxis .eq. 0)then
             call writit('    Null data array; NAXIS = 0')
             return
        else
             write(caxis,1002)naxis
1002         format(i4)
             call writit('    NAXIS ='//caxis)
        end if

        if (naxis .gt. 1 .and. naxes(1) .eq. 0)then
C            this case is needed for the old Random Group FITS files
                call ftgkyl(iunit,'GROUPS',groups,comm,status)
                if (status .gt. 0)then
                        status=0
                        groups=.false.
                end if
                if (groups)then
                    npix=1
                else
                    npix=0
                end if
        else
             npix=naxes(1)
        end if

        write(caxes,1000)naxes(1)
        call writit('         NAXIS1  ='//caxes)
        do 10 i=2,naxis
             npix=npix*naxes(i)
C            concatinate the axis number to the root NAXIS string:
             call ftkeyn('NAXIS',i,cnaxis,status)
             if (status .gt. 0)return
             write(caxes,1000)naxes(i)
             call writit('         '//cnaxis//'='//caxes)
10      continue

        if (npix .eq. 0)then
              call writit('    Null data array; one or more axes = 0')
              return
        end if

C       return if user doesn't want to read every data value
        if (.not. verdat)return

C       Now read all the data, MAXPIX pixels at a time; do each group
        do 200 igrp=1,gcount
        nleft=npix
        fpixel=1
        numnul=0
        dmin=1.0E+37
        dmax=-1.0e+37
20      ntodo=min(nleft,maxpix)
        if (ntodo .eq. 0)go to 100

C       read a buffer full of pixels (as double precision values)
        call ftgpfd(iunit,igrp,fpixel,ntodo,dvals,fgvals,anyflg,status)
        if (status .gt. 0)then
            write(emin,1000)fpixel
            write(emax,1000)fpixel+ntodo-1
1000        format(i9)
            call writit('*** Error reading image pixels'//emin//
     &                  ' -'//emax)
            nerror=nerror+1
            return
        end if
        do 30 i=1,ntodo
               if (fgvals(i))then
C                   count the number of null values in the array
                    numnul=numnul+1
               else
C                   calculate the min and max of all the good pixels
                    dmin=min(dmin,dvals(i))
                    dmax=max(dmax,dvals(i))
               end if
30      continue
        nleft=nleft-ntodo
        fpixel=fpixel+ntodo
        go to 20

100     continue
C       read all the data; print statistics.
        write(cnpix,1000)npix
        write(cnnull,1000)numnul
        write(cmin,1001)dmin
        write(cmax,1001)dmax
1001    format(g15.7)
        if (gcount .gt. 1)then
            write(cgrp,1000)igrp
            call writit('    Group'//cgrp//':  No. pixels ='//
     &        cnpix//'.  No. of null pixels ='//cnnull//'.')
        else
            call writit('    Total no. of pixels ='//cnpix//
     &        '.  No. of null pixels ='//cnnull//'.')
        end if
        call writit('    Data min. and max. ='//cmin//','//cmax)

200     continue
        end
C---------------------------------------------------------------------------
        subroutine tsttbl(iunit,exttyp,verdat,status)

C       test the table data (TABLE or BINTABLE extension)

        integer iunit,exttyp,status
        integer maxdim,rowlen,nrows,tfield,tbcol(999),vardat,i
        integer dattyp,repeat,width,numnul,decim,nulval
        character(40) ttype(999)
        character(12) tform(999),tunit(999)
        character cfield*4,crows*9,cnnull*9,cmin*15,cmax*15,extnam*24
        character keywrd*8,comm*8
        double precision dmin,dmax
        logical verdat
        integer nerror,nwarn
        common/messag/nerror,nwarn

C       get the required parameters
        maxdim=999

        if (exttyp .eq. 1)then
C                get required keyword values for ASCII TABLE
                 call ftghtb(iunit,maxdim,rowlen,nrows,tfield,
     &           ttype,tbcol,tform,tunit,extnam,status)
        else
C                get required keyword values for BINTABLE
                 call ftghbn(iunit,maxdim,nrows,tfield,ttype,
     &           tform,tunit,extnam,vardat,status)
        end if

        if (status .gt. 0)return

        write(cfield,1000)tfield
        write(crows,1001)nrows
        call writit('    Table '//extnam// 'contains'//cfield
     &              //' columns and'//crows//' rows.')

C       test the column names (TTYPEn keywords)
        call tsttyp(iunit,tfield,status)

        if (verdat .and. nrows .gt. 0)then
            call writit('  Col  TTYPE         TFORM     TUNIT'
     &       //'         Nulls  Data Min.      Data Max.')
        else
            call writit('  Col  TTYPE         TFORM     TUNIT')
        end if

C       read each column of the table in sequence
        do 10 i=1,tfield
          write(cfield,1000)i
          cmin=' '
          cmax=' '

C         check for illegal use of TNULLn keywords in binary tables
          if (exttyp .eq. 2)then
C           construct TNULLn keyword name, then read it
            call ftkeyn('TNULL',i,keywrd,status)
            call ftgkyj(iunit,keywrd,nulval,comm,status)
C           (Note: FITSIO itself will issue an error if TNULLn is not an integer

            if (status .eq. 202)then
C               null value not specified; OK, no error
                status=0
            else if (status .le. 0)then
C               is this a B, I or J column? (only these can have TNULL keyword)
                call ftbnfm(tform(i),dattyp,repeat,width,status)
                if (abs(dattyp) .eq. 11)then
                   if (nulval .lt. 0 .or. nulval .gt. 255)then
                      call writit('*** Error: '//keywrd//' value'//
     &                ' exceeds 0 - 255 range for a B datatype.')
                      nerror=nerror+1
                   end if
                else if (abs(dattyp) .eq. 21)then
                   if (nulval .lt. -32768 .or. nulval .gt. 32767)then
                      call writit('*** Error: '//keywrd//' value'//
     &                ' exceeds range for a I*2 datatype.')
                      nerror=nerror+1
                   end if
                else if (abs(dattyp) .eq. 41)then
C                  don't need to do range check
                else
                   call writit('*** Error: '//keywrd//' keyword'//
     &             ' is illegal for this column datatype.')
                      nerror=nerror+1
                end if
            end if
          end if

          if (verdat .and. nrows .gt. 0)then
C           determine the datatype of the column
            if (exttyp .eq. 1)then
                repeat=1
                call ftasfm(tform(i),dattyp,width,decim,status)
                if (status .gt. 0)return
            else
                call ftbnfm(tform(i),dattyp,repeat,width,status)
                if (status .gt. 0)return
                if (dattyp .eq. 1)then
C                       treat Bit datatype as if it were a Byte datatype
                        dattyp=11
                        repeat=(repeat+7)/8
                end if
            end if

           if (dattyp .lt. 0)then
C               flag that this is a variable length col by setting repeat=-1
                dattyp=-dattyp
                repeat=-1
           end if

           if (dattyp .eq. 16)then
C               this is a character string column
                call tstscl(iunit,i,nrows,repeat,
     &                      numnul,status)
                write(cnnull,1001)numnul
            else if (dattyp .eq. 14)then
C               this is a logical column
                call tstlcl(iunit,i,nrows,repeat,
     &                      numnul,status)
                write(cnnull,1001)numnul
            else if (dattyp .eq. 83)then
C               this is a complex column
                call tstccl(iunit,i,nrows,repeat,
     &                      numnul,dmin,dmax,status)
                write(cmin,1002)dmin
                write(cmax,1002)dmax
                write(cnnull,1001)numnul
            else if (dattyp .eq. 163)then
C               this is a double complex column
                call tstmcl(iunit,i,nrows,repeat,
     &                      numnul,dmin,dmax,status)
                write(cmin,1002)dmin
                write(cmax,1002)dmax
                write(cnnull,1001)numnul
            else
C               read all other numeric type columns in the same way
                call tstncl(iunit,i,nrows,repeat,
     &                      numnul,dmin,dmax,status)
                write(cmin,1002)dmin
                write(cmax,1002)dmax
                write(cnnull,1001)numnul
            end if

            if (status .gt. 0)return

1000        format(i4)
1001        format(i9)
1002        format(g15.7)

            call writit(' '//cfield//' '//
     &      ttype(i)(1:16)//' '//tform(i)(1:8)//' '//tunit(i)(1:8)
     &      //cnnull//cmin//cmax)

          else

C           did not read all the data values, so just print labels
            call writit(' '//cfield//'  '//
     &      ttype(i)(1:16)//' '//tform(i)(1:8)//' '//tunit(i)(1:8))
          end if
10      continue
        end
C----------------------------------------------------------------------
        subroutine tsttyp(iunit,tfield,status)

C       test the column names in a table.  Every column should have
C       a unique name (within 16 characters and preferably within 8 chars)
C       and only contain letters digits, or the underscore character.

C       iunit   i  fortran unit number
C       tfield  i  number of columns in the table
C       status  i  output error status

        integer iunit,tfield,status,i,j,ndup
        character keywrd*8,tmpnam*68,comm*8,cpos1*3, cpos2*3
        logical pntrec
        character(16) colnam
        common/nambuf/colnam(1000)
        integer nerror,nwarn
        common/messag/nerror,nwarn

C       read in the column names
        pntrec=.false.
        do 10 i=1,tfield
            if (status .gt. 0)return

            colnam(i)=' '
            write(cpos1,1000)i
1000        format(i3)

C           construct column name keyword
            call ftkeyn('TTYPE',i,keywrd,status)
C           read the column name
            call ftgkys(iunit,keywrd,tmpnam,comm,status)
            if (status .eq. 202)then
                call writit('*** Warning: Column '//cpos1//
     &          ' has no name (no '//keywrd//' keyword).')
                nwarn=nwarn+1
                status=0
            else
C               test for illegal characters
                call tstchr(tmpnam,cpos1,pntrec,status)
C               store first 16 characters of name, and convert to upper case
                colnam(i)=tmpnam
                call ftupch(colnam(i))
            end if
10      continue

C       now compare every keyword to look for duplicates

        ndup=0
C       first check for non-uniqueness within 16 characters (a warning)
        do 40 i=1,tfield-1
          if (colnam(i) .ne. ' ')then
            do 30 j=i+1,tfield
                if (colnam(i) .eq. colnam(j))then
C                   found a duplicate name
                    ndup=ndup+1
                    write(cpos1,1000)i
                    write(cpos2,1000)j
                    call writit('*** Warning: column names '//
     &              cpos1//' and '//cpos2//' are not unique within '//
     &              'first 16 characters.')

C                   erase the duplicate name, to eliminate multiple warnings
                    colnam(j)=' '
                    nwarn=nwarn+1
                end if
30          continue
          end if
40      continue

C       now check for non-uniqueness within 8 characters (a caution)
        do 60 i=1,tfield-1
          if (colnam(i) .ne. ' ')then
            do 50 j=i+1,tfield
                if (colnam(i)(1:8) .eq. colnam(j)(1:8))then
C                   found a duplicate name
                    ndup=ndup+1
                    write(cpos1,1000)i
                    write(cpos2,1000)j
                    call writit('*** Caution: column names '//
     &              cpos1//' and '//cpos2//' are not unique within '//
     &              'first 8 characters.')

C                   erase the duplicate name, to eliminate multiple warnings
                    colnam(j)=' '
                    nwarn=nwarn+1
                end if
50          continue
          end if
60      continue

        if (ndup .gt. 1)then
             write(cpos1,1000)ndup
             call writit('*** Total of '//cpos1//' non-unique'
     &      //' column name pairs were found.')
             call writit(' ')
        end if
        end
C----------------------------------------------------------------------
        subroutine tstchr(keynam,ncol,pntrec,status)

C       test that column name contains only legal characters:
C       letters, numbers or underscore, or space
C       Also check that name begins with a letter.

C       keynam  c*68  keyword name
C       ncol    C*3  column number (ASCII string)
C       pntrec  logical  has the recommendation already been printed?
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)

        character keynam*68,ncol*3,c1*1
        integer status,i
        logical spaces,pntrec
        integer nerror,nwarn
        common/messag/nerror,nwarn

        if (status .gt. 0)return

         c1=keynam(1:1)
        if ((c1 .lt. 'A' .or. c1 .gt. 'Z') .and.
     &      (c1 .lt. 'a' .or. c1 .gt. 'z'))then
            call writit('*** Caution: name of column '//ncol//
     &              ' does not start with a letter.')
            nwarn=nwarn+1
        end if

        spaces=.false.
        do 10 i=1,68
            c1=keynam(i:i)
            if ((c1 .ge. 'A' .and. c1 .le. 'Z') .or.
     &          (c1 .ge. 'a' .and. c1 .le. 'z') .or.
     &          (c1 .ge. '0' .and. c1 .le. '9') .or.
     &           c1 .eq. '_')then
                 if (spaces)then
                    call writit('*** Caution: name of column '//ncol//
     &              ' contains embedded space(s).')
                    nwarn=nwarn+1
                    return
                 end if
            else if (c1 .eq. ' ')then
                 spaces=.true.
            else
C                non-standard character found
                 call writit('*** Caution: name of column '//ncol//
     &              ' contains non-standard character(s).')
                 if (.not. pntrec)then
                     call writit('    (Should only use letters, '//
     &               'digits, and the underscore character).')
                     pntrec=.true.
                 end if
                 nwarn=nwarn+1
                 return
            end if
10      continue
        end
C---------------------------------------------------------------------------
        subroutine tstncl(iunit,colnum,nrows,repeat,
     &             numnul,dmin,dmax,status)

C       test the values in a numerical column of a TABLE or BINTABLE extension

        integer maxpix
        parameter (maxpix=1000)
        integer iunit,colnum,nrows,repeat,status
        integer nleft,frow,felem,numnul,ntodo,i,offset,delrow,delelm
        double precision dmin,dmax,dvals(maxpix)
        logical anyflg,fgvals(maxpix),fixcol
        character ccol*4
        integer nerror,nwarn
        common/messag/nerror,nwarn

        if (status .gt. 0)return

        delrow = 0
        delelm = 0

C       Read all the data, maximum of MAXPIX pixels at a time
        if (repeat .lt. 0)then
C           variable length vector; have to read one row at a time
            fixcol=.false.
C           ftgdes return the number vector  elements in the row
            call ftgdes(iunit,colnum,1,repeat,offset,status)
            nleft=repeat
        else if (repeat .eq. 0)then
C           this is a null column
            dmin=0.
            dmax=0.
            numnul=0
            return
        else
            fixcol=.true.
            nleft=nrows*repeat
            delrow=maxpix/repeat
            delelm=maxpix-delrow*repeat
        end if

        frow=1
        felem=1
        numnul=0
        dmin=1.0E+37
        dmax=-1.0e+37

20      ntodo=min(nleft,maxpix)

C       read a buffer full of pixels

C       skip read if no data present (eg, zero-length variable column)
        if (ntodo.gt.0)
     &       call ftgcfd(iunit,colnum,frow,felem,ntodo,dvals,fgvals,
     &                   anyflg,status)
        if (status .gt. 0)then
            write(ccol,1001)colnum
1001        format(i4)
            call writit('*** Error reading values from column'//
     &      ccol// ' into a double precision data array.')
            nerror=nerror+1
            return
        end if

C       calculate the min and max and number of null pixels
        do 30 i=1,ntodo
               if (fgvals(i))then
                    numnul=numnul+1
               else
                    dmin=min(dmin,dvals(i))
                    dmax=max(dmax,dvals(i))
               end if
30      continue

        nleft=nleft-ntodo
        if (nleft .eq. 0)then
            if (fixcol)then
C                this is a fixed length column so we're finished
                 go to 100
            else
C                check if we've done all the rows of the variable length column
                 if (frow .eq. nrows)then
                     go to 100
                 else
                     frow=frow+1
                     felem=1
                     call ftgdes(iunit,colnum,frow,repeat,offset,status)
                     nleft=repeat
                 end if
             end if
        else
C           calculate the starting row and element number for next read
            if (fixcol .and. repeat .eq. 1)then
C              not a vector column, so simply increment row number of next read
               frow=frow+ntodo
            else if (fixcol)then
C              this is a fixed length vector column; calc new starting row
C              and element position for next read
               frow=frow+delrow
               felem=felem+delelm
               if (felem .gt. repeat)then
                    felem=felem-repeat
                    frow=frow+1
               end if
            else
C              this is a variable length column; increment the starting element
               felem=felem+ntodo
            end if
        end if

        go to 20

100     continue
        end
C---------------------------------------------------------------------------
        subroutine tstscl(iunit,colnum,nrows,repeat,
     &             numnul,status)

C       test the values in a string column of a TABLE or BINTABLE extension

        integer iunit,colnum,nrows,repeat,status
        integer nleft,frow,felem,numnul,ntodo,i
        logical anyflg,fgvals(1000)
        character(16) strings(1000)
        character ccol*4
        integer nerror,nwarn
        common/messag/nerror,nwarn

        if (status .gt. 0)return

C       Read one just C*16 string from each row
C       this may not work for variable length column?!

        nleft=nrows
        frow=1
        felem=1
        numnul=0
20      ntodo=min(nleft,1000)
        if (ntodo .eq. 0)go to 100

C       read a buffer full of pixels
        call ftgcfs(iunit,colnum,frow,felem,ntodo,strings,fgvals,
     &              anyflg,status)
        if (status .gt. 0)then
            write(ccol,1001)colnum
1001        format(i4)
            call writit('*** Error reading String values from'
     &      //' column'//ccol)
            nerror=nerror+1
            return
        end if
        do 30 i=1,ntodo
               if (fgvals(i))then
C                   count the number of null values in the array
                    numnul=numnul+1
               end if
30      continue

        nleft=nleft-ntodo
C       calculate the starting row and element number for next read
        frow=frow+ntodo
        go to 20

100     continue
        end
C---------------------------------------------------------------------------
        subroutine tstlcl(iunit,colnum,nrows,repeat,
     &             numnul,status)

C       test the values in a logical column of a BINTABLE extension

        integer maxpix
        parameter (maxpix=1000)
        integer iunit,colnum,nrows,repeat,status
        integer nleft,frow,felem,numnul,ntodo,i,offset,delrow,delelm
        logical lvals(maxpix),anyflg,fgvals(maxpix),fixcol
        character ccol*4
        integer nerror,nwarn
        common/messag/nerror,nwarn

        if (status .gt. 0)return

        delrow = 0
        delelm = 0

C       Read all the data, maximum of MAXPIX pixels at a time
        if (repeat .lt. 0)then
C           variable length vector; have to read one row at a time
            fixcol=.false.
C           ftgdes return the number vector  elements in the row
            call ftgdes(iunit,colnum,1,repeat,offset,status)
            nleft=repeat
        else
            fixcol=.true.
            nleft=nrows*repeat
            delrow=maxpix/repeat
            delelm=maxpix-delrow*repeat
        end if

        frow=1
        felem=1
        numnul=0

20      ntodo=min(nleft,maxpix)

C       read a buffer full of pixels
        call ftgcfl(iunit,colnum,frow,felem,ntodo,lvals,fgvals,
     &              anyflg,status)
        if (status .gt. 0)then
            write(ccol,1001)colnum
1001        format(i4)
            call writit('*** Error reading Logical values from'
     &      //' column'//ccol)
            nerror=nerror+1
            return
        end if

C       calculate thenumber of null pixels
        do 30 i=1,ntodo
               if (fgvals(i))then
                    numnul=numnul+1
               end if
30      continue

        nleft=nleft-ntodo
        if (nleft .eq. 0)then
            if (fixcol)then
C                this is a fixed length column so we're finished
                 go to 100
            else
C                check if we've done all the rows of the variable length column
                 if (frow .eq. nrows)then
                     go to 100
                 else
                     frow=frow+1
                     felem=1
                     call ftgdes(iunit,colnum,frow,repeat,offset,status)
                     nleft=repeat
                 end if
             end if
        else
C           calculate the starting row and element number for next read
            if (fixcol .and. repeat .eq. 1)then
C              not a vector column, so simply increment row number of next read
               frow=frow+ntodo
            else if (fixcol)then
C              this is a fixed length vector column; calc new starting row
C              and element position for next read
               frow=frow+delrow
               felem=felem+delelm
               if (felem .gt. repeat)then
                    felem=felem-repeat
                    frow=frow+1
               end if
            else
C              this is a variable length column; increment the starting element
               felem=felem+ntodo
            end if
        end if

        go to 20

100     continue
        end
C---------------------------------------------------------------------------
        subroutine tstccl(iunit,colnum,nrows,repeat,
     &             numnul,dmin,dmax,status)

C       test the values in a complex column of a BINTABLE extension

        integer maxpix
        parameter (maxpix=1000)
        integer iunit,colnum,nrows,repeat,status
        integer nleft,frow,felem,numnul,ntodo,i,j,offset,delrow,delelm
        double precision dmin,dmax
        real rvals(2000),rmin,rmax
        logical anyflg,fgvals(maxpix),fixcol
        character ccol*4
        integer nerror,nwarn
        common/messag/nerror,nwarn

        if (status .gt. 0)return

        delrow = 0
        delelm = 0
        rmin=1.0E+37
        rmax=-1.0e+37

C       Read all the data, maximum of MAXPIX pixels at a time
        if (repeat .lt. 0)then
C           variable length vector; have to read one row at a time
            fixcol=.false.
C           ftgdes return the number vector  elements in the row
            call ftgdes(iunit,colnum,1,repeat,offset,status)
            nleft=repeat
        else
            fixcol=.true.
            nleft=nrows*repeat
            delrow=maxpix/repeat
            delelm=maxpix-delrow*repeat
        end if

        frow=1
        felem=1
        numnul=0
        dmin=1.0E+37
        dmax=-1.0e+37
20      ntodo=min(nleft,maxpix)
        if (ntodo .eq. 0)go to 100

C       read a buffer full of pixels
        call ftgcfc(iunit,colnum,frow,felem,ntodo,rvals,fgvals,
     &              anyflg,status)
         if (status .gt. 0)then
            write(ccol,1001)colnum
1001        format(i4)
            call writit('*** Error reading Complex values from'
     &      //' column'//ccol)
            nerror=nerror+1
            return
        end if

C       calculate the min and max and number of null pixels
        j=1
        do 30 i=1,ntodo
               if (fgvals(i))then
C                   count the number of null values in the array
                    numnul=numnul+1
                    j=j+2
               else
C                   calculate the min and max of all the good pixels
                    rmin=min(rmin,rvals(j))
                    rmax=max(rmax,rvals(j))
                    j=j+1
                    rmin=min(rmin,rvals(j))
                    rmax=max(rmax,rvals(j))
                    j=j+1
               end if
30      continue
        nleft=nleft-ntodo
        if (nleft .eq. 0)then
            if (fixcol)then
C                this is a fixed length column so we're finished
                 go to 100
            else
C                check if we've done all the rows of the variable length column
                 if (frow .eq. nrows)then
                     go to 100
                 else
                     frow=frow+1
                     felem=1
                     call ftgdes(iunit,colnum,frow,repeat,offset,status)
                     nleft=repeat
                 end if
             end if
        else
C           calculate the starting row and element number for next read
            if (fixcol .and. repeat .eq. 1)then
C              not a vector column, so simply increment row number of next read
               frow=frow+ntodo
            else if (fixcol)then
C              this is a fixed length vector column; calc new starting row
C              and element position for next read
               frow=frow+delrow
               felem=felem+delelm
               if (felem .gt. repeat)then
                    felem=felem-repeat
                    frow=frow+1
               end if
            else
C              this is a variable length column; increment the starting element
               felem=felem+ntodo
            end if
        end if

        go to 20

100     continue
        dmin=rmin
        dmax=rmax
        end
C---------------------------------------------------------------------------
        subroutine tstmcl(iunit,colnum,nrows,repeat,
     &             numnul,dmin,dmax,status)

C       test the values in a double complex column of a BINTABLE extension

        integer maxpix
        parameter (maxpix=1000)
        integer iunit,colnum,nrows,repeat,status
        integer nleft,frow,felem,numnul,ntodo,i,j,offset,delrow,delelm
        double precision dvals(2000),dmin,dmax
        logical anyflg,fgvals(maxpix),fixcol
        character ccol*4
        integer nerror,nwarn
        common/messag/nerror,nwarn

        if (status .gt. 0)return

        delrow = 0
        delelm = 0

C       Read all the data, maximum of MAXPIX pixels at a time
        if (repeat .lt. 0)then
C           variable length vector; have to read one row at a time
            fixcol=.false.
C           ftgdes return the number vector  elements in the row
            call ftgdes(iunit,colnum,1,repeat,offset,status)
            nleft=repeat
        else
            fixcol=.true.
            nleft=nrows*repeat
            delrow=maxpix/repeat
            delelm=maxpix-delrow*repeat
        end if

        frow=1
        felem=1
        numnul=0
        dmin=1.0E+37
        dmax=-1.0e+37

20      ntodo=min(nleft,maxpix)
        if (ntodo .eq. 0)go to 100

C       read a buffer full of pixels
        call ftgcfm(iunit,colnum,frow,felem,ntodo,dvals,fgvals,
     &              anyflg,status)
         if (status .gt. 0)then
            write(ccol,1001)colnum
1001        format(i4)
            call writit('*** Error reading Double Complex values'
     &      //' from column'//ccol)
            nerror=nerror+1
            return
        end if
        j=1
        do 30 i=1,ntodo
               if (fgvals(i))then
C                   count the number of null values in the array
                    numnul=numnul+1
                    j=j+2
               else
C                   calculate the min and max of all the good pixels
                    dmin=min(dmin,dvals(j))
                    dmax=max(dmax,dvals(j))
                    j=j+1
                    dmin=min(dmin,dvals(j))
                    dmax=max(dmax,dvals(j))
                    j=j+1
               end if
30      continue

        nleft=nleft-ntodo
        if (nleft .eq. 0)then
            if (fixcol)then
C                this is a fixed length column so we're finished
                 go to 100
            else
C                check if we've done all the rows of the variable length column
                 if (frow .eq. nrows)then
                     go to 100
                 else
                     frow=frow+1
                     felem=1
                     call ftgdes(iunit,colnum,frow,repeat,offset,status)
                     nleft=repeat
                 end if
             end if
        else
C           calculate the starting row and element number for next read
            if (fixcol .and. repeat .eq. 1)then
C              not a vector column, so simply increment row number of next read
               frow=frow+ntodo
            else if (fixcol)then
C              this is a fixed length vector column; calc new starting row
C              and element position for next read
               frow=frow+delrow
               felem=felem+delelm
               if (felem .gt. repeat)then
                    felem=felem-repeat
                    frow=frow+1
               end if
            else
C              this is a variable length column; increment the starting element
               felem=felem+ntodo
            end if
        end if

        go to 20

100     continue
        end
