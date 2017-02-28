C*************************************************************************
C SUBROUTINE:
C       FFILECAT
C
C FILE: 
C       ffilecat.f
C
C DESCRIPTION:
C       Takes the values of a series of keywords in the input files,
C       and puts them into the rows of the binary extension in outfile.
C
C AUTHOR/DATE:
C       Jim Ingham 1/5/93
C
C MODIFICATION HISTORY:
C       Ron Zellar 6/24/93 -- Added the aform parameter
C       Ron Zellar 7/29/93 -- Added a check for the aform value
C       8/23/94 EAG 3.0a - clobber capability
C       12/17/97 PDW 3.0b - Changed error-opening-file test to conform
C                           to CFITSIO's return code
C
C NOTES:
C       
C       
C
C USAGE:
C       call ffilet 
C       
C ARGUMENTS:
C       
C
C PRIMARY LOCAL VARIABLES:
C       vexten  - The vector containing the input extensions
C       vinfile - The vector containing the input file names
C       ninfiles        - The number of input files
C       nkywds  - the number of keywords to be written out
C       vkywds  - the vector containing the keywords to be copied
C       maxin           - The maximum number of input files
C       maxky           - The maximun number of keywords to copy
C       maxlen  - The maximum length for character strings
C       outextn - the output file extension number
C       outfile - the output filename
C       ounit           - The unit number for outfile
C       outexist        - Boolean- T if an output file exists...
C       nprevlines      - Number of lines in the (existing) outfile extension
C       tform           - The output tform keyword for the keywords
C       ttype           - The output ttype keyword for the keywords
C       svalues - The string array containing the keyword values
C       omit            - Boolean- T if bad input is omitted...
C       fstat           - the error flag
C       
C       
C CALLED ROUTINES:
C       ffgpar  - Gets the parameters from the par file
C       ffoldout        - Checks for existing outfile, opens it, and
C                         reads in the keywords 
C       ffrin           - reads in the keyword values
C       ffchk           - Checks datatype for an existing outfile
C       ffinitjci  - If no outfile exists, creates one
C       fftrans - writes the keyword values to the outfile
C       fcecho  - Echoes to the screen            
C
C******************************************************************************
      subroutine ffilet
      
      integer maxin, maxky
      parameter(maxin = 999, maxky = 90)
      character(8) vkywds(maxky)
      character(160) outfile,vinfile(maxin)
      character(80) svalues(maxky,maxin),context
      character(16) tform(maxky)
      character(16) aform,eform,iform
      integer vexten(maxin),ninfiles,nkywds,maxlen,outextn,ounit
      integer fstat,nprevlines,minlen, afval
      logical outexist,omit,quiet

      character(40) taskname
      common /task/ taskname

      taskname = 'ffilecat3.0a'

      call ftcmsg
      
      ounit = 16
      
C   First get the parameters, and parse them

      call ffgpar(outfile,outextn,vinfile,vexten,ninfiles,
     &     vkywds,nkywds,maxlen,minlen,aform,afval,eform,iform,
     &     quiet,omit,fstat)

      if(fstat.ne.0) then
         context = 'Error in ffgpar'
         call fcerr(context)
         goto 999
      endif

C   If the outfile already exists, then get the keyword values from it
      
      call ffoldout(ounit,outfile,outextn,vkywds,nkywds,
     &     outexist,tform,nprevlines,fstat)
      if(fstat.ne.0) then
         context = 'Error in ffoldout'
         call fcerr(context)
         goto 999
      endif
      
      
C   Now read in the infiles, and get the keyword values 

      call ffrin(vinfile,vexten,ninfiles,vkywds,nkywds,
     &     svalues,quiet,fstat)
      
      if(fstat.ne.0) then
         context = 'Error in ffrin'
         call fcerr(context)
         goto 999
      endif   
      
C   If the outfile exists, check that all the new input data is of the 
C   proper format

      if(outexist) then
         call ffchk(vinfile,ninfiles,vkywds,nkywds,svalues,
     &        tform,omit,fstat)
         if(fstat.ne.0) then
            context = 'Error in ffchk'
            call fcerr(context)
            goto 999
         endif        

C   Else open the outfile if it is new and initialize it
         
      else
         call ffinitjci(outfile,outextn,ounit,tform,ninfiles,vinfile,
     &        vkywds,nkywds,svalues,maxlen,minlen,aform,afval,eform,
     &        iform,omit,quiet,fstat)
         if(fstat.ne.0) then
            context = 'Error in ffinitjci'
            call fcerr(context)
            goto 999
         endif        
      endif
      
C   Now transfer the values to the outfile

      call fftrans(ounit,vkywds,nkywds,tform,outexist,nprevlines,
     &     vinfile,vexten,ninfiles,svalues,fstat)
      if(fstat.ne.0) then
         context = 'Error in fftrans'
         call fcerr(context)
         goto 999
      endif   

 999  return
      end

C*************************************************************************
C SUBROUTINE:
C       FFGPAR  
C
C FILE: 
C       ffilecat.f
C
C DESCRIPTION:
C       This routine gets the parameters from the par file and parses
C       them.   
C
C AUTHOR/DATE:
C       Jim Ingham 1/5/93
C
C MODIFICATION HISTORY:
C       Ron Zellar 6/24/93 -- Added the aform keyword
C       Ron Zellar 7/29/93 -- Added check for the aform value
C       Jim Ingham 6/06/94 -- Lengthened kwds, tmep, infile and filename
C
C NOTES:
C
C USAGE:
C       call ffgpar(outfile,outextn,vinfile,vexten,ninfiles,
C     &         vkywds,nkywds,maxlen,minlen,aform,eform,iform,
C     &         quiet,omit,fstat)
C       
C ARGUMENTS:
C       outfile - The output filename
C       outextn - The output file extension
C       vinfile - The vector of input files
C       vexten  - The vector of input extensions
C       ninfiles        - The number of infiles
C       vkywds  - The vector of keywords
C       nkywds  - The number of keywords
C       maxlen  - The maximum length for character strings
C       omit            - Boolean- T if bad inputs are omitted
C       fstat           - The error flag        
C
C PRIMARY LOCAL VARIABLES:
C       infile  - The string containing the list of input files 
C       kywds           - The string containing the list of keywords
C       
C       
C CALLED ROUTINES:
C       uclgs*  - Gets parameters from the par file     
C       fcstln  - Returns length of string without trailing blanks
C       fcpars  - Parses outfile[#] into outfile and #
C       fcgcls  - Parses blank-delimited string into its elements
C       fcecho  - Echoes to the screen
C       
C
C******************************************************************************
      subroutine ffgpar(outfile,outextn,vinfile,vexten,ninfiles,
     &     vkywds,nkywds,maxlen,minlen,aform,afval,eform,iform,
     &     quiet,omit,fstat)
      
      integer maxin, maxky
      parameter(maxin = 999, maxky = 90)
      character*(*) vkywds(maxky)
      character*(*) aform,eform,iform
      character*(*) vinfile(maxin), outfile
      character(80)   context
      character(255) kywds, infile, filename, temp
      integer vexten(maxin),ninfiles,nkywds,afval,aflen
      integer maxlen,minlen,outextn,i,fstat,fcstln
      logical negflag,omit,quiet
      
C   First read in the parameters
c   (after initializing fstat, of course -- MJT 24July96)
      fstat=0

      call uclgst('infile',infile,fstat)
      if(fstat.ne.0) then
         context = 'could not get infile parameter'
         call fcerr(context)
         goto 999
      endif

      call uclgst('outfile',filename,fstat)
      if(fstat.ne.0) then
         context = 'could not get outfile parameter'
         call fcerr(context)
         goto 999
      endif

      call uclgst('keywords',kywds,fstat)
      if(fstat.ne.0) then
         context = 'could not get keywords parameter'
         call fcerr(context)
         goto 999
      endif

      call uclgsi('maxlen',maxlen,fstat)
      if(fstat.ne.0) then
         context = 'could not get maxlen parameter'
         call fcerr(context)
         goto 999
      endif

      call uclgsi('minlen',minlen,fstat)
      if(fstat.ne.0) then
         context = 'could not get minlen parameter'
         call fcerr(context)
         goto 999
      endif

      call uclgst('aform',aform,fstat)
      if(fstat.ne.0) then
         context = 'could not get aform parameter'
         call fcerr(context)
         goto 999
      endif

C   check the aform parameter value format
C   return afval to be used in ffinitjci
      aflen = fcstln(aform)
      if (aform(:aflen) .ne. 'NONE') then
         afval = 1000
         call ftc2ii(aform(2:aflen),afval,fstat)
         if ((fstat .ne. 0) .or. (aform(:1) .ne. 'A')) then
            context = 'bad aform parameter value'
            call fcerr(context)
            goto 999
         endif
      endif

      call uclgst('eform',eform,fstat)
      if(fstat.ne.0) then
         context = 'could not get eform parameter'
         call fcerr(context)
         goto 999
      endif

      call uclgst('iform',iform,fstat)
      if(fstat.ne.0) then
         context = 'could not get iform parameter'
         call fcerr(context)
         goto 999
      endif

      call uclgsb('omit',omit,fstat)
      if(fstat.ne.0) then
         context = 'could not get omit parameter'
         call fcerr(context)
         goto 999
      endif

      call uclgsb('quiet',quiet,fstat)
      if(fstat.ne.0) then
         context = 'could not get quiet parameter'
         call fcerr(context)
         goto 999
      endif
C   Now parse them:
C       First the infile list

      call fcgcls(infile,vinfile,ninfiles,negflag)
      do 200 i=1,ninfiles
         temp = vinfile(i)
         call fcpars(temp,vinfile(i),vexten(i),fstat)

C EAG 8/25/93 default to 1st extension
         if (vexten(i) .eq. -99) vexten(i) = 1
         if(fstat.ne.0)  then
            write(50,context) i
 50         format('error parsing the ',i3,'th keyword')
            call fcerr(context)
            goto 999
         endif
 200  continue

C       Then the keywords list.  An empty list means the keyword 
C       list is to be taken from the column names of the outfile
      
      nkywds = fcstln(kywds)
      if(nkywds.gt.0) then
         call fcgcls(kywds,vkywds,nkywds,negflag)
      endif

C Now the outfile
      
      call fcpars(filename,outfile,outextn,fstat)

C EAG 8/25/93 default to 1st extension
      if (outextn .eq. -99) outextn = 1

      if(fstat.ne.0)  then
         context = 'error parsing the outfile keyword'
         call fcerr(context)
         goto 999
      endif

 999  return
      end


C*************************************************************************
C SUBROUTINE:
C       FFRIN   
C
C FILE: 
C       ffilecat.f      
C
C DESCRIPTION:
C       This routine opens the infiles and reads in the values into the
C       string array svalues. 
C
C AUTHOR/DATE:
C       Jim Ingham 1/5/93
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C       call ffrin(vinfile,vexten,ninfiles,vkywds,nkywds,
C     &                                         svalues,quiet,fstat)
C       
C ARGUMENTS:
C       vinfile - The vector of input files
C       vexten  - The vector of input extensions
C       ninfiles        - The number of input files
C       vkywds  - The vector of keywords
C       nkywds  - The number of keywords
C       svalues - The string array of keyword values
C       fstat           - The error flag
C
C PRIMARY LOCAL VARIABLES:
C       iunit           - The input unit number
C       
C       
C CALLED ROUTINES:
C       ft*             - Fitsio routines
C       fcecho  - Echoes to the screen
C
C******************************************************************************
      subroutine ffrin(vinfile,vexten,ninfiles,vkywds,nkywds,
     &     svalues,quiet,fstat)


      integer maxin,maxky
      parameter(maxin = 999, maxky = 90)
      character*(*) vkywds(maxky)
      character*(*) vinfile(maxin)
      character*(*) svalues(maxky,maxin)
      character(80)  context
      character(160) message
      integer vexten(maxin),ninfiles,nkywds,rwmode,block,hdutype
      integer i,j,iunit,fstat,fcstln
      logical quiet

      block = 1
      rwmode = 0
      iunit = 15
      

      do 100 i=1,ninfiles
         call ftopen(iunit,vinfile(i),rwmode,block,fstat)        
         if(fstat.ne.0) then
            message = 'Could not open infile '
     &           //vinfile(i)(:fcstln(vinfile(i)))
            call fcerr(message(1:fcstln(message)))
            goto 999
         endif
         if(vexten(i).gt.0) then
            call ftmahd(iunit,vexten(i)+1,hdutype,fstat)
            if(fstat.ne.0) then
               write(message,50) vexten(i),
     &              vinfile(i)(:fcstln(vinfile(i)))
 50            format('Could not move to extension ',i2,'of infile ',a)
               call fcerr(message(1:fcstln(message)))
               goto 999
            endif
         endif
         do 120 j=1,nkywds       
            call ftgkey(iunit,vkywds(j),svalues(j,i),context,fstat)
            if(fstat.ne.0) then
               if(.not.quiet) then
                  write(message,60) vkywds(j),
     &                 vinfile(i)(:fcstln(vinfile(i)))
 60               format('Could not get KYWD ',a,' from ',a)
                  call fcecho(message(1:fcstln(message)))
               endif
               svalues(j,i) = 'NOT_FOUND'
               fstat = 0
            endif

 120     continue
         call ftclos(iunit,fstat)
 100  continue
 999  return
      end
      
C*************************************************************************
C SUBROUTINE:
C       FFOLDOUT        
C
C FILE: 
C       ffilecat.f      
C
C DESCRIPTION:
C       This routine opens the outfile, if it exists, and reads 
C       the column headings into vkywds, and the tform keywords into
C       tform.  It also sets outexist.
C         
C
C AUTHOR/DATE:
C       Jim Ingham 1/5/92
C
C MODIFICATION HISTORY:
C
C NOTES:
C       
C USAGE:
C       call ffoldout(ounit,outfile,outextn,vkywds,nkywds,outexist,
C    &                                  tformn,nprevlines,fstat) 
C       
C ARGUMENTS:
C       ounit           - The unit number for outfile
C       outfile - the output filename
C       outextn - the output file extension number
C       nkywds  - the number of keywords to be written out
C       vkywds  - the vector containing the keywords to be copied
C       outexist        - Boolean - T if an output file exists...
C       tform           - The output tform keyword for the keywords
C       nprevlines      - Number of lines in the (existing) outfile extension
C       fstat           - the error flag
C
C PRIMARY LOCAL VARIABLES:
C       
C       
C CALLED ROUTINES:
C       ft*             - Fitsio routines       
C       fcecho  - echoes to the screen
C       fcerrm  - echoes the error message to the screen
C
C******************************************************************************
      subroutine ffoldout(ounit,outfile,outextn,vkywds,nkywds,outexist,
     &     tform,nprevlines,fstat)
      
      integer maxin, maxky
      parameter(maxin = 999, maxky = 90)
      character*(*) vkywds(maxky),tform(maxky)
      character(8) scratch
      character*(*) outfile
      character(80) context
      character(160) message
      integer fstat,ounit,nkywds,block,rwmode,start,fcstln
      integer hdutype,nfound,outextn,nprevlines,i,one,inull,inull2
      logical outexist
      
      one = 1
      inull = -999
      block = 1
      rwmode = 1
      
      
      call ftopen(ounit,outfile,rwmode,block,fstat)   

C   Fstat = 103 means no such file, so pass out of here
C   When linking to CFITSIO, though, Fstat=104 whenever any error
C   occurs when opening a file.
      
      if(fstat.eq.103.or.fstat.eq.104) then
         fstat = 0
         outexist = .false.
         nprevlines = 0
         goto 999
      else if (fstat.ne.0) then
         context = 'Error opening '//outfile
         call fcerr(context)
         call fcerrm(fstat)
         goto 999
      endif
      outexist = .true.
      
 900  call ftmahd(ounit,outextn+1,hdutype,fstat)
      if (fstat.ne.0) then
         write(message,50) outextn,outfile
 50      format('Could not move to extension ',i2,' in ',a) 
         call fcerr(message(1:fcstln(message)))
         call fcerrm(fstat)
         goto 999
      endif
      if(hdutype.ne.2) then
         context = 'Only binary type supported for outfile'
         call fcerr(context)
         goto 999
      endif
      outexist = .true.



C   Get the number of lines already in the file

      call ftgkyj(ounit,'NAXIS2',nprevlines,context,fstat)
      if(fstat.ne.0) then
         context = 'Could not get NAXIS2 keyword from'//outfile
         call fcerr(context)
         goto 999
      endif
      
C   The first two columns are the infile name and exten. no.  
C   Start with the third, and, if they are the
C   same as the outfile column names, get the keywords. 
      
      if(nkywds.eq.0) then
         
C   First get the number of columns in outfile
         
         call ftgkyj(ounit,'TFIELDS',nkywds,context,fstat)
         if(fstat.ne.0) then
            context = 'Could not get TFIELDS keyword from'//outfile
            call fcerr(context)
            goto 999
         endif
         start = 3
         call ftgkns(ounit,'TTYPE',start,nkywds,vkywds,nfound,fstat)
         if(fstat.ne.0) then
            context = 'Error reading TTYPE keywords from '//outfile
            call fcerr(context)
            goto 999
         endif
         
C   The first two columns are the infile name and extension columns

         nkywds = nkywds-2
      endif
      
      start = 1
      call ftgkns(ounit,'TFORM',start,nkywds+2,tform,nfound,fstat)
      if(fstat.ne.0) then
         context = 'Error reading TFORM keywords from '//outfile
         call fcerr(context)
         goto 999
      endif

C   Now write in TNULL keywords if not already present

      do 310 i=3,nkywds+2
         inull2 = inull
         if(index(tform(i),'I').ne.0.or
     &        .index(tform(i),'J').ne.0) then
            call ftgknj(ounit,'TNULL',i,one,inull2,nfound,fstat)
            if(fstat.eq.202.or.nfound.eq.0) then
               fstat = 0
               call ftpknj(ounit,'TNULL',i,one,inull,'&',fstat)
               if(fstat.ne.0)then
                  context = 'Error writing TNULL keyword'
                  call fcerr(context)
                  call fcerrm(fstat)
                  goto 999
               endif
            else if(fstat.ne.0) then
               context = 'Error getting TNULL keyword'
               call fcerr(context)
               call fcerrm(fstat)
               goto 999
            else if (inull.ne.inull2) then
               call ftkeyn('TNULL',i,scratch,fstat)
               call ftmkyj(ounit,scratch,inull,'&',fstat)
               if(fstat.ne.0) then
                  context = 'Error modifying TNULL keyword'
                  call fcerr(context)
                  call fcerrm(fstat)
                  goto 999
               endif
            endif
         endif
 310  continue  
 999  return
      end 
      
C*************************************************************************
C SUBROUTINE:
C       FFCHK   
C
C FILE: 
C       ffilecat        
C
C DESCRIPTION:
C       This routine checks that all the values in svalues are of the type
C       given in tform.  If a variable is of the wrong type, the row it is
C       in is eliminated, and the user is flagged.
C         
C
C AUTHOR/DATE:
C       Jim Ingham 1/5/92
C
C MODIFICATION HISTORY:
C
C NOTES:
C       
C USAGE:
C       call ffchk(vinfile,ninfiles,vkywds,nkywds,
C    &                                  svalues,tform,omit,fstat)
C       
C ARGUMENTS:
C       vinfile - The vector containing the input file names
C       ninfiles        - The number of input files
C       nkywds  - the number of keywords to be written out
C       vkywds  - the vector containing the keywords to be copied
C       svalues - The string array containing the keyword values
C       tform           - The output tform keyword for the keywords
C       omit            - Boolean- T if bad input is omitted...
C       fstat           - the error flag
C
C PRIMARY LOCAL VARIABLES:
C       valtype - I=>integer, L=>logical, C=>character, F=>Float        
C       dattyp  - Variable type from Tform kywd, output by ftbnfm
C       width           - Width of a character column, from its Tform kywd
C       
C CALLED ROUTINES:
C       ft*             - Fitsio routines       
C       fecho           - Echoes to the screen
C       ffomit  - Removes a line from the input array
C       fsctln  - Returns the length of a string sans trailing spaces
C
C******************************************************************************
      subroutine ffchk(vinfile,ninfiles,vkywds,nkywds,
     &     svalues,tform,omit,fstat)

      integer maxin, maxky
      parameter(maxin = 999, maxky = 90)
      character*(*) svalues(maxky,maxin)
      character(80) context
      character*(*) vinfile(maxin)
      character*(*) vkywds(maxky),tform(maxky)
      character(1) valtype
      integer ninfiles,nkywds,fstat,i,j,repeat,width,dattyp,fcstln,inull
      integer len1
      logical omit
      
      inull = -999

      do 110 i=1,nkywds
         call ftbnfm(tform(i+2),dattyp,repeat,width,fstat)
         if(fstat.ne.0) then
            context = 'Could not parse '//vkywds(i)//' tform keyword'
            call fcerr(context)
            goto 999
         endif
         j = 0
 120     j = j + 1
         if(j.gt.ninfiles) goto 110 
         if(svalues(i,j).eq.'NOT_FOUND') then
            if(omit) then
               call ffomit(vinfile,ninfiles,nkywds,svalues,j)
               goto 120
            else
               goto 120
            endif
         endif
         if(dattyp.eq.16) then
C   First strip off those annoying 's
            len1 = fcstln(svalues(i,j))
            if(svalues(i,j)(1:1).eq.'''') then
               svalues(i,j) = svalues(i,j)(2:len1)
               if(svalues(i,j)(len1-1:len1-1).eq.'''') then
                  svalues(i,j)(len1-1:len1-1) = ' '
               endif
            endif
            if(fcstln(svalues(i,j)).gt.width) then
               context = 'Keyword value for '//vkywds(i)//
     &              ' too long for '//vinfile(j)(1:30)
               call fcecho(context)
               if(omit) then
                  call ffomit(vinfile,ninfiles,nkywds,svalues,j)
                  goto 120
               else
                  svalues(i,j)=svalues(i,j)(1:width)
               endif
            endif
         else if (dattyp.eq.21.or.dattyp.eq.41) then
            call ftdtyp(svalues(i,j),valtype,fstat)
            if(valtype.ne.'I') then
               context = 'Wrong variable type for '//vkywds(i)//
     &              ' from '//vinfile(j)(1:30)
               call fcecho(context)        
               if(omit) then
                  call ffomit(vinfile,ninfiles,nkywds,svalues,j)
                  goto 120
               else
                  svalues(i,j) = 'NOT_FOUND'
               endif
            endif
         else if (dattyp.eq.42.or.dattyp.eq.82) then
            call ftdtyp(svalues(i,j),valtype,fstat)
            if(valtype.ne.'F'.and.valtype.ne.'I') then
               context = 'Wrong variable type for '//vkywds(i)//
     &              ' from '//vinfile(j)(1:30)
               call fcecho(context)        
               if(omit) then
                  call ffomit(vinfile,ninfiles,nkywds,svalues,j)
                  goto 120
               else
                  svalues(i,j) = 'NOT_FOUND'
               endif
            endif
         else if (dattyp.eq.14) then
            call ftdtyp(svalues(i,j),valtype,fstat)
            if(valtype.ne.'L') then
               context = 'Wrong variable type for '//vkywds(i)//
     &              ' from '//vinfile(j)(1:30)
               call fcecho(context)        
               if(omit) then
                  call ffomit(vinfile,ninfiles,nkywds,svalues,j)
                  goto 120
               else
                  svalues(i,j) = 'NOT_FOUND'
               endif
            endif
         endif
         goto 120
 110  continue
      
 999  return
      end
      
      
C*************************************************************************
C SUBROUTINE:
C       FFOMIT
C
C FILE: 
C       ffilecat.f      
C
C DESCRIPTION:
C       This routine eliminates the j'th row from svalues and vinfile,  
C       and decrements j and ninfiles by one
C
C AUTHOR/DATE:
C       Jim Ingham 
C
C MODIFICATION HISTORY:
C
C NOTES:
C       
C USAGE:
C       call ffomit(vinfile,ninfiles,nkywds,svalues,j)
C       
C ARGUMENTS:
C       vinfile - The vector containing the input file names
C       ninfiles        - The number of input files
C       nkywds  - the number of keywords to be written out
C       svalues - The string array containing the keyword values
C       j               - the row to omit       
C
C PRIMARY LOCAL VARIABLES:
C       
C CALLED ROUTINES:
C       
C******************************************************************************
      subroutine ffomit(vinfile,ninfiles,nkywds,svalues,j)
      
      integer maxin, maxky
      parameter(maxin = 999, maxky = 90)
      character*(*) svalues(maxky,maxin)
      character*(*) vinfile(maxin)
      integer nkywds,ninfiles,j,k,l
      
      do 100 k=j+1,ninfiles
         vinfile(k-1) = vinfile(k)
         do 120 l=1,nkywds
            svalues(l,k-1) = svalues(l,k)
 120     continue
 100  continue
      j = j-1
      ninfiles = ninfiles-1
      
      return
      end

C*************************************************************************
C SUBROUTINE:
C       FFINITJCI  
C
C FILE: 
C       ffilecat.f      
C
C DESCRIPTION:
C       This routine initializes the outfile, using svalues to create the
C       tform keywords.
C
C AUTHOR/DATE:
C       Jim Ingham 
C
C MODIFICATION HISTORY:
C       Ron Zellar -- Added the aform parameter
C NOTES:
C       
C       
C
C USAGE:
C       call ffinitjci(outfile,outextn,ounit,tform,ninfiles,vinfile,
C     &            vkywds,nkywds,svalues,maxlen,minlen,aform,eform,
C     &            iform,omit,quiet,fstat)
C       
C ARGUMENTS:
C       outextn - the output file extension number
C       outfile - the output filename
C       ounit           - The unit number for outfile
C       tform           - The output tform keyword for the keywords
C       ninfiles        - The number of input files
C       nkywds  - the number of keywords to be written out
C       vkywds  - the vector containing the keywords to be copied
C       svalues - The string array containing the keyword values
C       maxlen  - The maximum length for character strings
C       omit            - Boolean- T if bad input is omitted...
C       fstat           - the error flag
C       
C
C PRIMARY LOCAL VARIABLES:
C       dtype*  - datatype, format: see ftdtyp in Fitsio
C         *             - Most others are required values for Fitsio routines 
C CALLED ROUTINES:
C       ft*             - Fitsio routines
C       fcecho  - Echoes to the screen
C       fcerrm  - Echoes Fitsio error messages to the screen
C       
C******************************************************************************
      subroutine ffinitjci(outfile,outextn,ounit,tform,ninfiles,vinfile,
     &     vkywds,nkywds,svalues,maxlen,minlen,aform,afval,eform,
     &     iform,omit,quiet,fstat)

      integer maxin, maxky
      parameter(maxin = 999, maxky = 90)
      character*(*) vkywds(maxky)
      character(8)   scratch, extname
      character(1) dtype,dtype2
      character*(*) vinfile(maxin)
      character*(*) svalues(maxky,maxin), outfile
      character(80) context
      character(25) tunit(maxky)
      character*(*) tform(maxky)
      character(16) ttype(maxky)
      character*(*) aform,eform,iform
      integer fstat,ounit,outextn,nkywds,i,j,length,fcstln,ninfiles
      integer block,varidat,bitpix,naxis,inull,decimals,maxlen,one
      integer minlen,pcount,gcount,naxes,len1,afval
      logical simple,extend,first,omit,quiet
      
      block = 1
      varidat = 0
      inull = -999
      decimals = 2
      one = 1
      
C   Construct the TTYPE keywords

      ttype(1) = 'FILENAME'
      ttype(2) = 'FILEXTEN'
      do 98 i=1,nkywds
         ttype(i+2) = vkywds(i)
 98   continue
      
C   Construct the Tform keywords
C       First, the filename column:

      length = 0
      do 800 i=1,ninfiles
         length = max(length,fcstln(vinfile(i)))
 800  continue
      write(scratch,50) length
      tform(1) = scratch
 50   format(i4,'A')
      tform(2) = '1J'

C       Now the rest

      do 100 i=1,nkywds

C   Check the string value for its type, and if none were found,
C   set it to a string type
         first = .true.
         j = 0
 110     j = j + 1
         if(j.gt.ninfiles) goto 112 
         if(svalues(i,j).eq.'NOT_FOUND') then
            if(omit) then
               call ffomit(vinfile,ninfiles,nkywds,svalues,j)
               goto 110
            else if (first) then
               dtype = 'C'
               goto 110
            else
               goto 110
            endif
         endif
         if(first) then
            call ftdtyp(svalues(i,j),dtype,fstat)
            if(fstat.ne.0) then
               context = 'Error parsing the keyword'//svalues(i,1)
               call fcerr(context)
               call fcerrm(fstat)
               goto 999
            endif
            first = .false.
            goto 110         
         endif
         call ftdtyp(svalues(i,j),dtype2,fstat)
         
C   If two keyword values are of different types,  
C   write the whole column as a text type, unless a real value appears
C   in a heretofore integer column

         if(dtype.ne.dtype2) then
            if(dtype.eq.'I'.and.dtype2.eq.'F') then
               dtype = 'F'
            else if(dtype.eq.'F'.and.dtype2.eq.'I') then
               continue
            else
               write(context,27)
     &         vinfile(j)(:MIN(fcstln(vinfile(j)),len(context)-40)),
     &              vkywds(i)(1:8)
 27            format('Bad keyword value for ',a,' in file: ',a)
               dtype = 'C'
               goto 112
            endif
         endif
         goto 110
         
 112     if(dtype.eq.'C') then
C Determine the width of the columns:
            length = maxlen
            do 120 j=1,ninfiles
               
C   First strip off those annoying 's
               len1 = fcstln(svalues(i,j))
               if(svalues(i,j)(1:1).eq.'''') then
                  svalues(i,j) = svalues(i,j)(2:len1)
                  len1 = len1-1
                  if(svalues(i,j)(len1:len1).eq.'''') then
                     svalues(i,j)(len1:len1) = ' '
                     len1 = len1-1
                  endif
               endif
               length = max(length,len1)
 120        continue
            if(minlen.gt.0) length = minlen
            write(scratch,50) length

            if (afval .gt. length) then
               if (.not.quiet) then
                  context='Warning: aform value too big:'//aform
                  call fcecho(context)
                  call fcecho('...Resetting the aform value')
               endif
               write(aform,'(i12)')length
               aform = 'A'//aform(:fcstln(aform))
            endif
            
            tform(i+2) = scratch
         else if(dtype.eq.'L') then
            tform(i+2) = '1L'
         else if(dtype.eq.'I') then
            tform(i+2) = '1J'
         else if (dtype.eq.'F') then
            tform(i+2) = '1E'
         endif
 100  continue
      
C   Now create the output file

      call ftinit(ounit,outfile,block,fstat)
      if(fstat.ne.0) then
         context = 'Error opening '//outfile
         call fcerr(context)
         call fcerrm(fstat)
         goto 999
      endif
      
C   construct simple primary header for OUTPUT file
      simple = .true.
      naxis = 0
      naxes = 0
      pcount = 0
      gcount = 1
      bitpix = 8
      extend = .true.
      
C   Write the keywords

      call ftphpr(ounit,simple,bitpix,naxis,naxes,pcount,
     &     gcount,extend,fstat)
      if(fstat.ne.0) then
         context = 'Could not write primary header kywds in ' //
     &        outfile
         call fcerr(context)
         goto 999
      endif

C   Define the array
      
      call ftpdef(ounit,bitpix,naxis,naxes,pcount,gcount,fstat)

C   create a new extension in the output FITS file

      call ftcrhd(ounit,fstat) 
      if(fstat.ne.0) then
         context = 'Could not create extension in '//outfile
         call fcerr(context)
         goto 999
      endif
      
C   Put the keywords

      extname = ' '
      do 150 i=1,nkywds+2
         tunit(i) = ' '
 150  continue

      call ftphbn(ounit,ninfiles,nkywds+2,ttype,tform,tunit,
     &     extname, varidat,fstat)
      if(fstat.ne.0) then
         context = 'Could not write extension header keywords'
     &        // ' in ' // outfile
         call fcerr(context)
         call fcerrm(fstat)
         goto 999
      endif
C   Put in null keywords for the integer and real columns

      do 210 i=3,nkywds+2
         if(index(tform(i),'J').ne.0.or.
     &        index(tform(i),'I').ne.0) then
            call ftkeyn('TNULL',i,scratch,fstat)
            write(context,79) i
 79         format('Null value for Integer column ',i2)
            call ftpkyj(ounit,scratch,inull,context,fstat)
            if( iform.ne.'NONE') then
               call ftkeyn('TDISP',i,scratch,fstat)
               context = 'Output format'
               call ftpkys(ounit,scratch,iform,context,fstat)
            endif
         else if (index(tform(i),'E') .ne. 0 .and.
     &           eform.ne.'NONE') then
            call ftkeyn('TDISP',i,scratch,fstat)
            context = 'Output format'
            call ftpkys(ounit,scratch,eform,context,fstat)
         else if (index(tform(i),'A') .ne. 0 .and.
     &           aform.ne.'NONE') then
            call ftkeyn('TDISP',i,scratch,fstat)
            context = 'Output format'
            call ftpkys(ounit,scratch,aform,context,fstat)
         endif
 210  continue

C   Put in a tdisp keyword to display the extension column nicely
      call ftpkys(ounit,'TDISP2','I3',' ',fstat)      
C   Define the table

      call ftpdat(ounit,fstat)
      call ftbdef(ounit,nkywds+2,tform,varidat,ninfiles,fstat)
      if(fstat.ne.0) then
         context = 'Could not define binary table in '//outfile
         call fcerr(context)
         goto 999
      endif
 999  return
      end             
      
      
C*************************************************************************
C SUBROUTINE:
C       FFTRANS 
C
C FILE: 
C       ffilecat        
C
C DESCRIPTION:
C       This routine transfers the keyword values in svalues into the
C       outfile, reformatting as dictated by the tform keywords, if     
C       outexist is true.
C
C AUTHOR/DATE:
C       Jim Ingham 
C
C MODIFICATION HISTORY:
C
C NOTES:
C       
C       
C
C USAGE:
C       call fftrans(ounit,vkywds,nkywds,tform,outexist,nprevlines,
C     &                         vinfile,vexten,ninfiles,svalues,fstat)
C       
C ARGUMENTS:
C       ounit           - The unit number for outfile
C       nkywds  - the number of keywords to be written out
C       vkywds  - the vector containing the keywords to be copied
C       tform           - The output tform keyword for the keywords
C       outexist        - Boolean- T if an output file exists...
C       nprevlines      - Number of lines in the (existing) outfile extension
C       vinfile - The vector containing the input file names
C       vexten  - The vector of input file extension numbers
C       ninfiles        - The number of input files
C       svalues - The string array containing the keyword values
C       fstat           - the error flag
C
C PRIMARY LOCAL VARIABLES:
C       colnum  - The column number currently being written to
C       pointer - The row number currently being written to
C CALLED ROUTINES:
C       ft*             - Fitsio routines
C       fcecho  - Echoes to the screen
C       fcerrm  - Echoes Fitsio error messages to the screen
C       
C******************************************************************************
      subroutine fftrans(ounit,vkywds,nkywds,tform,outexist,nprevlines,
     &     vinfile,vexten,ninfiles,svalues,fstat)

      integer maxin, maxky
      parameter(maxin = 999, maxky = 90)
      character*(*) vkywds(maxky)
      character*(*) svalues(maxky,maxin),vinfile(maxin)
      character(80) context,stemp(maxin)
      character*(*) tform(maxky)
      real etemp
      integer fstat,ounit,nprevlines,felem,i,j,colnum,itemp(maxin)
      integer nkywds,ninfiles,inull,one,pointer,vexten(maxin)
      logical outexist,ltemp
      
      felem = 1
      inull = -999
      one = 1
      
C   First write out the filename and extension columns

      colnum = 1
      call ftpcls(ounit,colnum,nprevlines+1,felem,
     &     ninfiles,vinfile,fstat)
      colnum = colnum+1
      call ftpclj(ounit,colnum,nprevlines+1,felem,
     &     ninfiles,vexten,fstat)
      

C   Now transfer the keyword values

      do 110 i=1,nkywds
         colnum = colnum + 1
         if(index(tform(i+2),'A').ne.0) then
            do 120 j=1,ninfiles
               stemp(j) = svalues(i,j)
 120        continue
            call ftpcls(ounit,colnum,nprevlines+1,felem,ninfiles,
     &           stemp,fstat)
            if(fstat.ne.0) then
               context = 'Error writing keyword values for '
     &              // vkywds(i)
               call fcerr(context)
               call fcerrm(fstat)
               goto 999
            endif
         else if(index(tform(i+2),'J').ne.0
     &           .or.index(tform(i+2),'I').ne.0) then
            do 130 j=1,ninfiles
               if(svalues(i,j).eq.'NOT_FOUND') then
                  itemp(j) = -999
               else
                  call ftc2ii(svalues(i,j),itemp(j),fstat)
                  if(fstat.ne.0) then
                     context = 'Error parsing value for '//vkywds(i)
                     call fcerr(context)
                     goto 999
                  endif
               endif
 130        continue
            call ftpclj(ounit,colnum,nprevlines+1,felem,ninfiles,
     &           itemp,fstat)              
            if(fstat.ne.0) then
               context = 'Error writing keyword values for '
     &              //vkywds(i)
               call fcerr(context)
               call fcerrm(fstat)
               goto 999
            endif
         else if(index(tform(i+2),'E').ne.0
     &           .or.index(tform(i+2),'D').ne.0) then
            pointer = nprevlines
            do 140 j=1,ninfiles
               pointer = pointer + 1
               if(svalues(i,j).eq.'NOT_FOUND') then
                  call ftpclu(ounit,colnum,pointer,felem,one,fstat)
                  if(fstat.ne.0) then
                     context = 'Error putting undefined value in'
     &                    // vkywds(i)
                     call fcerr(context)
                     goto 999
                  endif
               else
                  call ftc2rr(svalues(i,j),etemp,fstat)
                  if(fstat.ne.0) then
                     context = 'Error parsing value for'//vkywds(i)
                     call fcerr(context)
                     goto 999
                  endif
                  call ftpcle(ounit,colnum,pointer,felem,one,
     &                 etemp,fstat)              
                  if(fstat.ne.0) then
                     context = 'Error writing keyword values for '
     &                    // vkywds(i)
                     call fcerr(context)
                     call fcerrm(fstat)
                     goto 999
                  endif
               endif
 140        continue
         else if(index(tform(i+2),'L').ne.0) then
            pointer = nprevlines
            do 145 j=1,ninfiles
               pointer = pointer + 1
               if(svalues(i,j).eq.'NOT_FOUND') then
                  call ftpclu(ounit,colnum,pointer,felem,one,fstat)
                  if(fstat.ne.0) then
                     context = 'Error putting undefined value in'
     &                    // vkywds(i)
                     call fcerr(context)
                     goto 999
                  endif
               else
                  call ftc2ll(svalues(i,j),ltemp,fstat)
                  if(fstat.ne.0) then
                     context = 'Error parsing value for'//vkywds(i)
                     call fcerr(context)
                     goto 999
                  endif
                  call ftpcll(ounit,colnum,pointer,felem,one,
     &                 ltemp,fstat)              
                  if(fstat.ne.0) then
                     context = 'Error writing keyword values for '
     &                    // vkywds(i)
                     call fcerr(context)
                     call fcerrm(fstat)
                     goto 999
                  endif
               endif
 145        continue
         endif
 110  continue
      
C   Now fix the NAXIS2 keyword
      
      call ftmkyj(ounit,'NAXIS2',nprevlines+ninfiles,'&',fstat)
      if(fstat.ne.0) then
         context = 'Error modifying NAXIS2 keyword'
         call fcerr(context)
         call fcerrm(fstat)
         goto 999
      endif
      
      call ftclos(ounit,fstat)        
      if(fstat.ne.0) then
         context = 'Error closing output file'
         call fcerr(context)
         call fcerrm(fstat)
         goto 999
      endif
            
 999  return
      end
              
        
