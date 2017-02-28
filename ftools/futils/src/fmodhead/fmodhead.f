C***************************************************************************** 
C SELECTOR TASK:
C      fmodhead
C
C FILE:
C      fmodhead.f 
C
C DESCRIPTION: 
C      Modifies a FITS file header according to instructions in a template file
C
C AUTHOR:  
C      Vidya Sagar. Jul '92
C
C MODIFICATION HISTORY:
C      cleaned up code and documentation - Sept 1992 - W Pence
C      8/29/94 EAG 3.0a - call faopen to open text file
C      9/22/94 EAG 3.0b - add more ftcmsg calls appropriately
C -----------------------------------------------------------------
C  Banashree M Seifert (Feb 24, 1997)v 4.0 
C  Modifications made as suggested by Bill pence "so that it can be used 
C  to modify the value of a defective keyword (e.g., a string keyword 
C  value that is missing the closing quote character)."
C
C     In fmodhead.f, delete line 185:
C
C        character(80) comment
C
C     and change line 265 from
C
C        call ftgkey(funit,kywrd,kyval,comment,ftstat)
C     to
C        call ftgcrd(funit,kywrd,kyval,ftstat)
C -------------------------------------------------------------------- 
C
C NOTES:
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infile    - input FITS file and extension number
C      tmpfil    - output histogram file
C CALLED ROUTINES:
C      subroutine gmodhd - gets parameters from environment
C      subroutine fmdfit - Modifies read FITS file.
C
C******************************************************************************
      subroutine fmodhd
C
      character * 160 infile 
      character * 160 tmpfil
      
      integer         ftstat
      character(40) taskname
      common /task/ taskname

      taskname = 'fmodhead4.0'

      call ftcmsg

      infile = ' '
      tmpfil = ' '
      ftstat = 0

C  get the parameters from the par file

      call gmodhd(infile,tmpfil,ftstat)
      if (ftstat .ne. 0) goto 999

C  modify the FITS file

      call fmdfit(infile,tmpfil,ftstat)

 999  continue
      if (ftstat .ne. 0) call fcerrm(ftstat)
      return
      end



C******************************************************************************
C SUBROUTINE:
C      gmodhd
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C AUTHOR:  
C
C MODIFICATION HISTORY:
C
C NOTES:
C      gmodhd uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gmodhd(infile,tmpfil,status)
C
C ARGUMENTS:
C      infile   - input FITS file and extension number
C      tmpfil   - output histogram file
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
      subroutine gmodhd(infile,tmpfil,ftstat)
C
      character(80) context
      character*(*) infile, tmpfil

      integer ftstat
C
C  initialize variables
C
      ftstat = 0
C
C  get the name of the input FITS file
C
      call uclgst('infile',infile,ftstat)
      if (ftstat .ne. 0) then
         context = 'could not get INFILE parameter'
         call fcerr(context)
         goto 999
      endif
      if (infile .eq. ' ')then
         call fcerr('Error: blank FITS file name.')
         ftstat=104
         goto 999
      end if

C  get the name of the TEMPLATE file
      call uclgst('tmpfil',tmpfil,ftstat)
      if (ftstat .ne. 0) then
         context = 'could not get TMPFIL parameter'
         call fcerr(context)
         goto 999
      endif
      if (tmpfil .eq. ' ')then
         call fcerr('Error: blank template file name.')
         ftstat=104
      end if


 999  continue

      return
      end

C******************************************************************************
C SUBROUTINE:
C      fmdfit
C
C DESCRIPTION: 
C      Modifies a FITS file header according to instructions in a template file
C
C AUTHOR:  
C      Vidya Sagar Jul '92 
C
C MODIFICATION HISTORY:
C      cleaned up code and documentation -  Sept 1992 - W Pence
C	added change of keyword name only 8/2/93 EAG
C
C NOTES:
C      fmdfit uses FITSIO calls to read FITS file
C
C USAGE:
C      call fmdfit(infile,tmpfil,ftstat)
C
C ARGUMENTS:
C      infile   - input FITS file and extension number
C      tmpfil   - input header template file
C
C PRIMARY LOCAL VARIABLES
C      infile     - name of FITS file
C      context    - error message 
C      errstr     - fitsio error message string
C      extnum     - FITS file extension number
C      ftstat     - FITSIO error number
C
C CALLED ROUTINES:
C      subroutine fcasfm - get ASCII format
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - report an error number to terminal
C      subroutine fcpars - parse off filename and extension number
C      subroutine ftclos - close a FITS file
C      subroutine ftmrhd - relative move to FITS header
C      subroutine ftopen - open a FITS file
C
C****************************************************************************** 
      subroutine fmdfit(filnam,tmpfil,ftstat)

      character*(*) filnam,tmpfil
      character(160)  infile
      character(80) context
ccc      character(80) comment
      
      integer ftstat, status
      integer funit
      integer tunit
      integer block
      integer extnum
      integer hdtype
      integer kylen

C Max size of keyword (incl. HIERARCH), equal to FLEN_KEYWORD (fitsio.h)
      character(72) kywrd
      character(80) card

      character(100) tmplte
      character(80)  errstr
      character(80)  kyval

C  initialize variables
      status = 0
      funit = 10
      tunit = 11

C  get the input FITS filename and extension number
      call fcpars(filnam,infile,extnum,ftstat)

C EAG 8/25/93 default to 1st extension
      if (extnum .eq. -99) extnum = 1

C  if extension is negative then give error and exit
      if (extnum .lt. 0) then
         context = 'illegal extension number'
         call fcerr(context)
         goto 999
      endif

C  open the input FITS file for read and write
      call ftopen(funit,infile,1,block,ftstat)
      if (ftstat .ne. 0) then
         context = 'unable to open infile'
         call fcerr(context)
         goto 999
      endif

C  move to the extension number
      call ftmrhd(funit,extnum,hdtype,ftstat)
      if (ftstat .ne. 0) then
         errstr = 'error moving to extension number '
         write(context,1002) errstr, extnum, ftstat
 1002    format(a35,i6,i6)
         call fcerr(context)
         goto 999
      endif

      call faopen (tunit, tmpfil, 1, 0, status)
      if (status .ne. 0) goto 1000
      
 10   continue

C --- Read each record from the input TEMPLATE file.

      read(tunit,2000,end=999,err=1001) tmplte
 2000 format(a)

C --- Skip comment or blank lines in template file
      if (tmplte(1:1) .eq. '#' .or. tmplte(1:1) .eq. ' ')goto 10

C --- Parse the template string to get back a formatted 80
C --- character length string.

      call ftgthd(tmplte,card,hdtype,ftstat)
      if (ftstat .gt. 0)go to 999

C --- Get keyword name (fits_get_keyname allows for HIERARCH keywords):

      call ftgknm(card,kywrd,kylen,ftstat)

      if (hdtype .eq. 0) then
C	      modify keyword if it exists, otherwise append new keyword

C             Check to see if keyword is present in the FITS header
ccc  replaced this line of call ftgkey by call ftgcrd
ccc         call ftgkey(funit,kywrd,kyval,comment,ftstat)

         call ftgcrd(funit,kywrd,kyval,ftstat)

         if (ftstat .eq. 202) then
C                 keyword doesn't exist, so append new keyword
            ftstat = 0
            call ftcmsg
            call ftprec(funit,card,ftstat)
         else
C                 modify existing keyword
            call ftmcrd(funit,kywrd,card,ftstat)
         endif
      else if (hdtype .eq. 1) then
C		  simple append new keyword (e.g. COMMENT or HISTORY)
         call ftprec(funit,card,ftstat)

      else if (hdtype .eq. -1) then
C		  delete the record with this keyword.
         call ftdkey(funit,kywrd,ftstat)
         if (ftstat .eq. 202)then
C		    issue warning message that keyword doesn't exist
            call fcecho(' Warning: could not delete '//kywrd)
            ftstat=0
            call ftcmsg
         end if

      else if (hdtype .eq. -2) then
C		change keyword name only
         call ftmnam (funit, card(1:8), card(41:48), ftstat)

      else
C	          we must have hit an END record, so quit
         goto 999
      endif

C	jump out of loop if error was encountered
      if (ftstat .gt. 0) go to 999

C	loop back for next template record
      go to 10

 1000 call fcerr(' Error opening template file')
      ftstat = 104
      go to 999

 1001 call fcerr(' Error reading template file')
      ftstat = 108

 999  continue	
      status = 0
      call ftclos(funit,status)
      close(tunit)

      return
      end
