C******************************************************************************
C FTOOLS TASK:
C      fparkey
C
C FILE:
C      fparkey.f 
C
C DESCRIPTION: 
C      Write an IRAF parameter to a FITS header keyword  
C
C AUTHOR:  
C      William Pence  6/25/92
C
C MODIFICATION HISTORY:
C       6/11/93 EAG  Add check for keyword length
C       9/1/94 EAG 3.0a Clear FITSIO error stack
C
C -----------------------------------------------------------------
C  Banashree M Seifert (Feb 24, 1997)v 4.0
C  Modifications made as suggested by Bill pence "so that it can be used
C  to modify the value of a defective keyword (e.g., a string keyword
C  value that is missing the closing quote character)."
C --------------------------------------------------------------------
C       5/21/98 NG 4.1 - Replace ftmrhd(...,extnum,...) with ftmahd(..,extnum+1,..) 
C      02/01/1999 toliver 4.2 keyword max length now 70 characters
C      3/15/1999 peachey allow / to be used in a keyword value
C     10/25/1999 pwilson 4.3 add 'insert' parameter for inserting new keys at
C                            a specified location
C
C NOTES:
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C      subroutine gparky - gets parameters from environment
C      fitsio library - calls to subroutines beginning with 'ft....'
C      function fcstln - find index of last non-blank character
C
C******************************************************************************
      subroutine fparky

      integer funit
      parameter (funit=37)
      integer status,blksiz,extnum,hdutyp,hdtype
      integer i,vbegin,vend,inskey,minkey
      character(160) outfil, output
      character(80) value,coment,card,oldcom,context
      character keywrd*70,tmplat*230,oldval*20
      character(80) card2,insert
      logical add,quotit

      integer fcstln

      character(40) taskname
      common /task/ taskname

      taskname = 'fparkey4.3'
      call ftcmsg
      status=0

C       get all the input parameters
      call gparky(output,keywrd,value,coment,add,insert,status)
      if (status .gt. 0)go to 999

C check that the keyword is 70 letters or less
C allow for possible "-" in front of keyword
      i = 1
      if (keywrd(1:1) .eq. '-') i = 2
      if (fcstln(keywrd(i:)) .gt. 70) then
         context = ' Specified keyword length > 70 characters '
         call fcerr (context)
         goto 999
      endif

C       parse the output string to get the file name and extension number
      call fcpars(output,outfil,extnum,status)

C EAG 8/25/93 default to 1st extension
      if (extnum .eq. -99) extnum = 1

      if ((status .gt. 0) .or. (extnum .lt. 0)) then
         context='unable to parse the filename[extension]'
         call fcerr(context)
         go to 999
      end if

C       open the new FITS file with write access
      call ftopen(funit,outfil,1,blksiz,status)
      if (status .gt. 0)then
         context='unable to open the FITS file ' // outfil
         call fcerr(context)
         go to 997
      end if

C       move to the correct extension
      call ftmahd(funit,extnum+1,hdutyp,status)
      if (status .gt. 0)then
         context='unable to move to specified extension'
         call fcerr(context)
         go to 997
      end if

C       test if this is a boolean keyword value
      oldval=value
      call ftupch(oldval)
      if (oldval .eq. 'Y' .or. oldval .eq. 'YES' .or.
     &     oldval .eq. 'T' .or. oldval .eq. 'TRUE')then
         value='T'
      else if(oldval .eq. 'N' .or. oldval .eq. 'NO' .or.
     &        oldval .eq. 'F' .or. oldval .eq. 'FALSE')then
         value='F'
      end if

C       find the end of the value string
      vend=1
      vbegin=1
      do 10 i=80,1,-1
         if (value(i:i) .ne. ' ')then
            vend=i
            go to 20
         end if
 10   continue
 20   continue

C       find the beginning of the value string
      do 30 i=1,vend
         if (value(i:i) .ne. ' ')then
            vbegin=i
            go to 40
         end if
 30   continue

 40   quotit=.false.
C       if string begins with a quote, then don't have to worry about spaces
      if (value(vbegin:vbegin) .eq. '''')go to 60

C       check if the value string has embedded spaces; if so have to add quotes
C       jp 3/15/1999: Add quotes also if value contains /. This is
C                     so that ftgthd does not interpret the / as
C                     the start of a comment.
      do 50 i=vbegin,vend
         if (value(i:i) .eq. ' ' .or. value(i:i) .eq. '/')then
            quotit=.true.
            go to 60
         end if
 50   continue
 60   continue

C       concatinate the strings together into the header template
      if (quotit)then
C               add quotes around the value string
         tmplat=keywrd//' '''//value(1:vend)//''' '//coment
      else
         tmplat=keywrd//' '//value(1:vend)//' '//coment
      end if

C       reformat the template into a legal FITS keyword record
      call ftgthd(tmplat,card,hdtype,status)
      if (status .gt. 0)then
         context='unable to parse keyword template'
         call fcerr(context)
         go to 997
      end if

C       test what type of keyword this is 
      if (hdtype .eq. 1)then
C               this is a comment keyword so just append it
         if (add) then

C        Check if we need to insert it rather than append it
            inskey = 0
            if ( insert.ne.' ' .and. insert.ne.'0' ) then
               call ftgcrd(funit,insert,context,status)
               if( status.le.0 ) then
                  call ftghps(funit,i,inskey,status)
               else
                  inskey = 0
                  read(insert,100) inskey
 100              format(I6)
               endif
               status = 0
            endif

C           Prevent header corruption
            if ( inskey.le.0 ) then
               call ftprec(funit,card,status)
            else
               call fndminpos(funit,extnum,minpos,status)
               if( status.gt.0 ) then
                  context = 'Error locating required keys in header'
                  call fcerr(context)
                  goto 998
               else if( inskey.lt.minpos ) then
                  status = 208
                  context = 'Inserting key will invalidate the file'
                  call fcerr(context)
                  goto 998
               else
                  call ftirec(funit,inskey,card,status)
               endif
            endif
         else
            context = 'No permission to write new record'
            call fcerr (context)
            goto 998
         endif
      else if (hdtype .eq. 0)then
C               modify the keyword if it already exists

ccc these few lines below has been changed in v4.0
ccc ---------------------------------------------------------------
ccc         call ftgkey(funit,card(1:8),oldval,oldcom,status)
ccc         if (status .le. 0)then
cccC                       keyword exists; modify it
ccc            call ftmcrd(funit,card(1:8),card,status)
cccC                       if the new comment string was blank, then
cccC                       replace it with the old comment string
ccc            if (coment .eq. ' ')then
ccc               call ftgkey(funit,card(1:8),oldval,coment,status)
ccc               call ftmcom(funit,card(1:8),oldcom,status)
ccc            end if
ccc ---------------------------------------------------------------
ccc to these lines below
ccc so that it can be used to modify the value of a defective keyword

         call ftgcrd(funit,keywrd,card2,status)
         if (status .le. 0)then
C                       keyword exists; modify it
            call ftmcrd(funit,keywrd,card,status)
C                       if the new comment string was blank, then
C                       replace it with the old comment string
            if (coment .eq. ' ')then
C              parse the old value and comment fields
               call ftpsvc(card2,oldval,oldcom,status)
               if (status .le. 0)then
C                 write the old comment field
                  call ftmcom(funit,keywrd,oldcom,status)
               else
C                 ignore error parsing the value of the old keyword
                  status = 0
               end if
            end if

         else if (status .eq. 202) then
C                       keyword not found; can we add a new one?
            status=0
            call ftcmsg
            if (add) then

C           Check if we need to insert it rather than append it
               inskey = 0
               if ( insert.ne.' ' .and. insert.ne.'0' ) then
                  call ftgcrd(funit,insert,context,status)
                  if( status.le.0 ) then
                     call ftghps(funit,i,inskey,status)
                  else
                     inskey = 0
                     read(insert,200) inskey
 200                 format(I6)
                  endif
                  status = 0
               endif

C              Prevent header corruption
               if ( inskey.le.0 ) then
                  call ftprec(funit,card,status)
               else
                  call fndminpos(funit,extnum,minpos,status)
                  if( status.gt.0 ) then
                     context = 'Error locating required keys in header'
                     call fcerr(context)
                     goto 998
                  else if( inskey.lt.minpos ) then
                     status = 208
                     context = 'Inserting key will invalidate the file'
                     call fcerr(context)
                     goto 998
                  else
                     call ftirec(funit,inskey,card,status)
                  endif
               endif
            else
C                           no permission to write new keyword
               context='No permission to create new keyword'
               call fcerr(context)
               go to 998
            end if
         else
C                       some other fitsio error
            context='Error reading FITS header'
            call fcerr(context)
         end if
      else if (hdtype .eq. 2)then
C               Tried to write the END keyword
         context = 'END keyword already exists'
         call fcerr(context)
         go to 998
      else if (hdtype .lt. 0)then
C              delete this keyword from the header, first, lose the leading '-'
         keywrd = keywrd(2:LEN(keywrd)-1) // ' '
         call ftdkey(funit,keywrd,status)
      end if
      if (status .le. 0)go to 998

 997  continue
C       print FITSIO error code and text string
      call fcerrm(status)
      status = 0

C       close the fits file
 998  call ftclos(funit,status)
 999  continue

      return
      end

C******************************************************************************
C SUBROUTINE:
C      gparky
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C AUTHOR:  
C      William Pence   6/26/92
C
C MODIFICATION HISTORY:
C      Peter Wilson 10/25/1999: Added 'insert' parameter
C
C NOTES:
C      gparky uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gparky(output,keywrd,value,coment,add,insert,status)
C
C ARGUMENTS:
C    output:
C      output   - name of the FITS file[extension]
C      keywrd   - name of the keyword
C      value    - value for the keyword
C      coment   - comment string for the keyword
C      add      - Is it OK to create a new keyword?
C      insert   - Where to insert new key
C      status   - returned error status (0 = OK)
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C      subroutine uclgst - get string parameter
C      subroutine uclgsb - get boolian parameter
C
C****************************************************************************** 
      subroutine gparky(output,keywrd,value,coment,add,insert,status)

      character*(*) output,keywrd,value,coment
      integer status
      logical add
      character(80) context,insert

C  get the value string
      call uclgst('value',value,status)
      if (status .ne. 0) then
         context = 'could not get value parameter'
         call fcerr(context)
         goto 999
      endif

C  get the name of the input FITS file
      call uclgst('fitsfile',output,status)
      if (status .ne. 0) then
         context = 'could not get fitsfile parameter'
         call fcerr(context)
         goto 999
      endif

C  get the name of the keyword
      call uclgst('keyword',keywrd,status)
      if (status .ne. 0) then
         context = 'could not get keyword parameter'
         call fcerr(context)
         goto 999
      endif

C  get the comment string
      call uclgst('comm',coment,status)
      if (status .ne. 0) then
         context = 'could not get comm parameter'
         call fcerr(context)
         goto 999
      endif

C  get the add record flag
      call uclgsb('add',add,status)
      if (status .ne. 0) then
         context = 'could not get add parameter'
         call fcerr(context)
         goto 999
      endif

C  get the insert record value
      call uclgst('insert',insert,status)
      if (status .ne. 0) then
         context = 'could not get insert parameter'
         call fcerr(context)
         goto 999
      endif

 999  continue
      end

C******************************************************************************
C SUBROUTINE:
C     fndminpos
C
C DESCRIPTION:
C     Locates earliest position a key can be inserted without corrupting
C     the FITS header
C
C******************************************************************************
      subroutine fndminpos(funit,ext,pos,status)
      IMPLICIT NONE
      integer funit,ext,pos
      integer status,type,i
      character(80) key,card

      if( status.gt.0 ) return

      pos    = 0
      if ( ext.eq. 0 ) then
         call ftgcrd(funit,'EXTEND',card,status)
         if( status.le.0 ) then
            call ftghps(funit,type,pos,status)
         else
            status = 0
            call ftgkyj(funit,'NAXIS',i,card,status)
            call ftghps(funit,type,pos,status)
            pos = pos + i
         endif
      else
         call ftghdt(funit,type,status)
         if ( type.eq.0 ) then
            key = 'GCOUNT'
         else
            key = 'TFIELDS'
         endif
         call ftgcrd(funit,key,card,status)
         if( status.le.0 ) then
            call ftghps(funit,type,pos,status)
         endif
      endif
      return
      end
