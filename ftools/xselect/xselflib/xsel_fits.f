c
c
c --------------------------------------------------------
      subroutine XSL_CHKCOL(filenm,colnam,nnames,status)
c --------------------------------------------------------
c   Checks whether the columns colnam are in the file filenm
c   status is the number of missing columns.
c   Jim Ingham 2/19/93
c   Modified: Jeff Guerber, HSTX, Aug 1997. Replaced XSL_EXTSTRIP with FCPARS
c   (and trap FCPARS's flag values).
c
      integer MAXFIL
      parameter (MAXFIL = 50)
      character*(*) filenm,colnam(MAXFIL)
      character(255) name,colist(200)
      character(255) messag
      integer status,tmpstat,iunit,exten,rwmode,block
      integer len1,len2,len3
      integer LENACT,hdutyp,ncols,nfound,i,j,nnames

      rwmode = 0
      block = 1
      status = 0

      CALL FCPARS(filenm,name,exten,status)

c     Default to extension 1.  Error if "*".
      if (exten .eq. -99)  exten = 1
      if (exten .eq. -1)  then
          messag = 'CHKCOL: invalid extension number: '//filenm
          call XWRITE(messag,5)
          status = -2
      endif

      IF(status.ne.0)  return

      CALL GETLUN(iunit)
      call FTOPEN(iunit,name,rwmode,block,status)
      IF(status.eq.103) then
         len1 = LENACT(name)
         messag = 'CHKCOL: unable to find FITS file: '//name(1:len1)
         len3 = LENACT( messag )
         call XWRITE(messag(1:len3),5)
         return
      ELSE IF(status.ne.0) then
         len1 = LENACT(name)
         messag = 'CHKCOL: unable to open FITS file: '//name(1:len1)
         len3 = LENACT( messag )
         call XWRITE(messag(1:len3),5)
         tmpstat = 0
         call FTCLOS(iunit,tmpstat)
         return
      ENDIF
      CALL FTMAHD(iunit,exten+1,hdutyp,status)
      if(status.ne.0) then
         len1 = LENACT(filenm)
         messag = 'CHKCOL: Error moving to extension '//filenm(1:len1)
         len3 = LENACT( messag )
         call XWRITE(messag(1:len3),5)
         tmpstat = 0
         call FTCLOS(iunit,tmpstat)
         return
      endif

      CALL FTGKYJ(iunit,'NAXIS1',ncols,messag,status)
      CALL FTGKNS(iunit,'TTYPE',1,ncols,colist,nfound,status)
      do 100 i=1,nnames
         do 150 j=1,ncols
            if(colnam(i).eq.colist(j))then
               goto 100
            endif
 150     continue
         len1 = LENACT(colnam(i))
         messag = 'CHKCOL: Parameter '//colnam(i)(1:len1)
     &                                         //' not in file '
         len1 = LENACT(messag) + 1
         len2 = LENACT(name)
         messag = messag(1:len1)//name(1:len2)

         len3 = LENACT( messag )
         call XWRITE(messag(1:len3),5)

         status = status + 1
 100  continue

      CALL FTCLOS(iunit,status)
      CALL FRELUN(iunit)
      return
      end
c
c
c -------------------------------------------------------
      subroutine XSL_CHKTYPE(file,type,status)
c -------------------------------------------------------
c  This function tests the type of file, and returns
c         1 if it is a fits gti file
c         2 if it is an ascii gti file
c         3 if it is a xronos window file
c  Status = -10 if files is unreadable fits, or unknown type
c  Status = -20 for file not found
c
c This routine also checks the TIMESYS, for FITS files, and the first
c line of the file for ASCII, and if they are  in UT, it converts them
c to spacecraft seconds from the MJDREF.
c


c  J. Ingham, 7/93
      character*(*) file
      integer ilun,ilun2,status,rwmode,type,ierr
      integer headlen,nstrt,LENACT,colnum,nhdu
      character(80) inline
      character(16) headstr
      double precision dtimes(2), tlims(2)

      status = 0
      rwmode = 0
      ierr = 0
      call GETLUN(ilun)

c First test to see if it is a fits file:

      call FTNOPN(ilun,file,rwmode,status)
      IF(status.eq.0) THEN
         TYPE = 1

c If the HDU was not specified in the filename then move to the first GTI
c extension. Check for both 'GTI' and 'STDGTI' since both seem to be moved.

         CALL FTGHDN(ilun,nhdu)
         IF ( nhdu .EQ. 1 ) THEN
            CALL FTMNHD(ilun,2,'GTI',0,status)
            IF ( STATUS .NE. 0 ) THEN
               status = 0
               CALL FTMNHD(ilun,2,'STDGTI',0,status)
               IF ( status .NE. 0 ) THEN
                  inline = 'Error moving to GTI or STDGTI extension in '
     &                     //file(:LENACT(file))
                  call XWRITE(inline,5)
                  status = -10
                  RETURN
               ENDIF
            ENDIF
         ENDIF

c Find the column to read for times

         call FTGCNO(ilun,.FALSE.,'START',colnum,status)
         if ( status.ne.0 ) THEN
            status = 0
            call FTGCNO(ilun,.FALSE.,'STARTDAT',colnum,status)
            if ( status.ne.0) then
               inline = 'No START or STARTDAT column in file '//file
               call XWRITE(inline,5)
               status = -10
               return
            else
C Convert from UT to Mission Time:
               call XWRITE('Fits conversion not yet supported',5)
               status = -10
               return
            endif
         endif
         call FTCLOS(ilun,status)
         call FRELUN(ilun)
         return
c FITSIO error 103 is file not found
      ELSEIF(status.eq.103) THEN
         status = -20
         call FRELUN(ilun)
         return
      ELSE
         status = 0
         call FTCLOS(ilun,status)
      ENDIF
c Now check whether it is an ascii, or xwin file
      open(unit=ilun, file = file, status = 'OLD',
     &                              iostat = ierr,err=250)

      read(ilun,'(a80)',end = 260, err = 260 ) inline
      IF(index(inline,'Window').ne.0) THEN
         TYPE = 3
      ELSE
         nstrt = index(inline,'UT')
         if(nstrt.ne.0) then
            headstr = inline(nstrt+2:)
            headlen = LENACT(headstr)
            if (headlen.eq.0) then
               headlen = 1
            endif
            call XWRITE
     &           ('Found UT-time file, converting to  scc time',5)
C Here we have to convert the file:
C Allow a header giving the base of the string:

            file = file(:LENACT(file))//'_scc'
            call XSL_RMFILE(file)
            call GETLUN(ilun2)
            call XSL_OPEN(ilun2,file,'NEW',' ',' ',0,0,status)
            do while (.TRUE.)
               read(ilun,'(a80)', err=265 , end= 300) inline
               call str2sec(inline, .TRUE., .FALSE., headstr, tlims,
     &                      dtimes, 2, ntimes)
               if(status.eq.0 .AND. ntimes .EQ. 2 ) then
                  write(ilun2,*) dtimes(1), dtimes(2)
               else
                  goto 265
               endif
            enddo
 300        close(ilun2)
            call FRELUN(ilun2)
C This is if we got the UT header line, then we can exit this section
            goto 275
         endif

         nstrt = index(inline,'MJD')
         if(nstrt.ne.0) then
            call XWRITE
     &           ('Found MJD-time file, converting to  scc time',5)
C Here we have to convert the file:
C Allow a header giving the base of the string:

            file = file(:LENACT(file))//'_scc'
            call XSL_RMFILE(file)
            call GETLUN(ilun2)
            call XSL_OPEN(ilun2,file,'NEW',' ',' ',0,0,status)
            do while (.TRUE.)
               read(ilun,'(a80)', err=265 , end= 310)
     &              inline
               call str2sec(inline, .TRUE., .FALSE., headstr, tlims,
     &                      dtimes, 2, ntimes)
               if(status.eq.0 .AND. ntimes .EQ. 2 ) then
                  write(ilun2,*) dtimes(1), dtimes(2)
               else
                  goto 265
               endif
            enddo
 310        close(ilun2)
            call FRELUN(ilun2)
         endif
 275     TYPE = 2
      ENDIF
      goto 270
c This is the error for an empty file
 260  status = -10
      goto 270

c This is the error for bad UT conversion:
 265  call XWRITE('Error in UT or MJD->SCC conversion of file: ',5)
      call XWRITE(file,5)
      call XWRITE('The offending line was:',5)
      call XWRITE(inline,5)
      close(ilun2)
      call FRELUN(ilun2)
      call XSL_RMFILE(file)
      file = file(:LENACT(file)-4)
      goto 270
c This is the error for no file found
 250  status = -20

 270  close(ilun)
      call FRELUN(ilun)
      return

      end
c
c
c ---------------------------------------------
      subroutine XSL_COL2INT(incol,intval,nvals,ncol,MXCOL,
     &     filnam,catidx,catrows,MAXROWS,status)
c ---------------------------------------------
c This takes the values in a column of a FITS file, and makes the
c a matrix of integers, intval.
c JCI Mod 6/21/94
c Modified: Jeff Guerber, HSTX, Sep 1997. Replaced XSL_EXTSTRIP with FCPARS
c (and trap FCPARS's flag values).
c

      integer ncol,MXCOL,MAXROWS,status,SUBMAX
      parameter (SUBMAX = 10)
      character*(*) incol(MXCOL),filnam
      character(80) str1
      character(255) name
      character(512) context
      integer catidx(MAXROWS),catrows
      integer intval(MXCOL,MAXROWS),nvals(MXCOL)
      integer block,htype,rwmode,itemp,ilun,exten,tmpstat
      integer nrows,i,j,k,colno(SUBMAX),isentinal(SUBMAX)
      logical EXACT

      if(MXCOL.gt.SUBMAX) then
         call XWRITE('Error in XSL_COL2INT, array too large',5)
         status = -13
         return
      endif

      EXACT = .FALSE.

      rwmode = 1
      status = 0

c Parse the input name

      call FCPARS(filnam,name,exten,status)

c     Default to extension 1.  Error if "*".
      if (exten .eq. -99)  exten = 1
      if (exten .eq. -1)  then
          context = 'COL2INT: invalid extension number: '//filnam
          call XWRITE(context,5)
          status = -2
      endif

c     SHOULD THIS RETURN??
      IF(status.ne.0) THEN
         context = 'Error parsing file name'//filnam
         call XWRITE(context,5)
      endif

c Open the input file:
      call GETLUN(ilun)
      call FTOPEN(ilun, name, rwmode, block, status)
      if (status .eq. 103) then
         context='Unable to find FITS file '//name
         call XWRITE(context,5)
         call FRELUN(ilun)
         status = 0
         return
      else if (status .ne.0) then
         context='0, Unable to open FITS file '//name
         call XWRITE(context,5)
         tmpstat = 0
         call FTCLOS(ilun,tmpstat)
         call FRELUN(ilun)
         return
      endif
c go to first extension
      call FTMRHD(ilun, exten, htype, status)
      if(status .gt. 0) then
         write(context,95) exten,name
 95      format('Unable to move to ext# ',i3,' in file',a160)
         call XWRITE(context,5)
         tmpstat = 0
         call FTCLOS(ilun,tmpstat)
         call FRELUN(ilun)
         status=-10
         return
      endif

c Get the naxis and the column numbers, and initialize outkey
      call FTGKYJ(ilun,'NAXIS2',nrows,str1,status)
      do i=1,ncol
         call FTGCNO(ilun,EXACT,incol(i),colno(i),status)
         IF(status.ne.0) THEN
            colno(i) = -1
            context = 'Couldn''t find column '//incol(i)
            call XWRITE(context,5)
         ENDIF
      enddo
c Get the column values: initialize:
      if(catidx(1) .eq. -1 ) then
         do j=1,ncol
            call FTGCVJ(ilun,colno(j),1,1,1,0,intval(j,1),
     &           EXACT,status)
            nvals(j) = 1
            isentinal(j) = 2
         enddo
      else
         do j=1,ncol
            call FTGCVJ(ilun,colno(j),catidx(1),1,1,0,intval(j,1),
     &           EXACT,status)
            nvals(j) = 1
            isentinal(j) = 2
         enddo
      endif
c Now get the rest and compare:
c This uses a sentinal.  If there are more than MXCOL values, then
c both nvals and the list of values will be incorrect.
      if(catidx(1) .eq. -1) then
         do i=2,nrows
            do j=1,ncol
               call FTGCVJ(ilun,colno(j),i,1,1,0,itemp,
     &              EXACT,status)
               if(status.ne.0) then
                  str1 = 'Error getting column: '//incol(j)//
     &                 ' from file:'
                  CALL XWRITE(str1,5)
                  CALL XWRITE(filnam,5)
                  write(str1,'(a,I4)') 'FITSIO error no: ',status
                  CALL XWRITE(str1,5)
                  goto 999
               endif
               k = 1
               intval(j,isentinal(j)) = itemp
               do while ( itemp.ne.intval(j,k) )
                  k = k+1
               enddo
               if ( k .eq. isentinal(j) ) then
                  isentinal(j) = min(isentinal(j)+1,MXCOL)
                  nvals(j) = nvals(j) + 1
               endif
            enddo
         enddo
      else
         do i=2,catrows
            do j=1,ncol
               call FTGCVJ(ilun,colno(j),catidx(i),1,1,0,itemp,
     &              EXACT,status)
               if(status.ne.0) then
                  str1 = 'Error getting column: '//incol(j)//
     &                 ' from file:'
                  CALL XWRITE(str1,5)
                  CALL XWRITE(filnam,5)
                  write(str1,'(a,I4)') 'FITSIO error no: ',status
                  CALL XWRITE(str1,5)
                  goto 999
               endif
               intval(j,isentinal(j)) = itemp
               k = 1
               do while ( itemp.ne.intval(j,k) )
                  k = k+1
               enddo
               if ( k .eq. isentinal(j) ) then
                  isentinal(j) = min(isentinal(j)+1,MAXROWS)
                  nvals(j) = nvals(j) + 1
               endif
            enddo
         enddo
      endif
 999  itemp = 0
      call FTCLOS(ilun,itemp)
      call FRELUN(ilun)


      return
      end
c
c
c ---------------------------------------------
      subroutine XSL_COL2KEY(incol,outkey,outcom,ncol,MXCOL,
     &     filnam,catidx,catrows,MAXROWS,status)
c ---------------------------------------------
c This takes the values in a column of a FITS file, and makes the
c distinct values into a '::' delimited string.  IF outkey == 'NONE',
c then the values are returned in outcom, otherwise they are written
c to a keyword outkey in the file.
c JCI Mod 6/21/94
c Modified: Jeff Guerber, HSTX, Sep 1997. Replaced XSL_EXTSTRIP with FCPARS
c (and trap FCPARS's flag values).
c

      integer ncol,MXCOL,status,SUBMAX,MAXROWS
      parameter (SUBMAX = 10)
      character*(*) incol(MXCOL),outkey(MXCOL),outcom(MXCOL),filnam
      character(80) str1,tmpvec(SUBMAX),outval(SUBMAX)
      character(512) context
      character(255) name
      character(8) cnull
      integer catidx(MAXROWS),catrows
      integer block,htype,rwmode,LENACT,ilun,exten,len1,len2,tmpstat
      integer nrows,i,j,colno(SUBMAX),outlen(SUBMAX)
      logical EXACT,ANYF,TRUNC(SUBMAX)

      EXACT = .FALSE.
      do i=1,ncol
         TRUNC(i) = .FALSE.
      enddo
      rwmode = 1
      status = 0
      cnull = ' '

c Parse the input name
      call FCPARS(filnam,name,exten,status)

c     Default to extension 1.  Error if "*".
      if (exten .eq. -99)  exten = 1
      if (exten .eq. -1)  then
          context = 'COL2KEY: invalid extension number: '//filnam
          call XWRITE(context,5)
          status = -2
      endif

      IF(status.ne.0) THEN
         context = 'Error parsing file name'//filnam
         call XWRITE(context,5)
      endif

c Open the input file:
      call GETLUN(ilun)
      call FTOPEN(ilun, name, rwmode, block, status)
      if (status .eq. 103) then
         context='Unable to find FITS file '//name
         call XWRITE(context,5)
         call FRELUN(ilun)
         status = 0
         return
      else if (status .ne.0) then
         context='1, Unable to open FITS file '//name
         call XWRITE(context,5)
         tmpstat = 0
         call FTCLOS(ilun,tmpstat)
         call FRELUN(ilun)
         return
      endif
c go to first extension
      call FTMRHD(ilun, exten, htype, status)
      if(status .gt. 0) then
         write(context,95) exten,name
 95      format('Unable to move to ext# ',i3,' in file',a160)
         call XWRITE(context,5)
         tmpstat = 0
         call FTCLOS(ilun,tmpstat)
         call FRELUN(ilun)
         status=-10
         return
      endif

c Get the naxis and the column numbers, and initialize outkey
      call FTGKYJ(ilun,'NAXIS2',nrows,str1,status)
      do i=1,ncol
         call FTGCNO(ilun,EXACT,incol(i),colno(i),status)
         IF(status.ne.0) THEN
            colno(i) = -1
            context = 'Couldn''t find column '//incol(i)
            call XWRITE(context,5)
         ENDIF
         outlen(i) = 0
         outval(i) = ' '
      enddo
c Get the column values, catidx(1) = -1 is the flag to use all.
      if(catidx(1).eq.-1) then
         do i=1,nrows
            do j=1,ncol
               call FTGCVS(ilun,colno(j),i,1,1,cnull,tmpvec(j),
     &              ANYF,status)
               len1 = LENACT(tmpvec(j))
               len2 = outlen(j) + len1 + 2
               IF(len1 .eq. 0) THEN
                  continue
               ELSE IF(index(outval(j),tmpvec(j)(:len1)).eq.0) THEN
                  IF(outlen(j).eq.0) THEN
                     outval(j) = tmpvec(j)(:len1)
                     outlen(j) = len1
                  ELSE IF(len2.le.80) THEN
                     outval(j) = outval(j)(:outlen(j))//'::'
     &                    //tmpvec(j)(:len1)
                     outlen(j) = len2
                  ELSE IF(.not.TRUNC(j)) THEN
                     context = 'Value too long for keyword '//
     &                    outkey(j)
                     call XWRITE(context,5)
                     call XWRITE('It will be truncated.',5)
                     TRUNC(j) = .true. 
                  ENDIF
               ENDIF
            enddo
         enddo
      else
         do i=1,catrows
            do j=1,ncol
               call FTGCVS(ilun,colno(j),catidx(i),1,1,cnull,tmpvec(j),
     &              ANYF,status)
               len1 = LENACT(tmpvec(j))
               len2 = outlen(j) + len1 + 2
               IF(len1 .eq. 0) THEN
                  continue
               ELSE IF(index(outval(j),tmpvec(j)(:len1)).eq.0) THEN
                  IF(outlen(j).eq.0) THEN
                     outval(j) = tmpvec(j)(:len1)
                     outlen(j) = len1
                  ELSE IF(len2.le.80) THEN
                     outval(j) = outval(j)(:outlen(j))//'::'
     &                    //tmpvec(j)(:len1)
                     outlen(j) = len2
                  ELSE IF(.not.TRUNC(j)) THEN
                     context = 'Value too long for keyword '//
     &                    outkey(j)
                     call XWRITE(context,5)
                     call XWRITE('It will be truncated.',5)
                     TRUNC(j) = .true. 
                  ENDIF
               ENDIF
            enddo
         enddo

      endif
c Now write them as the keywords, overwriting if they already exist:
      do i=1,ncol
         if(outkey(i).ne.'NONE' ) then
            call FTMKYS(ilun,outkey(i)(:8),outval(i),
     &           outcom(i),status)
            IF(status.ne.0) THEN
               status = 0
               call FTPKYS(ilun,outkey(i)(:8),outval(i),
     &              outcom(i),status)
            ENDIF
         else
            outcom(i) = outval(i)
         endif
      enddo
      call FTCLOS(ilun,status)
      call FRELUN(ilun)


      return
      end
c
c
c ---------------------------------------------
      subroutine XSL_EXTRACT(infile,outfil,extnam,ECHO,status)
c ---------------------------------------------
c This routine extracts the extension extnam from infile, and
c puts it into outfil.  Both are assumed to be in the work directory.
c Jim Ingham Sept 93

      integer ilun,status,LENACT,len2,ierr,extno
      character*(*) infile, outfil, extnam
      character(255) cmdfil, lstfil,errfil,wrkdir,str1
      character(3) extnum
      logical ECHO
      common /xselcmd/ cmdfil, lstfil,errfil,wrkdir

      status = 0

      call XSL_RMFILE(outfil)
      call XSL_OPCF(cmdfil,ilun)

c
      call XSL_GET_EXTNO(infile,wrkdir,extnam,extno,extnum,status)
      if(status.ne.0) then
         str1 = 'Could not find extension '//extnam(:LENACT(extnam))//
     &        'in file:'
         call XWRITE(str1,5)
         str1 = '     '//infile
         call XWRITE(str1,5)
         return
      endif
      len2 = LENACT(infile)
      str1 = 'fextract '//infile(:len2)//'+'//extnum//' '//
     &             outfil(:LENACT(outfil))
      call XSL_OPCF(cmdfil,ilun)
      call XSL_WRTCF(ilun,str1,1)
      call XSL_CLCF(ilun)

      call XSL_RUNCF(cmdfil,ECHO,ierr)

      IF(status.ne.0) THEN
         str1 = 'Error extracting extension '//extnam
         call XWRITE(str1,5)
      ENDIF
      return

      end
c
c
c ---------------------------------------------
      subroutine XSL_FITS_2_QDP(curfil,keytim,keyrat,keyrte,ratext,
     &     lcdata,MXCLBN, MXLCLN,iery,MXVEC, nrow,npts,
     &     nvec,cmd,MXCMD,ncmd,status)
c ---------------------------------------------
c THis reads FITS XRONOS format rate files, and produced a QDP vector,
c and commands appropriate for PLT.
c J. Ingham 1/94

      integer MXLCLN,MXSTEP,STEP,MXCLBN,MXVEC,MXCMD
      parameter (MXSTEP = 360)
      double precision timdel,toffset,tstop,tfirst
      double precision timecl(MXSTEP),dnulval
      real lcdata(MXCLBN),temp1(MXSTEP),temp2(MXSTEP),nulval,timerr
      integer iery(MXVEC)
      integer nrow,npts,nvec,ncmd
      integer ilun,rwmode,block, status,hdutype,LENACT,rcnum,ecnum
      integer spoint,nblks,nrem,i,j,tcnum
      integer start1,start2,nrow2,nrow3,nrow4,extno
      character*(*) cmd(MXCMD),curfil,keytim,keyrat,keyrte,ratext
      character(80) comment,extnam,ratunit
      character(8) tunit
      character(255) context
      LOGICAL ANYF,TIME,RTERR

      status = 0
      block = 0
      rwmode = 0
      extno = 0
      nulval = -1.0
      dnulval = -1.0
      nrow = MXLCLN
      nrow2 = nrow*2
      nrow3 = nrow*3
      nrow4 = nrow*4
      nvec = 3

      iery(1) = 1
      iery(2) = 1
      do i=3,MXVEC
         iery(i) = 0
      enddo


      call GETLUN(ilun)
      call FTOPEN(ilun,curfil,rwmode,block,status)
      IF(status.ne.0) THEN
c We will use -10 to signify that the file was unopenable at start,
c which usually means that it is not a FITS file.
         status = -10
         call FRELUN(ilun)
         return
      ENDIF
c Loop until the RATE extension is found:

 105  extno = extno + 1
      call FTMRHD(ilun,1,hdutype,status)
      IF(status .eq. 107) THEN
         context = 'Got to end of file without finding '//
     &        'Light Curve extension: '//ratext
         call XWRITE( context,5)
         goto 999
      ELSEIF(status.ne.0) THEN
         write(context,57) extno,curfil(:LENACT(curfil))
 57      format('Error moving to ext. no. ',i3,' in file ',a)
         call XWRITE( context,5)
         goto 999
      ENDIF

      call FTGKYS(ilun,'EXTNAME',extnam,comment,status)
      IF(status.ne.0) THEN
         status = 0
      ELSEIF(extnam.ne.ratext)THEN
         goto 105
      ENDIF
c We've found the RATE extension, so get the necessary keywords
      call FTGKYJ(ilun,'NAXIS2',npts,comment,status)
      IF(status .ne. 0) THEN
         write(context,58) 'NAXIS2',status
 58      format('Unable to get ',a,' keyword: FITSIO errno ',i4)
         call XWRITE( context,5)
         goto 999
      ELSEIF (npts.gt.MXLCLN) THEN
         context = 'Too many rows in file '//curfil//'.'
         call XWRITE(context,5)
         write(context,59) npts,MXLCLN
 59      format('   There were ',i8,', truncating to ',i8)
         call XWRITE(context,5)
         npts = MXLCLN
      ELSEIF (npts .eq. 0) THEN
         call XWRITE('The light curve is empty.',5)
         return
      ENDIF
      call FTGKYJ(ilun,'NAXIS1',i,comment,status)
      IF(status .ne. 0) THEN
         write(context,58) 'NAXIS1',status
         call XWRITE( context,5)
         goto 999
      ENDIF
c Find the number of rows in each FITS block:
      STEP = 2880/i

c See if there is a time column:
      call FTGCNO(ilun,.TRUE.,keytim,tcnum,status)
      IF(status.ne.0) THEN
         status = 0
         TIME = .FALSE.
      ELSE
         TIME = .TRUE.
      ENDIF

c Now get the required timing keywords,...

      call FTGKYD(ilun,'TIMEDEL',timdel,comment,status)
      IF(status .ne. 0) THEN
c If we don't find timdel then just set the error to 0
         write(context,58) 'TIMEDEL',status
         call XWRITE( context,5)
         call XWRITE('Setting the TIME error to zero.',5)
         timdel = 0
         status = 0
      ENDIF
      call XSL_GETKIF(ilun,'TIMEZERO','TIMEZERI','TIMEZERF',
     &     toffset,status)
c      print*,'Toffset = ',toffset,' status = ',status

c Get the first time stamp and add it to toffset. Below we will subtract
c this time from all time stamps.

      IF ( TIME ) THEN
         call FTGCVD(ilun,tcnum,1,1,1,dnulval,tfirst,ANYF,status)
         toffset = toffset + tfirst
      ELSE
         tfirst = 0.d0
      ENDIF

      call XSL_GETKIF(ilun,'TSTOP','TSTOPI','TSTOPF',tstop,status)
c      print*,'Tstop = ',tstop,' status = ',status


c Now read in the RATE and ERROR columns
      call FTGCNO(ilun,.TRUE.,keyrat,rcnum,status)
      IF(status.ne.0) THEN
         i = LENACT(keyrat)
         write(context,60) keyrat(:i),keyrat(:i)
 60      format('Could not find ',a,' in ',a,' extension')
         call XWRITE( context,5)
         goto 999
      ENDIF

c Get the units for the RATE column:
      call FTKEYN('TUNIT',rcnum,tunit,status)
      call FTGKYS(ilun,tunit,ratunit,comment,status)
      IF(status.ne.0) THEN
         status = 0
         ratunit = 'Counts/Time'
      ENDIF

c SAEXTRCT doesn't yet write an error column, so...
      IF(keyrte.eq.'NONE') THEN
         RTERR = .FALSE.
      ELSE
         RTERR = .TRUE.
         call FTGCNO(ilun,.TRUE.,keyrte,ecnum,status)
         IF(status.ne.0) THEN
            write(context,60) keyrte(:LENACT(keyrte))
     &           ,keyrat(:LENACT(keyrat))
            call XWRITE( context,5)
            goto 999
         ENDIF
      ENDIF



c This reads in the FITS file a block at a time.
c The rates files have naxis1 = 8, so STEP = 128

      nblks = npts/STEP
      nrem = npts - nblks*STEP
      spoint = 1

c If there is not one full block...
      IF(nblks.eq.0) goto 205

      do i=1,nblks
         IF(TIME) THEN
            call FTGCVD(ilun,tcnum,spoint,1,STEP,dnulval,
     &           timecl,ANYF,status)
            IF(status.ne.0) THEN
               write(context,62) keytim(:LENACT(keytim)),status,
     &              spoint,STEP
               call XWRITE( context,5)
               goto 999
            ENDIF
         ENDIF
         call FTGCVE(ilun,rcnum,spoint,1,STEP,nulval,
     &        temp1,ANYF,status)
         IF(status.ne.0) THEN
            write(context,62) keyrat(:LENACT(keyrat)),status,
     &           spoint,STEP
            call XWRITE( context,5)
            goto 999
         ENDIF
         IF(RTERR) THEN
            call FTGCVE(ilun,ecnum,spoint,1,STEP,nulval,
     &           temp2,ANYF,status)
            IF(status.ne.0) THEN
               write(context,62) 'ERROR',status,spoint,STEP
               call XWRITE( context,5)
               goto 999
            ENDIF
 62         format('Error reading ',a,' column: FITSIO error ',i3,
     &           ' between rows ',i5,' and ',i5)
         ELSE
            do j=1,STEP
               temp2(j) = 0.0
            enddo
         ENDIF

c Fill in the time column if there is one
         IF(TIME) THEN
            start1 = spoint - 1
            do j=1,STEP
               lcdata(start1 + j) = SNGL(timecl(j) - tfirst)
            enddo
         ENDIF
c Now fill in the lcdata vector for the rate and error
         start1 = nrow2 + spoint - 1
         start2 = nrow3 + spoint - 1
         do j=1,STEP
            lcdata(start1 + j) = temp1(j)
            lcdata(start2 + j) = temp2(j)
         enddo

c Increment the pointers
         spoint = spoint + STEP
      enddo

c Now get the remainder:
c First decrease the end pointer:

 205  IF(TIME) THEN
         call FTGCVD(ilun,tcnum,spoint,1,nrem,dnulval,
     &        timecl,ANYF,status)
         IF(status.ne.0) THEN
            write(context,62) keytim(:LENACT(keytim)),status,
     &           spoint,nrem
            call XWRITE( context,5)
            goto 999
         ENDIF
      ENDIF
      call FTGCVE(ilun,rcnum,spoint,1,nrem,nulval,
     &     temp1,ANYF,status)
      IF(status.ne.0) THEN
         write(context,62) 'RATE',status,spoint,nrem
         call XWRITE( context,5)
         goto 999
      ENDIF
      IF(RTERR) THEN
         call FTGCVE(ilun,ecnum,spoint,1,nrem,nulval,
     &        temp2,ANYF,status)
         IF(status.ne.0) THEN
            write(context,62) 'ERROR',status,spoint,nrem
            call XWRITE( context,5)
            goto 999
         ENDIF
      ELSE
         do j=1,nrem
            temp2(j) = 0.0
         enddo
      ENDIF

c Fill in the time column if there is one
      IF(TIME) THEN
         start1 = spoint - 1
         do j=1,STEP
            lcdata(start1 + j) = SNGL(timecl(j) - tfirst)
         enddo
      ENDIF

c Now finish the lcdata vector for the rate and error
      start1 = nrow2 + spoint - 1
      start2 = nrow3 + spoint - 1

      do j=1,STEP
         lcdata(start1 + j) = temp1(j)
         lcdata(start2 + j) = temp2(j)
      enddo

c Now we are done with the FITS files:

      call FTCLOS(ilun,status)
      call FRELUN(ilun)

c Now fill in the time column, and set the time error to half the
c binsize, and the deadtime to 1.0
      IF(TIME) THEN
         do i=1,npts
C THIS IS WRONG FOR NOW, THERE SHOULD BE SOME TIME ERROR COLUMN IN THIS
C CASE
            lcdata(nrow + i) = SNGL(timdel/2.0d0)
            lcdata(nrow4 + i) = 1.0
         enddo
      ELSE
         timerr = SNGL(timdel/2.0)
         do i=1,npts
            lcdata(i) = SNGL((i-1)*timdel)
            lcdata(nrow + i) = timerr
            lcdata(nrow4 + i) = 1.0
         enddo
      ENDIF

c Finally, fill in the commands for PLT:

      cmd(1) = 'LA F '//curfil(:LENACT(curfil))
c The fits toffset is in fact the middle of the first bin, but toffset
c should be the beginning of the first bin:
      toffset = toffset-timdel/2.0
      write(cmd(2),65) toffset, timdel
c      print*,'CMD(2) = ',cmd(2)(:LENACT(cmd(2)))
 65   format('Label Top Light Curve, starts at ',G23.16,
     &     ' Binned at: ',g13.6)
      cmd(3) = 'LA X Delta Time'
      cmd(4) = 'LA Y '//ratunit(:LENACT(ratunit))

      if (tstop.gt.0.0d-20) then
         write(cmd(5),*) 'rescale x   0   ',tstop - toffset
      else
         write(cmd(5),*) 'rescale x   0   ',lcdata(npts)
      endif
      ncmd = 5

      return
 999  i = 0
      call FTCLOS(ilun,i)
      call FRELUN(ilun)
      return

      end

c -----------------------------------------------------------------
      subroutine XSL_FITS_EXIST(filenm, status)
c -----------------------------------------------------------------

      INTEGER status
      CHARACTER filenm*(*)

c Routine to check whether a FITS file exists. Uses a call to FTOPEN
c to take into account the case when extension and/or filtering information
c is included in the filename.

      INTEGER iunit, rwmode, block

      rwmode = 0
      block = 1
      status = 0

      CALL getlun(iunit)
cd      print*,"EXIST", filenm(1:lenact(filenm))
      CALL ftopen(iunit, filenm, rwmode, block, status)
cd      print*,"EXIST status = ",status
      IF ( status .EQ. 0 ) CALL ftclos(iunit, status)
      CALL frelun(iunit)

      RETURN
      END

c -----------------------------------------------------------------
      subroutine XSL_FLIST(dmpfil,rows,prhead)
c -----------------------------------------------------------------
c This routine displays the FITS form of the observation catalogue
c using FLIST.
c J. Ingham 1/94
      character(255) cmdfil, lstfil,errfil,str1,wrkdir
      integer ilun,status,len1,LENACT
      logical ECHO
      character*(*) dmpfil,prhead,rows
      common /xselcmd/ cmdfil, lstfil,errfil, wrkdir

      ECHO = .FALSE.
      status = 0
      len1 = LENACT(dmpfil)

c  Now construct the command string

      str1 = 'flist '//dmpfil(1:len1)//' '//
     &               'STDOUT'//' '//
     &               '-'//' '//
     &                rows(:LENACT(rows))//' '//
     &               'prhead = '//prhead//' '//
     &                'prdata = yes'//' '//
     &               'showscale = yes'//' '//
     &               'skip = 1'//' '//
     &               'tdisp = no'//' '//
     &               'page = yes'
      call XSL_OPCF(cmdfil,ilun)
      len1 = LENACT(str1)
      write(ilun,50) str1(1:len1)
      call XSL_CLCF(ilun)
      call FRELUN(ilun)
      call XSL_RUNCF(cmdfil,ECHO,status)
 50   format(a)
      return
      end
c
c
c ---------------------------------------------
      subroutine XSL_FPARKEY(ilun,key,val,commen,fil)
c ---------------------------------------------
c
c Fixes up the string for an FPARKEY command line.  If the value is
c a number, then prefix it with #.  If you want a # for some reason
c as the first character, put ##.
c Otherwise we force it to be interpreted as a string by FITSIO.
c Jim Ingham Feb/93
c

      character*(*) key,val,fil,commen
      integer ilun
      integer LENACT,len1,len2
      character(80) dval,dcom
      character(512) string
      character(255) cmdfil, lstfil,errfil,wrkdir
      common /xselcmd/ cmdfil, lstfil,errfil,wrkdir

      len1 = LENACT(val)
      if(val(1:1).eq.'#') then
         if(val(2:2).eq.'#') then
            dval = '"'//val(2:len1)//'"'
            len1 = len1 + 1
         else
            dval = val(2:len1)
            len1 = len1-1
         endif
      else if(val(1:1).ne.'"') then
         dval = '"'//val(1:len1)
         len1 = len1 + 1
         if(dval(len1:len1).ne.'"') then
            dval = dval(:len1)//'"'
            len1 = len1 + 1
         endif
      else if (val(len1:len1).ne.'"') then
         dval = dval(:len1)//'"'
         len1 = len1 + 1
      endif
c Now put the "'s around the comment, but only one set.
      len2 = LENACT(commen)
      if(commen(1:1).ne.'"') then
         dcom = '"'//commen(1:len2)
         len2 = len2 + 1
         if(dcom(len2:len2).ne.'"') then
            dcom = dcom(:len2)//'"'
            len2 = len2 + 1
         endif
      else if (commen(len2:len2).ne.'"') then
         dcom = dcom(:len2)//'"'
         len2 = len2 + 1
      endif

      string = 'fparkey value='//dval(:len1)//' '//
     +     'fitsfile='//fil(:LENACT(fil))//' '//
     +     'keyword='//key(:LENACT(key))//' '//
     +     'comm ='//dcom(:len2)//' '//
     +     'add=yes'

C      len1 = LENACT(string)
      call XSL_WRTCF(ilun,string,1)
      return
      end
c
c
c ----------------------------------------
      subroutine XSL_GET_EXTNO(filenm,datdir,extnam,extno,extnum,status)
c ----------------------------------------
c This routine gets the FITS extension number, as an integer, extno,
c and as a left justified string, extnum, for the extension extnam of 
c file filenm. If there is more than one extension of the same EXTNAME, 
c this finds the first.
c Status non-zero is the flag that no ext has been found

      character*(*) filenm,extnam,extnum,datdir
      integer status,extno,ilun,rwmode,block,htype,stat2
      character(64) value
      character(255) context

      value = ' '
      call GETLUN(ilun)

      call XSL_DATDIR(filenm,datdir,0)
      rwmode = 0
      call FTOPEN(ilun,filenm, rwmode, block, status)
      if (status .eq. 103) then
         context='Unable to find FITS file '//filenm
         call XWRITE(context,5)
         call FRELUN(ilun)
         return
      else if (status .gt. 0) then
         context='2. Unable to open FITS file '//filenm
         call XWRITE(context,5)
         write(context,51) status
 51      format('Fitsio Error no.: ',i3)
         call XWRITE(context,5)
         goto 999
      endif

      extnum = ' '
      extno = 0
      stat2 = 0
      do while (stat2.eq.0)
         status = 0
         call FTGKYS(ilun,'EXTNAME',value,context,status)
         if(value.eq.extnam) then
            call XSL_I2LJSTR(extno,extnum)
            GOTO 999
         ENDIF
         extno = extno + 1
         call FTMRHD(ilun,1,htype,stat2)
      enddo

      if(extnum.eq.' ') then
         status = -10
      endif

 999  stat2 = 0
      call FTCLOS(ilun,stat2)
      call FRELUN(ilun)

      return
      end
c
c
c --------------------------------------------------
      subroutine XSL_GCOLKWI(filenm,extnum,datdir,colnam,kwdroot,value,
     & statv,nkeys,MXKEYS,status)
c --------------------------------------------------
c Finds the value for keywords of the form kwdrootnnn, where nnn is the
c number of the columngiven in colnam, from the file filenm in datdir.
c Returned in the vector value.
c Statv indicates whether the individual kwds were found, status
c whether there was a problem opening the file.
c J. Ingham 3/94
c Modified: Jeff Guerber, HSTX, Sep 1997. Replaced XSL_EXTSTRIP with FCPARS
c (and trap FCPARS's flag values).
c

      integer status,nkeys,MXKEYS,i,statv(MXKEYS),value(MXKEYS)
      character*(*) filenm,datdir,kwdroot(MXKEYS),extnum
      character*(*) colnam
      character(32) kwdnam
      integer ilun,block,rwmode,htype,exten,tmpstat,colnum
      character(255) cmdfil, lstfil,errfil,wrkdir
      character(255) str1,fname
      character(512) context
      common /xselcmd/ cmdfil, lstfil,errfil,wrkdir

      status = 0
      tmpstat = 0

      do i=1,nkeys
         statv(i) = 0
      enddo
      if(extnum.eq.' ') then
         call FCPARS(filenm,fname,exten,status)
         if (exten .eq. -99) exten = 1
         if (exten .eq. -1) then
             context = 'GCOLKWI: invalid extension number: '//filenm
             call XWRITE(context,5)
             status = -2
             return
         endif
      else
         read(extnum,'(BN,I3)') exten
         fname = filenm
      endif
      call GETLUN(ilun)
      call XSL_DATDIR(fname,datdir,0)

      rwmode = 0
      call FTOPEN(ilun,fname, rwmode, block, status)
      if (status .eq. 103) then
         context='Unable to find FITS file '//fname
         call XWRITE(context,5)
         call FRELUN(ilun)
         return
      else if (status .gt. 0) then
         context='3. Unable to open FITS file '//fname
         call XWRITE(context,5)
         write(context,51) status
         call XWRITE(context,5)
         goto 999
      endif

c go to appropriate  extension (or move to next extension if (SEARCH))
      call FTMRHD(ilun, exten, htype, status)
      if(status .ne. 0) then
c IF we get here, we have reached the end of the file on the first
c try, so we give an error message.
         write(context,50) exten,fname
 50      format('Unable to move to ext number ',i2,' in ',a160)
         call XWRITE(context,5)
         write(context,51) status
 51      format(' Fitsio error no.: ',i3)
         call XWRITE(context,5)
         goto 999
      endif

      call FTGCNO(ilun,.FALSE.,colnam,colnum,status)
      IF(status.ne.0) THEN
         context = 'Error getting column number for column '//colnam
         call XWRITE( context,5)
         goto 999
      ENDIF

c Now get the keywords:
      do i=1,nkeys
         call FTKEYN(kwdroot(i),colnum,kwdnam,status)
         call FTGKYJ(ilun,kwdnam,value(i),str1,statv(i))
      enddo


 999  call FTCLOS(ilun,tmpstat)
      call FRELUN(ilun)
      IF ( status .NE. 0 ) THEN
         WRITE(context, '(a, i7)') 'XSL_GCOLKWI: status = ', status
         call XWRITE(context,5)
      ENDIF
c Finally, if statv(i) is still negative, set value(i) to -999
      do i=1,nkeys
         IF(statv(i).ne.0) THEN
            value(i) = -999
         ENDIF
      enddo

      return
      end
c
c
c --------------------------------------------------
      subroutine XSL_GETKIF(ilun,kwdnam,kwdi,kwdf,dvalue,status)
c --------------------------------------------------
c This looks for kwdnam in ilun, and if not found, looks for kwdi & kwdf
c J Ingham 4/94

      character*(*) kwdnam,kwdi,kwdf
      double precision dvalue,dval2
      integer status1,status2,status3,status,ilun,LENACT
      character(64) errmsg,comment
      status1 = 0
      status2 = 0
      status3 = 0

      call FTGKYD(ilun,kwdnam,dvalue,comment,status1)

      IF(status1 .ne. 0) THEN
c First look for the KWDI, KWDF:
         call FTGKYD(ilun,kwdi,dvalue,comment,status2)
         call FTGKYD(ilun,kwdf,dval2,comment,status3)
         if(status2.ne.0.or.status3.ne.0) THEN
            write(errmsg,58) kwdnam(:LENACT(kwdnam)),status1
 58         format('Could not find keyword ',a,' : FITSIO error ',I4)
            call XWRITE(errmsg,5)
            status = status1
            if(status2.ne.0) then
               write(errmsg,58) kwdi(:LENACT(kwdi)),status2
               call XWRITE(errmsg,5)
            endif
            if(status3.ne.0) then
               write(errmsg,58) kwdf(:LENACT(kwdf)),status3
               call XWRITE(errmsg,5)
            endif
         else
            dvalue = dvalue + dval2
         endif
      ENDIF

      return
      end


c
c --------------------------------------------------
      subroutine XSL_GETKWD(filenm,extnum,datdir,kwdnam,value,
     &                           statv,nkeys,MXKEYS,SEARCH,status)
c --------------------------------------------------
c Gets the values of the keywords kwdnam from the file filenm and
c puts them in the vector value.  If search is true, then the extensions
c after the named one are searched until the keyword is found.
c If the i'th keyword is not found, statv(i)=-20, value(i)=-999
c
c J. Ingham 8/29/93
c Modified: Jeff Guerber, HSTX, Sep 1997. Replaced XSL_EXTSTRIP with FCPARS
c (and trap FCPARS's flag values).
c

      integer status,nkeys,MXKEYS,i,statv(MXKEYS)
      character*(*) filenm,datdir,kwdnam(MXKEYS),extnum
      double precision value(MXKEYS)
      integer ilun,block,rwmode,htype,exten,tmpstat
      character(255) cmdfil, lstfil,errfil,wrkdir
      character(255) str1,fname
      character(512) context
      logical SEARCH,FIRST
      common /xselcmd/ cmdfil, lstfil,errfil,wrkdir

      FIRST = .TRUE.
      status = 0
      tmpstat = 0

      do i=1,nkeys
         statv(i) = -20
      enddo
      if(extnum.eq.' ') then
         call FCPARS(filenm,fname,exten,status)
         if (exten .eq. -99) exten = 1
         if (exten .eq. -1) then
             context = 'GETKWD: invalid extension number: '//filenm
             call XWRITE(context,5)
             status = -2
             return
         endif
      else
         read(extnum,'(BN,I3)') exten
         fname = filenm
      endif
      call GETLUN(ilun)
      call XSL_DATDIR(fname,datdir,0)

      rwmode = 0
      call FTOPEN(ilun,fname, rwmode, block, status)
      if (status .eq. 103) then
         context='Unable to find FITS file '//fname
         call XWRITE(context,5)
         call FRELUN(ilun)
         return
      else if (status .gt. 0) then
         context='4. Unable to open FITS file '//fname
         call XWRITE(context,5)
         write(context,51) status
         call XWRITE(context,5)
         goto 999
      endif

c go to appropriate  extension (or move to next extension if (SEARCH))
 700  CONTINUE
      call FTMRHD(ilun, exten, htype, status)
      if(status .gt. 0.and. FIRST) then
c IF we get here, we have reached the end of the file on the first
c try, so we give an error message.
         write(context,50) exten,fname
 50      format('Unable to move to ext number ',i2,' in ',a160)
         call XWRITE(context,5)
         write(context,51) status
 51      format(' Fitsio error no.: ',i3)
         call XWRITE(context,5)
         goto 999
      else if (FIRST) THEN
c This is just to set the FIRST value, to show we've found the first ext.
         FIRST = .FALSE.
      else if(status .gt. 0) then
c We have come to the end of the file
         status = 0
         goto 999
      endif

c Now get the keywords:
      do i=1,nkeys
         IF(statv(i).ne.0) THEN
            statv(i) = 0
            call FTGKYD(ilun,kwdnam(i),value(i),str1,statv(i))
         ENDIF
      enddo
      IF(SEARCH) THEN
c This is just to set the FIRST value, to show we've found the first ext.
         if(FIRST) then
            FIRST = .FALSE.
c Go back to the first extension:
            call FTMAHD(ilun,1,htype,status)
         ENDIF
         do i=1,nkeys
            IF(statv(i).ne.0) THEN
               exten =  1
               goto 700
            ENDIF
         enddo
      ENDIF
 999  call FTCLOS(ilun,tmpstat)
      call FRELUN(ilun)
c Finally, if statv(i) is still negative, set value(i) to -999
      do i=1,nkeys
         IF(statv(i).ne.0) THEN
            value(i) = -999
         ENDIF
      enddo

      return
      end

c
c --------------------------------------------------
      subroutine XSL_GETKWI(filenm,extnum,datdir,kwdnam,value,
     &                           statv,nkeys,MXKEYS,SEARCH,status)
c --------------------------------------------------
c Gets the values of the keywords kwdnam from the file filenm and
c puts them in the vector value.  If search is true, then the extensions
c after the named one are searched until the keyword is found.
c If the i'th keyword is not found, statv(i)=-20, value(i)=-999
c If a keyword is NONE then return statv(i)=0 but value(i)=-999

c J. Ingham 8/29/93
c Modified: Jeff Guerber, HSTX, Sep 1997. Replaced XSL_EXTSTRIP with FCPARS
c (and trap FCPARS's flag values).

      integer status,nkeys,MXKEYS,i,statv(MXKEYS)
      character*(*) filenm,datdir,kwdnam(MXKEYS),extnum
      integer value(MXKEYS)
      integer ilun,block,rwmode,htype,exten,tmpstat
      character(255) cmdfil, lstfil,errfil,wrkdir
      character(255) str1,fname
      character(512) context
      logical SEARCH,FIRST
      common /xselcmd/ cmdfil, lstfil,errfil,wrkdir

      FIRST = .TRUE.
      status = 0
      tmpstat = 0

      do i=1,nkeys
         statv(i) = -20
      enddo
      if(extnum.eq.' ') then
         call FCPARS(filenm,fname,exten,status)
         if (exten .eq. -99) exten = 1
         if (exten .eq. -1) then
             context = 'GETKWI: invalid extension number: '//filenm
             call XWRITE(context,5)
             status = -2
             return
         endif
      else
         read(extnum,'(BN,I3)') exten
         fname = filenm
      endif
      call GETLUN(ilun)
      call XSL_DATDIR(fname,datdir,0)

      rwmode = 0
      call FTOPEN(ilun,fname, rwmode, block, status)
      if (status .eq. 103) then
         context='Unable to find FITS file '//fname
         call XWRITE(context,5)
         call FRELUN(ilun)
         return
      else if (status .gt. 0) then
         context='5. Unable to open FITS file '//fname
         call XWRITE(context,5)
         write(context,51) status
         call XWRITE(context,5)
         goto 999
      endif

c go to appropriate  extension (or move to next extension if (SEARCH))
 700  call FTMRHD(ilun, exten, htype, status)
      if(status .gt. 0.and. FIRST) then
c IF we get here, we have reached the end of the file on the first
c try, so we give an error message.
         write(context,50) exten,fname
 50      format('Unable to move to ext number ',i2,' in ',a160)
         call XWRITE(context,5)
         write(context,51) status
 51      format(' Fitsio error no.: ',i3)
         call XWRITE(context,5)
         goto 999
      else if (FIRST) THEN
c This is just to set the FIRST value, to show we've found the first ext.
         FIRST = .FALSE.
      else if(status .gt. 0) then
c We have come to the end of the file
         status = 0
         goto 999
      endif

c Now get the keywords:
      do i=1,nkeys
         IF(statv(i).ne.0) THEN
            statv(i) = 0
            IF ( kwdnam(i) .NE. 'NONE' ) THEN
               call FTGKYJ(ilun,kwdnam(i),value(i),str1,statv(i))
            ELSE
               value(i) = -999
            ENDIF
         ENDIF
      enddo
      IF(SEARCH) THEN
c This is just to set the FIRST value, to show we've found the first ext.
         if(FIRST) then
            FIRST = .FALSE.
c Go back to the first extension:
            call FTMAHD(ilun,1,htype,status)
         ENDIF
         do i=1,nkeys
            IF(statv(i).ne.0) THEN
               exten =  1
               goto 700
            ENDIF
         enddo
      ENDIF
 999  call FTCLOS(ilun,tmpstat)
      call FRELUN(ilun)
c Finally, if statv(i) is still negative, set value(i) to -999
      do i=1,nkeys
         IF(statv(i).ne.0) THEN
            value(i) = -999
         ENDIF
      enddo

      return
      end
c
c --------------------------------------------------
      subroutine XSL_GETKWST(filenm,extnum,datdir,kwdnam,value,
     &                           statv,nkeys,MXKEYS,SEARCH,status)
c --------------------------------------------------
c Gets the values of the keywords kwdnam from the file filenm and
c puts them in the vector value.  If search is true, then the extensions
c after the named one are searched until the keyword is found.
c If the i'th keyword is not found, statv(i)=-20, value(i)='NOT_FOUND'
c If the input keyword name has a suffix (m:n) then the returned value
c is characters m to n of the keyword value.

c J. Ingham 8/29/93
c Modified: Jeff Guerber, HSTX, Sep 1997. Replaced XSL_EXTSTRIP with FCPARS
c (and trap FCPARS's flag values).

      integer status,nkeys,MXKEYS,i,statv(MXKEYS)
      character*(*) filenm,datdir,kwdnam(MXKEYS),value(MXKEYS),extnum
      integer ilun,block,rwmode,htype,exten,tmpstat,istart,iend,klen
      integer k1,k2
      character(255) cmdfil, lstfil,errfil,wrkdir
      character(255) str1,fname,retval
      character(512) context
      logical SEARCH,FIRST
      common /xselcmd/ cmdfil, lstfil,errfil,wrkdir

      FIRST = .TRUE.
      status = 0
      tmpstat = 0

      do i=1,nkeys
         statv(i) = -20
      enddo
      if(extnum .eq. ' ') THEN
         call FCPARS(filenm,fname,exten,status)
         if (exten .eq. -99) exten = 1
         if (exten .eq. -1) then
             context = 'GETKWST: invalid extension number: '//filenm
             call XWRITE(context,5)
             status = -2
             return
         endif
      else
         read(extnum,'(BN,I3)') exten
         fname = filenm
      endif
      call GETLUN(ilun)
      if(datdir.ne.'NONE') then
         call XSL_DATDIR(fname,datdir,0)
      endif

      rwmode = 0
      call FTOPEN(ilun,fname, rwmode, block, status)

      if (status .eq. 103) then
         context='Unable to find FITS file '//fname
         call XWRITE(context,5)
         call FRELUN(ilun)
         return
      else if (status .gt. 0) then
         context='6. Unable to open FITS file '//fname
         call XWRITE(context,5)
         write(context,51) status
         call XWRITE(context,5)
         goto 999
      endif

c go to appropriate  extension (or move to next extension if (SEARCH))
 700  call FTMRHD(ilun, exten, htype, status)
      if(status .gt. 0.and. FIRST) then
c IF we get here, we have reached the end of the file on the first
c try, so we give an error message.
         write(context,50) exten,fname
 50      format('Unable to move to ext number ',i2,' in ',a160)
         call XWRITE(context,5)
         write(context,51) status
 51      format(' Fitsio error no.: ',i3)
         call XWRITE(context,5)
         goto 999
      else if(status .gt. 0) then
c We have come to the end of the file, status should be non-zero, since
c statv reports success in finding keywords.
         status = 0
         goto 999
      endif
      
c Now get the keywords. If the kwdnam has a suffix (n:m) then we return
c only characters n to m of the value.

      do i=1,nkeys
         IF(statv(i).ne.0) THEN
            statv(i) = 0
            IF ( index(kwdnam(i),'(') .NE. 0 ) THEN
               klen = index(kwdnam(i),'(') - 1
               k1 = klen + 2
               k2 = index(kwdnam(i),':') - 1
               READ(kwdnam(i)(k1:k2), *) istart
               k1 = k2 + 2
               k2 = index(kwdnam(i),')') - 1
               READ(kwdnam(i)(k1:k2), *) iend
            ELSE
               istart = 1
               iend = len(value(i))
               klen = len(kwdnam(i))
            ENDIF
            call FTGKYS(ilun,kwdnam(i)(:klen),retval,str1,statv(i))
            value(i) = retval(istart:MIN(iend,len(retval)))
         ENDIF
      enddo
      IF(SEARCH) THEN
c This is just to set the FIRST value, to show we've found the first ext.
         if(FIRST) then
            FIRST = .FALSE.
c Go back to the first extension:
            call FTMAHD(ilun,1,htype,status)
         ENDIF
         do i=1,nkeys
            IF(statv(i).ne.0) THEN
               exten =  1
               goto 700
            ENDIF
         enddo
      ENDIF
 999  tmpstat = 0
      call FTCLOS(ilun,tmpstat)
      call FRELUN(ilun)
c Finally, if statv(i) is still negative, set value(i) to 'NOT_FOUND'
      do i=1,nkeys
         IF(statv(i).ne.0) THEN
            value(i) = 'NOT_FOUND'
         ENDIF
      enddo

      return
      end
c
c
c --------------------------------------------------
      subroutine XSL_GET_SEL(filenm,datdir,value,nkeys,
     &     MXKEYS,status)
c --------------------------------------------------
c Gets the History keywords written by FSELECT from the catalogue file
c and puts them in the vector value.  Also gets the LIST_STR keyword
c if present
c J. Ingham 8/29/93
c Modified: Jeff Guerber, HSTX, Sept 1997. Replaced XSL_EXTSTRIP with FCPARS
c (and trap FCPARS's flag values).

      integer status,nkeys,MXKEYS,i,tmpstat
      character*(*) datdir,filenm,value(MXKEYS)
      integer ilun,block,rwmode,htype,exten,nkwds,len1
      integer LENACT,lenkey,point
      character(255) cmdfil, lstfil,errfil,wrkdir
      character(255) str1,fname
      character(512) context
      character(80) card
      common /xselcmd/ cmdfil, lstfil,errfil,wrkdir

      status = 0
      lenkey = len(value(1))

      call FCPARS(filenm,fname,exten,status)
c     Default to extension 1.  Error if "*".
      if (exten .eq. -99)  exten = 1
      if (exten .eq. -1)  then
          context = 'GET_SEL: invalid extension number: '//filenm
          call XWRITE(context,5)
          status = -2
      endif
      if (status .ne. 0)  return

      call GETLUN(ilun)

      rwmode = 0
      call XSL_DATDIR(fname,datdir,0)
      call FTOPEN(ilun,fname, rwmode, block, status)
      if (status .eq.103) then
         context='Unable to find FITS file '//fname
         call XWRITE(context,5)
         status = 0
         call FRELUN(ilun)
         return
      else if (status .gt. 0) then
         context='7. Unable to open FITS file '//fname
         call XWRITE(context,5)
         write(context,51) status
         call XWRITE(context,5)
         tmpstat = 0
         call FTCLOS(ilun,tmpstat)
         call FRELUN(ilun)
         return
      endif

c go to appropriate  extension
      call FTMRHD(ilun, exten, htype, status)
      if(status .gt. 0) then
         write(context,50) exten,fname
 50      format('Unable to move to ext number ',i2,' in ',a160)
         call XWRITE(context,5)
         write(context,51) status
 51      format(' Fitsio error no.: ',i3)
         call XWRITE(context,5)
         tmpstat=0
         call FTCLOS(ilun,tmpstat)
         call FRELUN(ilun)
         return
      endif

c Now get the keywords:
      call FTGKYS(ilun,'LIST_STR',value(1),str1,status)
      IF(status.ne.0) THEN
         value(1) = 'NOT_FOUND'
         status = 0
      ENDIF

c The logic of the following section is hideous.  This would be much
c simpler if there was some nice way in FITSIO to read in continued
c history records.

      call FTGHSP(ilun,nkwds,i,status)
      nkeys = 1
      i = 1
      do while(i.le.nkwds)
         call FTGREC(ilun,i,card,status)
         IF(status.ne.0) THEN
            write(card,87) i
 87         format('Error getting header record: ',i3)
            call XWRITE(card,5)
            write(card,88) fname
 88         format('In file ',a70)
            call XWRITE(card,5)
            tmpstat = 0
            call FTCLOS(ilun,tmpstat)
            call FRELUN(ilun)
            return
         ENDIF
         IF(card(1:8).eq.'HISTORY') THEN
            IF(index(card,'FSELECT').ne.0) THEN
 98            i=i+1
               IF(i.gt.nkwds) THEN
                  goto 118
               ENDIF
               IF(nkeys.eq.MXKEYS) THEN
                  call XWRITE('Too many selection expressions',5)
                  status = 0
                  call FTCLOS(ilun,status)
                  call FRELUN(ilun)
                  return
               ENDIF
c There may be a continuation of the FSELECT card.
 108           call FTGREC(ilun,i,card,status)
               IF(index(card,'Expression').eq.0) THEN
                  i = i + 1
                  IF(i.gt.nkwds) THEN
                     goto 118
                  ENDIF
                  goto 108
               ENDIF
               nkeys = nkeys+1
               value(nkeys) = card(23:)
               point = 58
 109           len1 = LENACT(card)
               IF(len1.eq.80) THEN
                  i=i+1
                  IF(i.gt.nkwds) THEN
                     goto 118
                  ENDIF
                  call FTGREC(ilun,i,card,status)
c There should be an end of HISTORY character for continued
c history records, until then, this will have to do:
                  IF(card(1:8).eq.'HISTORY') THEN
c If it doesn't say 'TASK', it probably wasn't written by an Ftool, so
c I bet it is a continuation.
                     IF(index(card,'TASK').eq.0) THEN
                        value(nkeys) = value(nkeys)(:point)//card(11:)
                        point = point + 70
                        IF (point.le.lenkey) THEN
c If this card was also 80 characters long, need to get the next one...
                           go to 109
                        ENDIF
c If it is another FSELECT card, we want it:
                     ELSE IF(index(card,'FSELECT').ne.0) THEN
                        goto 98
                     ENDIF
c Otherwise, just go on.
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
         i = i + 1
      enddo
 118  tmpstat = 0
      call FTCLOS(ilun,tmpstat)
      call FRELUN(ilun)
      return
      end
c

c ---------------------------------------------
      subroutine XSL_FILES_PARSE(filstr, datdir, filist, numfil, 
     &                           MAXFIL,status)
c ---------------------------------------------

      CHARACTER filstr*(*), datdir*(*)
      INTEGER numfil, MAXFIL, status
      CHARACTER*(*) filist(MAXFIL)

c This routine parses the string filstr and turns it into a list of FITS files
c If filstr is itself an ascii filename containing a list of FITS files then
c this file is read. This is basically a wrap-up of XSL_PARSE and it attempts
c to be slightly clever about checking whether filstr is an ascii file with
c a list of files without requiring the user to prepend the string with "@"

      CHARACTER(512) tmpstr, contxt

      LOGICAL qexist

      INTEGER lenact
      EXTERNAL lenact

c Parse the string

      CALL XSL_PARSE(filstr, filist, numfil, MAXFIL, status)
      IF ( status .NE. 0 .OR. numfil .EQ. 0 ) RETURN

c Construct the full filename

      IF ( datdir .NE. 'NONE' .AND. filist(1)(1:4) .NE. 'ftp:' ) THEN
         tmpstr = datdir(:lenact(datdir))//'/'//
     &            filist(1)(:lenact(filist(1)))
      ELSE
         tmpstr = filist(1)(:lenact(filist(1)))
      ENDIF

c Check whether the file actually exists

      INQUIRE(file=tmpstr, exist=qexist)
      IF ( .NOT.qexist ) THEN
         contxt = 'Cannot find file '//tmpstr(:lenact(tmpstr))
         CALL xwrite(contxt, 5)
         status = 1
         RETURN
      ENDIF

c Check whether the first file is actually a FITS file

      CALL XSL_FITS_EXIST(tmpstr, status)

c If it is then assume that we are done

      IF ( status .EQ. 0 ) RETURN

c Otherwise we try to use the file as an ascii file containing a list of
c FITS files

      status = 0
      tmpstr = '@'//filist(1)(:MAX(len(tmpstr)-1,lenact(filist(1))))
      CALL XSL_PARSE(tmpstr, filist, numfil, MAXFIL, status)

c If this fails then give up

      IF ( status .NE. 0 ) THEN
         contxt = 'The file '//filist(1)(:lenact(filist(1)))
         CALL xwrite(contxt, 5)
         contxt = 'is neither a FITS file nor an ASCII file '//
     &            'containing a list of FITS files'
         CALL xwrite(contxt, 5)
      ENDIF

      RETURN
      END
c
c
c ---------------------------------------------
      subroutine XSL_PARSE(columns,colist,numcols,MAXCOL,status)
c ---------------------------------------------
c   Same as fcgcls from the ftools misc.f file, with fcstln -> LENACT
c   Jim Ingham Feb/93
c
        character*(*) columns
        integer numcols,MAXCOL,status
        character*(*) colist(MAXCOL)
c       logical negflag
        character(255) col_fname,context
        integer i, j, LENACT, colen, cname_index, funit, ios

        status = 0

        colen = LENACT(columns)

c  trap out case of a blank input string

        numcols = 0
        IF ( colen .EQ. 0 ) RETURN

c  find position of first nonwhite character

        do 10 i = 1, colen
            if (columns(i:i) .ne. ' ') goto 11
 10     continue
 11     cname_index = i

C  if first character is @ then the column names are in a file
        if (columns(cname_index:cname_index) .eq. '@') then
           call GETLUN(funit)
            col_fname = columns(cname_index+1:)
            open(unit=funit,file=col_fname,iostat=ios,status='old')
            if (ios .ne. 0) then
                context = 'file containing column list not found'
                CALL XWRITE(context,5)
                close(funit)
                call FRELUN(funit)
                status = -5
                goto 999
            else
                i = 1
 20             read(funit,1000,end=30) colist(i)
 1000   format(A)
C       deal with blank lines (EAG 8/25/92)
                if (LENACT(colist(i)) .le. 0) go to 20
                i = i + 1
                goto 20
 30             CONTINUE
                close(funit)
                CALL FRELUN(funit)
                numcols = i - 1
            endif

C  copy column names into column list
        else
            i = cname_index
            j = 1
 40         if (i .eq. colen) goto 42
            if (columns(i:i) .eq. '"') then
                i = i + 1
                cname_index = i
 43             if (columns(i:i) .eq. '"') goto 44
                i = i + 1
                goto 43
 44             IF(j.gt.MAXCOL) THEN
                   status = -10
                   goto 999
                ENDIF
                colist(j) = columns(cname_index:i-1)
                i = i + 2
                cname_index = i
                if ( i .ge. colen ) goto 45
                j = j + 1
                goto 40
            endif
            if ((columns(i:i) .eq. ' ') .or. (columns(i:i) .eq. ','))
     &          goto 41
            i = i + 1
            goto 40
 41         IF(j.gt.MAXCOL) THEN
               status = -10
               goto 999
            ENDIF
            colist(j) = columns(cname_index:i-1)
 46         i = i + 1
            if ( columns(i:i) .eq. ' ' ) goto 46
            cname_index = i
            j = j + 1
            goto 40
 42         IF(j.gt.MAXCOL) THEN
               status = -10
               goto 999
            ENDIF
            colist(j) = columns(cname_index:i)
 45         continue
            numcols = j
        endif

C  check for exclude column flag
c       negflag = .false.
c       if (colist(1)(1:1) .eq. '-') then
c           negflag = .true.
c           colen = LENACT(colist(1))
c           do 50 i = 1, colen
c               colist(1)(i:i) = colist(1)(i+1:i+1)
c50         continue
c       endif

 999    continue

        return
        end
c
c --------------------------------------------------
      subroutine XSL_PUTKWST(filenm,extnum,datdir,kwdnam,value,
     &                           nkeys,MXKEYS,status)
c --------------------------------------------------
c Puts the values in value into the keywords kwdnam of the file filenm,
c which is in the directory datdir.
c
c J. Ingham 8/29/93
c Modified: Jeff Guerber, HSTX, Sep 1997. Replaced XSL_EXTSTRIP with FCPARS
c (and trap FCPARS's flag values).
c

      integer status,nkeys,MXKEYS,i
      character*(*) filenm,datdir,kwdnam(MXKEYS),value(MXKEYS),extnum
      integer ilun,block,rwmode,htype,exten,tmpstat,LENACT
      character(255) fname
      character(512) context
      logical WARN
c We use the long string convention, but only write the long string
c waning if the keywords didn't already exist.

      WARN = .FALSE.

      status = 0
      tmpstat = 0

      if(extnum .eq. ' ') THEN
         call FCPARS(filenm,fname,exten,status)
         if (exten .eq. -99) exten = 1
         if (exten .eq. -1) then
             context = 'PUTKWST: invalid extension number: '//filenm
             call XWRITE(context,5)
             status = -2
             return
         endif
      else
         read(extnum,'(BN,I3)') exten
         fname = filenm
      endif
      call GETLUN(ilun)
      call XSL_DATDIR(fname,datdir,0)

      rwmode = 1
      call FTOPEN(ilun,fname, rwmode, block, status)
      if (status .eq. 103) then
         context='Unable to find FITS file '//fname
         call XWRITE(context,5)
         call FRELUN(ilun)
         return
      else if (status .ne. 0) then
         context='8. Unable to open FITS file '//fname
         call XWRITE(context,5)
         write(context,51) status
         call XWRITE(context,5)
         goto 999
      endif

c go to appropriate  extension (or move to next extension if (SEARCH))
      call FTMRHD(ilun, exten, htype, status)
      if(status .ne. 0) then
         write(context,50) exten,fname
 50      format('Unable to move to ext number ',i2,' in ',a160)
         call XWRITE(context,5)
         write(context,51) status
 51      format(' Fitsio error no.: ',i3)
         call XWRITE(context,5)
         goto 999
      endif

c Now put the keywords:
      do i=1,nkeys
         tmpstat = 0
         rwmode = max(1,LENACT(value(i)))
         call FTMKYS(ilun,kwdnam(i),value(i)(:rwmode),
     &        '&',tmpstat)
         if(tmpstat.eq.202) THEN
            tmpstat = 0
            call FTPKLS(ilun,kwdnam(i),value(i)(:rwmode),
     &           ' ',tmpstat)
            WARN = .TRUE.
         endif
         if (tmpstat.ne.0) then
            context = 'Error fixing keyword '//kwdnam(i)//
     &           ' in file '//filenm(:LENACT(filenm))
            call XWRITE(context,5)
            context = 'Tried to write |'//value(i)(:rwmode)//'|'
            call XWRITE(context,5)
            write(context,'(''Fitsio Error No: '',I4)') tmpstat
            call XWRITE(context,5)
         endif
      enddo
      if(WARN) THEN
         call FTPLSW(ilun,status)
      endif

 999  tmpstat = 0
      call FTCLOS(ilun,tmpstat)
      status = max(status,tmpstat)
      call FRELUN(ilun)

      return
      end

c
c
c --------------------------------------------------
      subroutine XSL_TLIMITS(filenm,datdir,startcol,stopcol,tstart,
     &     tstop,status)
c --------------------------------------------------
c This gets the first value in startcol, and puts that in tstart,
c and the last value of stopcol, and puts that in tstop.
c J. Ingham 4/94
c Modified: Jeff Guerber, HSTX, Sept 1997. Replaced XSL_EXTSTRIP with FCPARS
c (and trap FCPARS's flag values).

      character*(*) filenm,startcol,stopcol,datdir
      integer status,startno,stopno,nrows
      double precision tstart,tstop,dnull
      character(64) comment
      integer ilun,block,rwmode,htype,exten,tmpstat,LENACT
      character(255) fname
      character(512) context
      logical EXACT,ANYF

      EXACT = .FALSE.
      status = 0
      tmpstat = 0
      dnull = 0.0d0

c The flag for no value found:
      tstart = -1.0d11
      tstop = -1.0d11

      call FCPARS(filenm,fname,exten,status)
c     Default to extension 1.  Error if "*".
      if (exten .eq. -99)  exten = 1
      if (exten .eq. -1)  then
          context = 'TLIMITS: invalid extension number: '//filenm
          call XWRITE(context,5)
          status = -2
      endif
      if (status .ne. 0)  return

      call GETLUN(ilun)
      call XSL_DATDIR(fname,datdir,0)

      rwmode = 0
      call FTOPEN(ilun,fname, rwmode, block, status)
      if (status .eq. 103) then
         context='Unable to find FITS file '//fname
         call XWRITE(context,5)
         call FRELUN(ilun)
         return
      else if (status .ne. 0) then
         context='9, Unable to open FITS file '//fname
         call XWRITE(context,5)
         write(context,51) status
         call XWRITE(context,5)
         goto 999
      endif

c go to appropriate  extension (or move to next extension if (SEARCH))
      call FTMRHD(ilun, exten, htype, status)
      if(status .ne. 0) then
         write(context,50) exten,fname
 50      format('Unable to move to ext number ',i2,' in ',a160)
         call XWRITE(context,5)
         write(context,51) status
 51      format(' Fitsio error no.: ',i3)
         call XWRITE(context,5)
         goto 999
      endif

      call FTGKYJ(ilun,'NAXIS2',nrows,comment,status)
      IF(status .ne. 0) THEN
         write(context,58) 'NAXIS2',status
 58      format('Unable to get ',a,' keyword: FITSIO errno ',i4)
         call XWRITE( context,5)
         goto 999
      ELSE IF(nrows.eq.0) THEN
         context = 'File '//filenm(:LENACT(filenm))//' is empty.'
         call XWRITE(context,5)
         goto 999
      ENDIF

      call FTGCNO(ilun,EXACT,startcol,startno,status)
      IF(status.ne.0) THEN
         write(context,59) startcol(:LENACT(startcol)),status
 59      format('Unable to find column ',a,' : FITSIO errno ',i4)
         call XWRITE( context,5)
         goto 999
      ENDIF

      call FTGCNO(ilun,EXACT,stopcol,stopno,status)
      IF(status.ne.0) THEN
         write(context,59) stopcol(:LENACT(stopcol)),status
         call XWRITE( context,5)
         goto 999
      ENDIF

      call FTGCVD(ilun,startno,1,1,1,dnull,tstart,anyf,status)
      call FTGCVD(ilun,stopno,nrows,1,1,dnull,tstop,anyf,status)


 999  call FTCLOS(ilun,tmpstat)
      call FRELUN(ilun)

      return
      end
c
c
c --------------------------------------------------
      subroutine XSL_WRITE_GTI(outfile,tstart,tstop,ngtis,MXGTI,
     &     refmjdi,refmjdf,keyuni,status)
c --------------------------------------------------
c This writes a GTI file with name outfile.  The MJDREF is given by
c refmjdi/f, and the timesys, and unit keywords are also passed.
c J. Ingham 9/10/94

      INCLUDE 'xsel.inc'

      character*(*) outfile, keyuni
      integer ngtis,MXGTI,status,refmjdi
      double precision tstart(MXGTI),tstop(MXGTI),refmjdf

      integer olun,block,tfields,i
      character(8) ttype(2),tform(2),tunit(2)
      character(32) str1

      data tfields /2/
      data (ttype(i), i=1,2) /'START','STOP'/
      data (tform(i), i=1,2) /2*'1D'/

      tunit(1) = keyuni
      tunit(2) = keyuni

      status = 0
      block = 2880

      call XSL_RMFILE(outfile)
      call GETLUN(olun)
      call FTINIT(olun,outfile,block,status)
      call FTPHPR(olun,.TRUE.,32,0,0,0,1,.TRUE.,status)
      call FTPDEF(olun,32,0,0,0,1,status)
      call FTCRHD(olun,status)
      call FTPHBN(olun,ngtis,tfields,ttype,tform,tunit,
     &     'STDGTI',0,status)
      call FTPKYJ(olun,'MJDREFI',refmjdi,
     &           'integer MJD corresponding to SC clock start',
     &            status)
      call FTPKYD(olun,'MJDREFF',refmjdf,12,
     &           'fractional MJD corresponding to SC clock start',
     &            status)
      call FTPKYS(olun,'TIMEUNIT',keyuni,
     &     'Unit for time related keywords',status)
      call FTPKYS(olun,'TIMEREF','LOCAL',
     &     'Barycentric correction not applied to times',status)

      str1 = 'Xselect V'//xslver(1:1)//'.'//xslver(2:)
      call FTPKYS(olun,'CREATOR',str1,' ',status)

      call FTPKYS(olun,'HDUCLASS','OGIP',
     &     'format conforms to OGIP/GSFC conventions',status)
      call FTPKYS(olun,'HDUCLAS1','GTI',
     &     'File contains Good Time Intervals',status)
      call FTPKYS(olun,'HDUCLAS2','STANDARD',
     &     'File contains Good Time Intervals',status)

      call FTPKYD(olun,'TIMEZERO',0.0d0,10,'Time Zero',status)
      call FTPKYD(olun,'TSTART',tstart(1),10,
     &     'data start time in mission time',status)
      call FTPKYD(olun,'TSTOP',tstop(ngtis),10,
     &     'data start time in mission time',status)

      call FTBDEF(olun,tfields,tform,0,ngtis,status)

      do i=1,ngtis
         call FTPCLD(olun,1,i,1,1,tstart(i),status)
         call FTPCLD(olun,2,i,1,1,tstop(i),status)
      enddo
      if (status.ne.0) then
         call XWRITE('Error writing data to FITS '//
     &        'GTI file',5)
         write(str1,'(''FITSIO error number:'',I3)') status
         call XWRITE(str1,5)
         goto 997
      endif

 997  i = 0
      call ftclos(olun,i)
      call FRELUN(olun)

      return
      end
