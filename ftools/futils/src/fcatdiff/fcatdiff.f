C*************************************************************************
C SUBROUTINE: 
C       FCATDIFF
C
C FILE: 
C       fcatdiff.f      
C
C DESCRIPTION:
C       This program takes a binary input file, and copies over the keywords
C       given by copyover, and compares each row to the previous row, and 
C       writes out the name and value of the columns given in the list
C       keyword by which they differ. 
C
C AUTHOR/DATE:
C       Jim Ingham 
C
C MODIFICATION HISTORY:
C       8/19/94 3.0a (EAG) - added clobber capability
C      12/26/1995 Jeff Guerber 3.0c - changed all column-name variables to
C         char*40 (ttype, inttype, copyover, kwdnme, exclude), changed the
C         TFORMs to match (also the VALUE tforms), and made dummy args char*(*)
C
C NOTES:
C       
C       
C
C USAGE:
C       call fcatdf 
C       
C ARGUMENTS:
C       
C
C PRIMARY LOCAL VARIABLES:
C       infile  - The input file
C       inext           - The input extension number
C       outfile - The output file
C       copyover        - Vector of column names to be copied over unchanged
C       ncopy           - number of keywords to copy over
C       maxcpy  - the maximum number of copied keywords
C       copycno - The column number of the copyover columns in infile
C       kwdnme  - Vector of keywords to be compared
C       nkwds           - Number of keywords
C       maxkwd  - The maximum number of compared keywords
C       kwdcno  - Their column numbers in infile
C       outcno  - The output column number of the keywords (by type)
C       namecno - The column number for the name column in outfile 
C       fstat           - Error flag
C       
C CALLED ROUTINES:
C       fcdgpar - gets the parameters from the par file 
C       fcdinit - Opens outfile, initializes, transfer copyover
C       fcdcmp  - Compares the keyword columns, transfer to outfile
C
C******************************************************************************
      subroutine fcatdf
      
      integer maxcpy,maxkwd
      parameter ( maxcpy = 20, maxkwd = 300)
      
      character(160) infile, outfile
      character(80) context
      character(40) taskname
      character(40) copyover(maxcpy),kwdnme(maxkwd),exclude(maxcpy)
      integer      copytyp(maxcpy),kwdtyp(maxkwd)
      integer      inext,ncopy,copycno(maxcpy),nkwds,kwdcno(maxkwd)
      integer      nrows,iunit,ounit,fstat,nexclude
      common /task/ taskname
      
      taskname = 'fcatdiff3.0c'

      call ftcmsg

      fstat = 0
      iunit = 15
      ounit = 16
      
C   First read the par file

      call fcdpar(infile,inext,outfile,copyover,ncopy,
     &     exclude,nexclude,kwdnme,nkwds,fstat)
      if(fstat.ne.0) then
         context = 'Error in fcdpar'
         call fcerr(context)
         goto 999
      endif
      
C   Now open the infile and initialize the outfile

      call fcdinit(infile,inext,iunit,outfile,ounit,copyover,
     &     copycno,copytyp,ncopy,
     &     exclude,nexclude,kwdnme,
     &     kwdcno,kwdtyp,nkwds,nrows,fstat)
      if(fstat.ne.0) then
         context = 'Error in fcdinit'
         call fcerr(context)
         goto 999
      endif

C   Now do the compare

      call fcdcmp(iunit,ounit,kwdnme,kwdcno,kwdtyp,nkwds,
     &     copycno,copytyp,ncopy,nrows,fstat)
      if(fstat.ne.0) then
         context = 'Error in fcdcmp'
         call fcerr(context)
         goto 999
      endif

C   Now close the files
 999  fstat = 0
      call ftclos(iunit,fstat)
      call ftclos(ounit,fstat)
      
      return
      end
      
C*************************************************************************
C SUBROUTINE:
C	fcdpar
C
C FILE: 
C	fcatdiff
C
C DESCRIPTION:
C	Reads in the par file
C
C AUTHOR/DATE:
C	Jim Ingham 
C
C MODIFICATION HISTORY:
C
C NOTES:
C	
C	
C
C USAGE:
C	call fcdpar(infile,inext,outfile,copyover,ncopy,
C    &				exclude,nexclude,kwdnme,nkwds,fstat) 
C	
C ARGUMENTS:
C	infile	- The input file
C	inext		- The input extension number
C	outfile	- The output file
C	copyover	- Vector of column names to be copied over unchanged
C	ncopy		- number of keywords to copy over
C	kwdnme	- Vector of keywords to be compared
C	nkwds		- Number of keywords
C	fstat		- Error flag
C
C PRIMARY LOCAL VARIABLES:
C	
C	
C CALLED ROUTINES:
C   	uclgst	- reads a string from the par file	
C	fcpars	- parses filename[ext] into filename, ext
C	fcgcls	- parses a space delimited string into vector
C
C	
C	
C
C******************************************************************************
      subroutine fcdpar(infile,inext,outfile,copyover,ncopy,
     &     exclude,nexclude,kwdnme,nkwds,fstat)  

      integer maxcpy,maxkwd
      parameter ( maxcpy = 20, maxkwd = 300)
      
      character*(*) infile, outfile
      character(160) filenm
      character(80)  context
      character*(*) copyover(maxcpy),kwdnme(maxkwd),exclude(maxcpy)
      integer      inext,ncopy,nkwds,nexclude
      integer      fstat
      logical	 flag
      
      call uclgst('infile',filenm,fstat)
      if(fstat.ne.0) then
         context = 'Could not get infile parameter'
         call fcerr(context)
         goto 999
      endif
      
      call fcpars(filenm,infile,inext,fstat)
      if(fstat.ne.0) then
         context = 'infile has incorrect syntax'
         call fcerr(context)
         goto 999
      endif

C EAG 8/25/93 default to 1st extension
      if (inext .eq. -99) inext = 1

      call uclgst('outfile',outfile,fstat)
      if(fstat.ne.0) then
         context = 'Could not get outfile parameter'
         call fcerr(context)
         goto 999
      endif
      
      call uclgst('copyover',filenm,fstat)
      if(fstat.ne.0) then
         context = 'Could not get copyover parameter'
         call fcerr(context)
         goto 999
      endif
      
      call fcgcls(filenm,copyover,ncopy,flag)
      if(flag) then
         copyover(1) = ' '
         ncopy = 0
      endif

      call uclgst('exclude',filenm,fstat)
      if(fstat.ne.0) then
         context = 'Could not get exclude parameter'
         call fcerr(context)
         goto 999
      endif
      
      call fcgcls(filenm,exclude,nexclude,flag)
      if(flag) then
         exclude(1) = ' '
         nexclude = 0
      endif
      
      call uclgst('keywords',filenm,fstat)
      if(fstat.ne.0) then
         context = 'Could not get keywords parameter'
         call fcerr(context)
         goto 999
      endif
      
      call fcgcls(filenm,kwdnme,nkwds,flag)
      if(flag) then
         nkwds = -1
      endif
      
 999  return
      end
      
C*************************************************************************
C SUBROUTINE:
C	fcdinit
C
C FILE: 
C	fcatdf
C
C DESCRIPTION:
C	This routine opens the input file, initializes the output file,
C	writes the copyover keywords, and gets the column numbers for the
C	keywords.
C
C AUTHOR/DATE:
C	Jim Ingham 
C
C MODIFICATION HISTORY:
C
C NOTES:
C	
C	
C
C USAGE:
C	 call fcdinit(infile,inext,iunit,outfile,ounit,copyover,
C     &            copycno,copytyp,ncopy,
C     &           exclude,nexclude,kwdnme,
C     &		kwdcno,kwdtyp,nkwds,nrows,fstat)
C	
C ARGUMENTS:
C	
C
C PRIMARY LOCAL VARIABLES:
C	
C	
C	
C CALLED ROUTINES:
C	
C	
C
C
C	
C	
C
C******************************************************************************
      subroutine fcdinit(infile,inext,iunit,outfile,ounit,copyover,
     &     copycno,copytyp,ncopy,
     &     exclude,nexclude,kwdnme,
     &     kwdcno,kwdtyp,nkwds,nrows,fstat)
      
      integer maxcpy,maxkwd,maxinc
      parameter ( maxcpy = 20, maxkwd = 300, maxinc = 320)
      character*(*) infile, outfile
      character(80) context,extnme
      character*(*) copyover(maxcpy),kwdnme(maxkwd),exclude(maxcpy)
      character(40) inttype(maxinc), ttype(maxinc)
      character(8)  intform(maxinc), intunit(maxinc)
      character(8)  tform(maxinc), tunit(maxinc)
      integer      inext,ncopy,copycno(maxcpy),nkwds,kwdcno(maxkwd)
      integer      copytyp(maxcpy),kwdtyp(maxkwd)
      integer      nrows,iunit,ounit,fstat,nexclude
      integer      maxlen,repeat,width,varidat
      integer      rwmode,block,morekeys,hdutype,i,j,incols
      logical      exact
      
      rwmode = 0
      block = 0
      morekeys = 0
      exact = .false.
      maxlen = 0
      varidat = 0

      call ftopen(iunit,infile,rwmode,block,fstat)
      if(fstat.ne.0) then
         context = 'Error opening infile'
         call fcerr(context)
         goto 999
      endif
      
      call ffinit(ounit,outfile,fstat)
      if(fstat.ne.0) then
         context = 'Error opening outfile, may exist? ' // outfile
         call fcerr(context)
         goto 999
      endif

      call ftcopy(iunit,ounit,morekeys,fstat)
      if(fstat.ne.0) then
         context = 'Error copying primary header'
         call fcerr(context)
         goto 999
      endif
      
      call ftmahd(iunit,inext + 1,hdutype,fstat)
      if(fstat.ne.0) then
         write(context,50) inext, infile
 50      format('Error moving to extension ',i2,' in ',a30)
         call fcerr(context)
         goto 999
      endif
C   Get the infile header keywords

      call ftghbn(iunit,maxinc,nrows,incols,inttype,intform,intunit,
     &     extnme,varidat,fstat)
      
C   Now create the new extension in the outfile

      call ftcrhd(ounit,fstat)
      
C   Now get the column numbers for the copyover and  
C   keyword columns in infile, and setup header info for outfile

      tform(1) = '1J'
      ttype(1) = 'IN_ROW'
      tunit(1) = ' '

      do 110 i=1,ncopy
         call ftgcno(iunit,exact,copyover(i),copycno(i),fstat)
         if(fstat.ne.0) then
            write(context,55) copyover(i),infile
 55         format('Error: can not find column ',a8,' in ',a30)
            call fcerr(context)
            goto 999
         endif
         tform(i+1) = intform(copycno(i))
         ttype(i+1) = inttype(copycno(i))
         tunit(i+1) = intunit(copycno(i))
         call ftbnfm(tform(i+1),copytyp(i),repeat,width,fstat)
 110  continue
      
C   Now take care of the use all the others case (keywords = "-"):
      if(nkwds.lt.0) then
         nkwds = 0
         do 115 i=1,incols
            do 117 j=1,ncopy
               if(inttype(i).eq.copyover(j)) goto 115
 117        continue
            do 118 j=1,nexclude
               if(inttype(i).eq.exclude(j)) goto 115
 118        continue
            nkwds = nkwds + 1
            kwdnme(nkwds) = inttype(i)
 115     continue
      endif
      
      if(nkwds.le.0) then
         context = 'There are no columns to be compared'
         call fcerr(context)
         fstat = -12
         goto 999
      endif
      
      do 120 i=1,nkwds
         call ftgcno(iunit,exact,kwdnme(i),kwdcno(i),fstat)
         if(fstat.ne.0) then
            write(context,55) kwdnme(i),infile
            call fcerr(context)
            goto 999
         endif
         call ftbnfm(intform(kwdcno(i)),kwdtyp(i),repeat,width,fstat)
 120  continue

C   Now put in the rest of the outfile keywords
      
      tform(ncopy + 2) = '40A'
      ttype(ncopy + 2) = 'PARNAME'
      tunit(ncopy + 2) = ' '
      
      tform(ncopy + 3) = '4A'
      ttype(ncopy + 3) = 'TYPE'
      tunit(ncopy + 3) = ' '
      
      tform(ncopy + 4) = '40A'
      ttype(ncopy + 4) = 'OLDVALUE'
      tunit(ncopy + 4) = ' '

      tform(ncopy + 5) = '40A'
      ttype(ncopy + 5) = 'NEWVALUE'
      tunit(ncopy + 5) = ' '

C   Now define the header for the outfile extension

      extnme = 'CATDIFF'
      call ftphbn(ounit,0,ncopy+5,ttype,tform,tunit,extnme,
     &     varidat,fstat)
      
      call ftpdat(ounit, fstat)
      
 999  return
      end
      
C*************************************************************************
C SUBROUTINE:
C	FCDCMP
C
C FILE: 
C	fcatdiff.f
C
C DESCRIPTION:
C	This routine compares the keyword columns, and writes out a row
C     for each ccolumn that differs.
C	  
C
C AUTHOR/DATE:
C	Jim Ingham 
C
C MODIFICATION HISTORY:
C
C NOTES:
C	
C	
C
C USAGE:
C	call fcdcmp(iunit,ounit,kwdnme,kwdcno,kwdtyp,nkwds,
C     &                            copycno,copytyp,ncopy,nrows,fstat)
C	
C ARGUMENTS:
C	
C
C PRIMARY LOCAL VARIABLES:
C	
C	
C	
C	
C CALLED ROUTINES:
C	
C	
C
C******************************************************************************
      subroutine fcdcmp(iunit,ounit,kwdnme,kwdcno,kwdtyp,nkwds,
     &     copycno,copytyp,ncopy,nrows,fstat) 

      integer maxcpy,maxkwd
      parameter ( maxcpy = 20, maxkwd = 300)
      
      character(40) soldkw(maxkwd),snewkw(maxkwd),scopy(maxcpy),stemp
      character*(*)  kwdnme(maxkwd)
      double precision  roldkw(maxkwd),rnewkw(maxkwd),rcopy(maxcpy)
      integer      ioldkw(maxkwd),inewkw(maxkwd),icopy(maxcpy)
      integer      copytyp(maxcpy),kwdtyp(maxkwd)
      integer      ncopy,copycno(maxcpy),nkwds,kwdcno(maxkwd)
      integer      nrows,iunit,ounit,fstat,i,j,pointer
      logical      change
      
      pointer = 0

C   Read in the first row
      
      call fcdread(maxkwd,iunit,1,nkwds,soldkw,ioldkw,
     &     roldkw,kwdcno,kwdtyp)
      call fcdread(maxcpy,iunit,1,ncopy,scopy,icopy,rcopy,
     &     copycno,copytyp)
C   Write out the first row
      pointer = pointer + 1
      call ftpclj(ounit,1,pointer,1,1,1,fstat)
      call fcdwri(maxcpy,ounit,pointer,ncopy,scopy,
     &     icopy,rcopy,copytyp)
      call ftpcls(ounit,ncopy+2,pointer,1,1,'NONE',fstat)  
      call ftpcls(ounit,ncopy+3,pointer,1,1,'A',fstat)
      call ftpcls(ounit,ncopy+4,pointer,1,1,'NA',fstat)
      call ftpcls(ounit,ncopy+5,pointer,1,1,'NA',fstat)

      do 100 i=2,nrows
         call fcdread(maxkwd,iunit,i,nkwds,snewkw,inewkw,
     &        rnewkw,kwdcno,kwdtyp)
         call fcdread(maxcpy,iunit,i,ncopy,scopy,icopy,rcopy,
     &        copycno,copytyp)
         change = .false.
         do 110 j=1,nkwds
            if(kwdtyp(j).eq.16) then
               if(snewkw(j).ne.soldkw(j)) then
                  pointer = pointer + 1
                  call ftpclj(ounit,1,pointer,1,1,i,fstat)
                  call fcdwri(maxcpy,ounit,pointer,ncopy,scopy,
     &                 icopy,rcopy,copytyp)
                  call ftpcls(ounit,ncopy+2,pointer,1,1,kwdnme(j),fstat)
                  call ftpcls(ounit,ncopy+3,pointer,1,1,'A',fstat)
                  call ftpcls(ounit,ncopy+4,pointer,1,1,soldkw(j),fstat)
                  call ftpcls(ounit,ncopy+5,pointer,1,1,snewkw(j),fstat)
                  change = .true.
                  soldkw(j) = snewkw(j)                    
               endif
            else if(kwdtyp(j).eq.21.or. kwdtyp(j).eq.41) then
               if(inewkw(j).ne.ioldkw(j)) then
                  pointer = pointer + 1
                  call ftpclj(ounit,1,pointer,1,1,i,fstat)
                  call fcdwri(maxcpy,ounit,pointer,ncopy,scopy,
     &                 icopy,rcopy,copytyp)
                  call ftpcls(ounit,ncopy+2,pointer,1,1,kwdnme(j),fstat)
                  call ftpcls(ounit,ncopy+3,pointer,1,1,'I',fstat)
                  write(stemp,50) ioldkw(j)
 50               format(I10)
                  call ftpcls(ounit,ncopy+4,pointer,1,1,stemp,fstat)
                  write(stemp,50) inewkw(j)
                  call ftpcls(ounit,ncopy+5,pointer,1,1,stemp,fstat)
                  change = .true.
                  ioldkw(j) = inewkw(j) 
               endif
            else if(kwdtyp(j).eq.42.or. kwdtyp(j).eq.82) then
               if(rnewkw(j).ne.roldkw(j)) then
                  pointer = pointer + 1
                  call ftpclj(ounit,1,pointer,1,1,i,fstat)
                  call fcdwri(maxcpy,ounit,pointer,ncopy,scopy,
     &                 icopy,rcopy,copytyp)
                  call ftpcls(ounit,ncopy+2,pointer,1,1,kwdnme(j),fstat)
                  call ftpcls(ounit,ncopy+3,pointer,1,1,'E',fstat)
                  write(stemp,60) roldkw(j)
 60               format(G10.4)
                  call ftpcls(ounit,ncopy+4,pointer,1,1,stemp,fstat)
                  write(stemp,60) rnewkw(j)
                  call ftpcls(ounit,ncopy+5,pointer,1,1,stemp,fstat)
                  change = .true.
                  roldkw(j) = rnewkw(j)
               endif
            endif
 110     continue
         if(.not.change) then
            pointer = pointer + 1
            call ftpclj(ounit,1,pointer,1,1,i,fstat)
            call fcdwri(maxcpy,ounit,pointer,ncopy,scopy,
     &           icopy,rcopy,copytyp)
            call ftpcls(ounit,ncopy+2,pointer,1,1,'NONE',fstat)  
            call ftpcls(ounit,ncopy+3,pointer,1,1,'A',fstat)
            call ftpcls(ounit,ncopy+4,pointer,1,1,'NA',fstat)
            call ftpcls(ounit,ncopy+5,pointer,1,1,'NA',fstat)
         endif
 100  continue
      
C   Update the naxis2 keyword
      call ftmkyj(ounit,'NAXIS2',pointer,'&',fstat)
      call ftrdef(ounit,fstat)
      
 999  return
      end
      
C*************************************************************************
C SUBROUTINE:
C	FCDREAD
C
C FILE: 
C	fcatdiff.f
C
C DESCRIPTION:
C	This routine reads in the columns in vcno from the ith row of the 
C     binary fits file iunit.  vtype gives the variable type, and the 
C     values are put in svect, ivect or rvect according to type. 
C	
C
C AUTHOR/DATE:
C	Jim Ingham 
C
C MODIFICATION HISTORY:
C
C NOTES:
C     The internal arrays should be dimensioned to the largest of the
C     two maxkwd or maxcpy.	
C	
C
C USAGE:
C     call fcdread(maxdim,iunit,rowno,ncols,svect,ivect,rvect,vcno,vtype)
C
C	
C ARGUMENTS:
C	
C
C PRIMARY LOCAL VARIABLES:
C	
C	
C CALLED ROUTINES:
C	
C******************************************************************************
      subroutine fcdread(maxdim,iunit,rowno,ncols,svect,ivect,
     &     rvect,vcno,vtype)
      
      integer      maxdim
      character*(*) svect(maxdim)
      character(40)  snull
      double precision rvect(maxdim),rnull
      integer      ivect(maxdim),vtype(maxdim),vcno(maxdim),iunit
      integer      inull,rowno,ncols,fstat,i
      logical      anyf
      
      snull = ' '
      inull = 999
      fstat = 0      
      do 100 i=1,ncols
         if(vtype(i) .eq.16) then
            call ftgcvs(iunit,vcno(i),rowno,1,1,snull,
     &           svect(i),anyf,fstat)
         else if(vtype(i) .eq.21.or.vtype(i).eq.41) then
            call ftgcvj(iunit,vcno(i),rowno,1,1,inull,
     &           ivect(i),anyf,fstat)
         else if(vtype(i) .eq.42.or.vtype(i).eq.82) then
            call ftgcvd(iunit,vcno(i),rowno,1,1,rnull,
     &           rvect(i),anyf,fstat)
         endif
 100  continue
      
      return
      end
      
C*************************************************************************
C SUBROUTINE:
C	FCDWRI
C
C FILE: 
C	fcatdiff.f
C
C DESCRIPTION:
C	This routine writes out the copyover variables
C
C AUTHOR/DATE:
C	Jim Ingham 
C
C MODIFICATION HISTORY:
C
C NOTES:
C     The internal arrays should be dimensioned to the largest of the
C     two maxkwd or maxcpy.	
C	
C
C USAGE:
C     call fcdwri(maxdim,ounit,rowno,ncols,svect,ivect,rvect,vtype)
C
C	
C ARGUMENTS:
C	
C
C PRIMARY LOCAL VARIABLES:
C	
C	
C CALLED ROUTINES:
C	
C******************************************************************************
      subroutine fcdwri(maxdim,ounit,rowno,ncols,svect,
     &     ivect,rvect,vtype)
      
      integer      maxdim,ounit
      character*(*) svect(maxdim)
      double precision rvect(maxdim)
      integer      ivect(maxdim),vtype(maxdim)
      integer      rowno,ncols,fstat,i

      fstat = 0

      do 100 i=1,ncols
         if(vtype(i) .eq.16) then
            call ftpcls(ounit,i+1,rowno,1,1,svect(i),fstat)
         else if(vtype(i) .eq.21.or.vtype(i).eq.41) then
            call ftpclj(ounit,i+1,rowno,1,1,ivect(i),fstat)
         else if(vtype(i) .eq.42.or.vtype(i).eq.82) then
            call ftpcld(ounit,i+1,rowno,1,1,rvect(i),fstat)
         endif
 100  continue      
      return
      end
