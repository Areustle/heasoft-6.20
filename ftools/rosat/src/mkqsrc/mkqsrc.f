*+MKQSRC
C 
C Description:
C  This tool adds new columns and new keywords to the first extension 
C  source table (SRCTBL) of an RDF *_src.fits file in preparation for 
C  quality checking. It writes the expanded table (and all other HDUs 
C  unchanged) into the outfile. It works on either HRI or PSPC data. 
C
C Passed Parameters
C  None
C
C User i/ps required (prompted for):
C  None here, isolated in GPCARF (see below)
C
C Include/Common blocks files
C  common TASK                   : (FTOOLS) standard fatal error message thingy
C 
C Called routines
C     gmkqsrc   - get parameters
C     make_qsrc - add the keywords and columns and write out the new table
C
C Compilation:
C  subroutines require FTOOLS, FITSIO, CALLIB, libraries
C 
C Principal Local Variables:
C     infile - input filename
C     outfile - output filename
C     chatter - how much blithering do you want on your screen?
C     clobber - Overwrite existing output file?
C     status - error condition
C
C Origin:
C  Original
C
C Authors/Modification History:
C  Lawrence E Brown (1.0.0:1994 Sept), based on mkqsrc.pro IDL program by 
C                                      Mike Corcoran
C  Peter D Wilson (1.0.1:1998 June), Updated for new FCPARS behavior
C  Ning Gan (1.0.2:1998 June),  Changed the comment of QC_DATE keyword.
C  Ning Gan (1.0.3:1998 June),  Changed the comment of QC_DATE keyword.
*- 
      subroutine mkqsrc
      implicit none
      
      character(7) version
      parameter (version = '1.0.3')
C     Internals 
      character(40) taskname
      character(160) infile,outfile
      character(160) message
      integer status,chatter
      logical clobber
C     Initialize
      COMMON/task/taskname
      taskname ='MKQSRC '//version
      status = 0
      
      
      
C     Get Parameters from the par file
      call gmkqsrc(infile,outfile,chatter,clobber,status)
      if(status.NE.0) goto 987
      
      call make_qsrc(infile,outfile,chatter,clobber,status)
      if(status.NE.0) goto 987




987      if(status.NE.0) then
        
        message = ' ERROR : '//'Incomplete Execution'
        call fcerr(message)
      else 
         if(chatter.gt.5) then
            message = taskname//' Finished'
            call fcecho(message)
         endif
      endif


      return
      end

*+GMKQSRC
      subroutine gmkqsrc(infile,outfile,chatter,clobber,status)
      implicit none
      character*(*) infile,outfile
      integer status,chatter
      logical clobber
C 
C Description:
C     Gets the parameters for the mkqsrc FTOOL.
C
C Passed Parameters
C     infile - input filename
C     outfile - output filename
C     chatter - how much blithering do you want on your screen?
C     clobber - Overwrite existing output file?
C     status - error condition
C
C Called routines
C  
C Compilation:
C  subroutines require FTOOLS, FITSIO, CALLIB libraries
C 
C Principal Local Variables:
C
C Origin:
C  Original
C
C Authors/Modification History:
C  Lawrence E Brown (1.0.0:1994 Sept)
C  Peter D Wilson (1.0.1:1998 June) Drop INQUIRE call
*- 
      character(160) message,filnam,ill_files(2)
      integer extnum,n_ill
      logical valfil,exst
C Get the name of the i/p  file
      call uclgst('infile',infile, status)
      if(status.NE.0) then
        message = 'Error Getting Input File (infile) parameter'
        call fcerr(message)
        call fcerrm(status)
        return
      endif

      call crmvlbk(infile)
C PDW 6/30/98: Drop INQUIRE call. Call ftrtnm to strip off extension
C      call fcpars(infile,filnam,extnum,status)
      call ftrtnm( infile, filnam, status )
      ill_files(1)=filnam

C      inquire(file=filnam,exist=exst)
C      if((.not.exst).or.(filnam.eq.' '))then
C         message='Input file does not exist: '//filnam
C         status=1
C         return
C      endif

C     since this task doesn't use extension numbers we'll send back
C     the stripped filename
      infile=filnam

      
C Get the name of the o/p  file
      call uclgst('outfile',outfile, status)
      if(status.NE.0) then
        message = 'Error Getting Output File (outfile) parameter'
        call fcerr(message)
        call fcerrm(status)
        return
      endif


      valfil=.true.
      n_ill=1
      call ck_file(outfile,ill_files,n_ill,valfil,clobber,chatter)
      if(.not.valfil) then
         message='Output file is invalid'
         call fcerr(message)
         status=1
         return
      endif


C Get the clobber parameter
      call uclgsb('clobber',clobber,status)
      if(status.ne.0) then
         clobber=.false.
         status=0
         call ftcmsg()
      endif


C Get the chattiness flag
      call uclgsi('chatter',chatter, status)
      if(status.NE.0) then
         message = 'Error getting CHATTER parameter'
         call fcecho(message)
         status = 0 
         message = 'Setting CHATTER = 10'
         call fcecho(message)
         chatter = 10
      endif      

      return
      end
      

*+MAKE_QSRC
      subroutine make_qsrc(infile,outfile,chatter,clobber,status)
      implicit none
      character*(*) infile,outfile
      integer status,chatter
      logical clobber
C 
C Description:
C     Adds 14 columns to a SRCTBL extension for quality checking.
C     Adds new 
C
C Passed Parameters
C     infile - input filename
C     outfile - output filename
C     chatter - how much blithering do you want on your screen?
C     clobber - Overwrite existing output file?
C     status - error condition
C
C User i/ps required (prompted for):
C  None here, isolated in GMKQSRC
C
C Include/Common blocks files
C  common TASK                   : (FTOOLS) standard fatal error message thingy
C 
C Called routines
C  
C Compilation:
C  subroutines require FTOOLS, FITSIO
C 
C Principal Local Variables:
C
C Origin:
C  Original
C
C Authors/Modification History:
C  Lawrence E Brown (1.0.0:1994 Sept)
*- 

      character(40) taskname

      COMMON/task/taskname

C Max arrays
C     we hope the following is big enough
      integer maxsrcs
      parameter (maxsrcs=5000)
      logical tvector(maxsrcs),fvector(maxsrcs)
      character(15) cvector(maxsrcs)
      integer maxcl
      parameter (maxcl=512)
      character(16) ttype(maxcl), tform(maxcl)
      character(25) tunit(maxcl)
C     Local variables
      character(8) keys(14),keynam
      integer iunit,ounit,block,htype,krows,tfields,varidat
      integer oldfields,ikey,width,buffpt,irow,ifield
      integer ist,ien
      character(20) extn,keyval
      character(80) histrec,comment
      character(160) message
      logical copyprime,copyall
C******************************************************************************
C    the following MEM common block definition is in the system iraf77.inc file
C    and is used for dynamic memory allocation 
      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD

C note:
C	datatype	value
C	logical		1
C	integer*2	3
C	Integer		4
C	Long Integer	5
C	Real		6
C	Double		7
C	Complex		8
C******************************************************************************


C     open input file and scroll to SRCTBL extension
      call cgetlun(iunit)
      call ftopen(iunit,infile,0,block,status)
      call ftmahd(iunit,2,htype,status)

      if(status.ne.0) then
         message='Trouble reading '//infile
         call fcerr(message)
         goto 999
      endif

      call ftghbn(iunit,maxcl,krows,tfields,ttype,tform,tunit,
     $     extn,varidat,status)

      if(status.ne.0.or.extn(1:6).ne.'SRCTBL') then
         message='Trouble getting SRCTBL extension header'
         call fcerr(message)
         goto 999
      endif
      call cgetlun(ounit)
      call opfits(ounit,outfile,clobber,chatter,status) 
      if(status.ne.0) then
         message='Trouble opening'//outfile
         call fcerr(message)
         goto 999
      endif

      copyprime=.true.
      copyall=.true.
      call mkqgrab_meaningful_name(infile,ist,ien)
      histrec='Created by mkqsrc from: '//infile(ist:ien)
C     Copy the primary hdu and any others up to the specified one
      call copyfirst(iunit,ounit,1,copyprime,copyall,
     $     .true.,1,histrec,status)

      if(status.ne.0) then
         message='Trouble copying primary FITS header.'
         call fcerr(message)
         goto 999
      endif

C     Make header
      call ftcrhd(ounit,status)
      ttype(tfields+1)='CONFUSED'
      ttype(tfields+2)='DET_STRUCT'
      ttype(tfields+3)='EXTENDED'
      ttype(tfields+4)='FALSE_DET'
      ttype(tfields+5)='INTEN_SUSP'
      ttype(tfields+6)='LTC_SUSP'
      ttype(tfields+7)='MULT_DET'
      ttype(tfields+8)='NOT_CHECKED'
      ttype(tfields+9)='POS_SUSP'
      ttype(tfields+10)='SPEC_SUSP'
      ttype(tfields+11)='VARFLG_SUSP'
      ttype(tfields+12)='WITHIN_EXT'
      ttype(tfields+13)='SOURCE_NAME'
      ttype(tfields+14)='COMMENT'
      tform(tfields+1)='1L'
      tform(tfields+2)='1L'
      tform(tfields+3)='1L'
      tform(tfields+4)='1L'
      tform(tfields+5)='1L'
      tform(tfields+6)='1L'
      tform(tfields+7)='1L'
      tform(tfields+8)='1L'
      tform(tfields+9)='1L'
      tform(tfields+10)='1L'
      tform(tfields+11)='1L'
      tform(tfields+12)='1L'
      tform(tfields+13)='15A'
      tform(tfields+14)='15A'
      oldfields=tfields
      tfields=tfields+14
      call ftphbn(ounit,krows,tfields,ttype,tform,tunit,
     $     extn,varidat,status)
      call ftbdef(ounit,tfields,tform,varidat,krows,status)
      call ftmahd(iunit,2,htype,status)
      if(status.ne.0) then
         message='Trouble making new extension in'//outfile
         call fcerr(message)
         goto 999
      endif

      call xcopyscale(iunit,ounit,status)
C     copy across the comments for the TTYPE and TFORM keywords
      do  5 ifield=1,oldfields
         call ftkeyn('TTYPE',ifield,keynam,status)
         call ftgkey(iunit,keynam,keyval,comment,status)
         call ftmcom(ounit,keynam,comment,status)
         call ftkeyn('TFORM',ifield,keynam,status)
         call ftgkey(iunit,keynam,keyval,comment,status)
         call ftmcom(ounit,keynam,comment,status)
 5    continue
C     copy across the comment for the EXTNAME keyword
      call ftgkey(iunit,'EXTNAME',keyval,comment,status)
      call ftmcom(ounit,'EXTNAME',comment,status)
      do 10 ikey=1,14
         call ftkeyn('TTYPE',oldfields+ikey,keys(ikey),status)
 10   continue
      call ftmcom(ounit,keys(1),
     $     'close sources confused detect',
     $     status)
      call ftmcom(ounit,keys(2),
     $     'source near detector structure',
     $     status)
      call ftmcom(ounit,keys(3),
     $     'source extended',
     $     status)
      call ftmcom(ounit,keys(4),
     $     'false detection',
     $     status)
      call ftmcom(ounit,keys(5),
     $     'source brightness suspect',
     $     status)
      call ftmcom(ounit,keys(6),
     $     'source lightcurve suspect',
     $     status)
      call ftmcom(ounit,keys(7),
     $     'complex emission split',
     $     status)
      call ftmcom(ounit,keys(8),
     $     'source not checked',
     $     status)
      call ftmcom(ounit,keys(9),
     $     'source position suspect',
     $     status)
      call ftmcom(ounit,keys(10),
     $     'source spectrum suspect',
     $     status)
      call ftmcom(ounit,keys(11),
     $     'source variability flag suspect',
     $     status)
      call ftmcom(ounit,keys(12),
     $     'source within extended emission',
     $     status)
      call ftmcom(ounit,keys(13),
     $     'Name of source',
     $     status)
      call ftmcom(ounit,keys(14),
     $     'Source comment',
     $     status)
C     set point to insert new keywords
      call ftkeyn('TFORM',tfields,keynam,status)
C     add new keywords
      call ftgkey(ounit,keynam,keyval,comment,status)
      call ftikyl(ounit,'ASP_SUSP',.false.,
     $     ' aspect solution may be in error (T/F)',
     $     status)
      call ftikyl(ounit,'BKG_SUSP',.false.,
     $     ' incorrect bkg image (T/F)',
     $     status)
      call ftikyl(ounit,'DEFERRED',.false.,
     $     ' quality control deferred (T/F)',
     $     status)
      call ftikyl(ounit,'EXT_EMIS',.false.,
     $     ' extended emission present (T/F)',
     $     status)
      call ftikyl(ounit,'FALS_DET',.false.,
     $     ' 1/more false dets present (T/F)',  
     $     status)
      call ftikyl(ounit,'SRC_MISS',.false.,
     $     ' 1/more real sources missed (T/F)',
     $     status)
      call ftikyl(ounit,'PROD_MIS',.false.,
     $     ' Data product missing (T/F)',
     $     status)
      call ftikyl(ounit,'OTHER',.false.,
     $     ' other problem found (T/F)',
     $     status)
      call ftikys(ounit,'QC_SITE',' ',
     $     ' Site where quality control done',
     $     status)
      call ftikys(ounit,'QC_DATE',' ',
     $     ' Date quality control done (yyyy-mm-dd)',
     $     status)
      call ftikys(ounit,'QC_NAME',' ',
     $     ' Name quality control inspector',
     $     status)
      call ftphis(ounit,
     $     'QC FLAGS STATISTICS:',
     $     status)
      write(histrec,*) 'NUMBER OF SOURCES= ',krows
      call ftphis(ounit,
     $     histrec,
     $     status)
      call ftphis(ounit,
     $     'NUMBER OF TRUE FLAGS BY TYPE:',
     $     status)
      call ftphis(ounit,
     $     'CONFUSED  DET_STRUCT   EXTENDED   FALSE_DET'//
     $     ' INTEN_SUSP  LTC_SUSP  ',
     $     status)
      call ftphis(ounit,
     $     ' ',
     $     status)
      call ftphis(ounit,
     $     'MULT_DET  NOT_CHECKED  POS_SUSP   SPEC_SUSP'//
     $     ' SPEC_SUSP   WITHIN_EXT',
     $     status)
      call ftphis(ounit,
     $     ' ',
     $     status)
      call ftphis(ounit,
     $     'TOTAL TIME IN BACKGROUND LEVELS:',
     $     status)
      call ftphis(ounit,
     $     ' ',
     $     status)
      call ftphis(ounit,
     $     'LOWER BOUNDS OF BACKGROUND LEVELS:',
     $     status)
      call ftphis(ounit,
     $     ' ',
     $     status)
      call ftpcom(ounit,
     $     'NOTE: QUALITY CONTROL FLAGS ARE NOT DEFINITIVE',
     $     status)
      call ftpcom(ounit,
     $     'QC FLAGS ARE MERELY WARNINGS THAT PROBLEMS MAY EXIST',
     $     status)
      call ftpcom(ounit,
     $     'ANY OTHER COMMENTS ABOUT QUALITY OF SOURCES',
     $     status)

      if(status.ne.0) then
         message='Trouble writing or copying keywords'
         call fcerr(message)
         goto 999
      endif


      call ftgkyj(iunit,'NAXIS1',width,comment,status)
      buffpt = 0
      call udmget(width,4,buffpt,status)

      do 20 irow=1,krows
C     copy original columns across
         call ftgtbb(iunit,irow,1,width,MEMI(buffpt),status)
         call ftptbb(ounit,irow,1,width,MEMI(buffpt),status)
C     initialize new column vectors
         fvector(irow)=.false.
         tvector(irow)=.true.
         cvector(irow)='               '
 20   continue
      if(status.ne.0) then
         message='Trouble copying data'
         call fcerr(message)
         goto 999
      endif

C     add new columns
      do 30 ifield=1,7
         call ftpcll(ounit,oldfields+ifield,1,1,krows,fvector,status)
 30   continue
      call ftpcll(ounit,oldfields+8,1,1,krows,tvector,status)
      do 40 ifield=9,12
         call ftpcll(ounit,oldfields+ifield,1,1,krows,fvector,status)
 40   continue
      do 50 ifield=13,14
         call ftpcls(ounit,oldfields+ifield,1,1,krows,cvector,status)
 50   continue

      if(status.ne.0) then
         message='Trouble adding new data columns'
         call fcerr(message)
         goto 999
      endif


      call copylast(iunit,ounit,status)

      if(status.ne.0) then
         message='Trouble copying other extensions'
         call fcerr(message)
         goto 999
      endif


 999  if(status.ne.0) then
         message='Error: Unable to finish'
         call fcerr(message)
         if(chatter.gt.10) then
            call fcerrm(status)
         endif
      endif
      status=0
      call udmfre(buffpt,4,status)
      call ftclos(iunit,status)
      call ftclos(ounit,status)
      return
      end


      subroutine mkqgrab_meaningful_name(filfil,ifs,ife)
C     return the start (ifs) and end (ife) positions of the 
C     characters following the final '/' in a string.  
C     Useful for stripping the pathname off a file specification.
      implicit none
      character*(*) filfil
      integer ifs,ife,i
      do i=160,1,-1
         if(filfil(i:i).ne.' ') goto 10
      enddo
 10   ife=i
      do i=ife,1,-1
         if(filfil(i:i).eq.'/') goto 20
      enddo
      ifs=1
      return
 20   ifs=i+1
      return
      end

