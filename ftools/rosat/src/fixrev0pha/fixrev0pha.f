      subroutine fixrea()
      implicit none
c Description:
c    This tool is meant to be called primarily by Xselect.  The only
C    other occaision for its use would be to correct spectral files
c    created by the extractor (or Xselect) from ROSAT/PSPC US Rev0 data
C    before the release of FTOOLS 3.4.
c
c    All versions of the extractor produce  WMAP extensions  with the wrong 
C    CDELT keywords when used on US Rev0 data. This can result in serious 
C    errors when using such tools as PCARF and PCPARPHA.  These  tools  will
C    warn you if they see suspicious pixel sizes. RDF data  or  German  data 
C    which  has  been  converted  with GEVT2RDF should NOT be processed with 
C    this tool.
c
c    This  tool will check that the file is a ROSAT PSPC file and that
C    it contains more than more than 2 keywords beginning with 'XS-' 
C    (arbitrarily picked for safety, it should contain 41 of them).
C    If so (i.e. US Rev0 data), it will replace the CDELT keywords 
C    in the primary header so that the base pixel size is:
C    
C    2.595021E-4
c
c Passed Parameters
c  None
c
c User i/ps required (prompted for):
c  None here, isolated in GP_fr0p(see below)
c
c Called routines
c  subroutine GP_fr0p     : (below) Gets parameter from par file
c  subroutine DO_fr0p     : (below) Does the rest
c  
c Compilation:
c  subroutines require FTOOLS, FITSIO
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Lawrence E Brown (2.0.0:1995 Sept), based on Perl script fix_rev0_pha
c                        and subroutine gt_wmap.  Added new screening to
c                        decide if Rev0 file
C  Lawrence E Brown (1994 Sept),  Perl script fix_rev0_pha1.0 

      character(7) version
      parameter (version = '2.0.0')
*- 
      character(80) context
      character(160) pha_file
      integer status

c Initialize
      character(40) taskname
      COMMON/task/taskname
      taskname ='fixrev0pha '//version
      
      status = 0
      
c     get filename parameter
      
      call gp_fr0p(pha_file,status)

      if(status.NE.0) goto 999

      call do_fr0p(pha_file,status)
        
 999  if(status.NE.0) then
         context = 'Fixing failed for file: '//pha_file
         call fcerr(context)
      endif
      end

C*****************************************************************************
      subroutine gp_fr0p(pha_file,status)
      implicit none
C SUBROUTINE
C      gp_fr0p
C
C FILE:
C      gp_fr0p.f
C
C DESCRIPTION:
C	Get the parameter from the par file
C	
C
C AUTHOR/DATE:
C       Larry Brown, 9/95
C	Hughes STX
C 
C MODIFICATION HISTORY:
C
C USAGE:
C        call gp_fr0p(pha_file,status)
C   
C NOTES:
C	
C
C ARGUMENTS:
C
C     pha_file - pha_file to be checked and fixed if necessary
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C
C************************************************************
c     Gets parameters

      character(160) pha_file
      integer status

C     Internals
      integer chatter

      character(80) context

C get the terminal chattiness parameter
      call uclgsi ('chatter', chatter, status)
      if (status .ne. 0) then
         context = ' Error getting CHATTER parameter'
         call fcerr (context)
         goto 999
      endif

C set up chattiness of program
      call xchaty(chatter, 0)


      call uclgst('phafile',pha_file, status)
      if(status.NE.0) then
         context = 'Couldn''t get PHAFILE parameter'
         call fcerr(context)
         goto 999
      endif


 999  continue
      return
      end

      
C*****************************************************************************
      subroutine do_fr0p(pha_file,status)
      implicit none
C SUBROUTINE
C      do_fr0p
C
C FILE:
C     gp_fr0p.f
C
C DESCRIPTION:
C     Fix the CDELT keywords in the primary extension of 
C     ROSAT PSPC US Rev0 spectral files produced by the extractor.
C	
C
C AUTHOR/DATE:
C       Larry Brown, 9/95
C	Hughes STX
C 
C MODIFICATION HISTORY:
C
C USAGE:
C        call do_fr0p(pha_file,status)
C   
C NOTES:
C	
C
C ARGUMENTS:
C
C     pha_file - pha_file to be checked and fixed if necessary
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C
C************************************************************

      character(160) pha_file
      integer status

C     Internals

      character(80) context,comment,comment1,comment2, record
      character(8) instrume, hduvers
      integer block, iunit

      integer wmbin, nxs, nkeys, nmore, i, ihdvrs
      double precision cdelt,cdelt1,cdelt2

      character(40) taskname
      COMMON/task/taskname

      
      
      call ftgiou(iunit, status)

      call ftopen(iunit, pha_file, 1, block, status)

      if(status.ne.0) then
         context = 'Couldn''t connect to file: '//pha_file
         call fcerr(context)
         goto 999
      endif
      
C     check for INSTRUME == PSCP

      call ftgkys(iunit,'INSTRUME',instrume,comment,status)
      IF (status.ne.0.or.instrume(1:4).ne.'PSPC') THEN
         status=0
         context = 'Doesn''t appear to be a PSPC file.  Exiting.'
         call xwrite(context,5)
         goto 999
      ENDIF


      nxs = 0
c check for 'XS-' keywords

C find how many keys there are:
      call ftghsp(iunit,nkeys,nmore,status)
      
C loop over all keys
      do i = 1, nkeys
         call ftgrec(iunit,i,record,status)
         if(record(1:3).eq.'XS-') nxs = nxs + 1
         if(nxs.gt.2) goto 100
      enddo

      if(nxs.le.2) then
         context = 'Doesn''t appear to be a US Rev0 file. Exiting.'
         call xwrite(context,5)
         goto 999
      endif

 100  continue

c Get the HDUVERS for the WMAP

      CALL ftgkys(iunit,'HDUVERS',hduvers,comment,status)
      IF ( status .NE. 0 ) THEN
         status = 0
         CALL ftgkys(iunit,'HDUVERS1',hduvers,comment,status)
      ENDIF
      IF ( status .NE. 0 ) THEN
         ihdvrs = 0
         status = 0
      ELSE
         READ(hduvers(1:1),'(i1)') ihdvrs
      ENDIF

c Get the rebin factor in the WMAP. pre HDUVERS=2 this is read from the
c WMREBIN keyword. With HDUVERS=2 it can be read from CDELT1P

      IF ( ihdvrs .LT. 2 ) THEN
         CALL ftgkyj(iunit,'WMREBIN',wmbin,comment,status)
      ELSE
         CALL ftgkyj(iunit,'CDELT1P',wmbin,comment,status)
      ENDIF
      if (status.ne.0) then
         status=0
         context = 
     $        'Can''t find rebinning parameter for WMAP, assuming 15.'
         call xwrite(context,5)
         wmbin=15
      endif

      call ftgkyd(iunit,'CDELT1',cdelt1,comment1,status)
      call ftgkyd(iunit,'CDELT2',cdelt2,comment2,status)

      if(status.ne.0) then
         context = 'Couldn''t get CDELT keywords to fix.'
         call fcerr(context)
         goto 999
      endif

C  US Rev0 data has wrong value in PHA file.
C  Standard value is DELTX=2.595021E-4.

      cdelt=dble(wmbin) * 2.595021d-4
      
      call ftmkyd(iunit,'CDELT1',cdelt,16,comment1,status)
      call ftmkyd(iunit,'CDELT2',cdelt,16,comment2,status)
      
      call timestamp(iunit)

      context = 'CDELT keywords modified by '//taskname
      call ftphis(iunit,context,status)
      
      write(context,'(''Original CDELT1:'',1pe27.16)') cdelt1

      call ftphis(iunit,context,status)

      write(context,'(''Original CDELT2:'',1pe27.16)') cdelt2

      call ftphis(iunit,context,status)

      call ftclos(iunit, status)

 999  if (status.ne.0) then
         context = 'Error fixing pha file'//pha_file
         call fcerr(context)
      endif
      return
      end


