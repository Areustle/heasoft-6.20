
*+RDTAB1
c     --------------------------------------------------------
      subroutine rdtab1(infile,instrume,iymap,max_dim,min,max,
     &            ierr,chatter)
c     --------------------------------------------------------
c --- VARIABLES ------------------------------------------------------
c
        IMPLICIT NONE
        character*(*) infile,instrume
        integer iunit,chatter,max_dim
	integer ierr,min,max
        integer iymap(max_dim,max_dim)
c
c --- DESCRIPTION -----------------------------------------------------
c
c   This subroutine reads a PSPC electronic position corrector map as
c a FITS image.
c
c PASSED PARAMETERS
c
c  iunit        i   : FORTRAN unit number of open ADC file
c  chatter      i   : chattiness flag for o/p (5 quite,10 norm,>19 silly)
c  max_dim      i   : maximum array dimensions
c  iymap        i   : 2-d array containing image
c
c
c AUTHORS/MODIFICATION HISTORY
c
c   Rehana Yusaf (1.0.0: 1995 Sept)
c   Rehana Yusaf (1.0.1: 1995 Oct); bugfix, change ftp2dj to ftg2dj
      character(5) version
      parameter (version = '1.0.1')
*-
c -----------------------------------------------------------------------
c Internals 
      	integer status,naxes(2),inull,block
        logical anyf
	character(30) errstr, wrnstr,comm
      	character(70) subinfo

c Initialization

	status = 0
	errstr = ' RDTAB1'//version//' ERROR:'
	wrnstr = ' RDTAB1'//version//' WARNING:'

c --- USER INFO ---
c
      IF (chatter.GE.20) THEN
         subinfo = ' ... using RDTAB1 Ver '//version
         call fcecho(subinfo)
      ENDIF

c
c --- OPEN OUTFILE ---
c
      call ftgiou(iunit,status)
      call ftopen(iunit,infile,0,block,status)
      subinfo = errstr//' opening infile'
      call wt_ferrmsg(status,subinfo)
      IF (status.NE.0) THEN
        goto 100
      ENDIF 

      status = 0
      call ftgkys(iunit,'INSTRUME',instrume,comm,status)
      subinfo = errstr//' reading instrume keyword'
      call wt_ferrmsg(status,subinfo) 
c
c
c --- READ DATA ---
c
      status = 0
      naxes(1) = 512
      naxes(2) = 512
      call ftg2dj(iunit,0,inull,max_dim,naxes(1),naxes(2),
     &            iymap,anyf,status)
      subinfo = errstr//' reading primary data '
      call wt_ferrmsg(status,subinfo)
      IF (status.NE.0) THEN
        status = 3
      ENDIF

      call ftclos(iunit,status)
      subinfo = errstr//' closing infile'
      call wt_ferrmsg(status,subinfo)
      If (status.NE.0) THEN
       ierr = 2
       goto 100
      endif

      if(chatter.GE.20) then
        subinfo = 
     &  ' ... read the electronic position image'
        call fcecho(subinfo)
      endif

100   if(ierr.NE.0) then
	  subinfo = errstr // 'FATAL: Extension not read'
	  call fcecho(subinfo)
	endif
      return
      end
c --------------------------------------------------------------------
c     END OF RDTAB1
c --------------------------------------------------------------------   
