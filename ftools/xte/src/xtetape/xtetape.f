CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C TASK: xtetape
C
C FILE: xtetape.f
C
C DESCRIPTION: xtetape reads files from FITS standard XTE FITS 
C distribution tapes. Only tapes following the NASA Astrophysics Data Facility
C code 631 FITS tape standard are understandable by this utility.
C
C AUTHOR/DATE:
C
C     Don Jennings 01/XX/95
C
C  MODIFICATION HISTORY:
C
C     12/23/94 D. Jennings, original module code taken from ascatape FTOOL.
C                           Based upon document XFF/94-002.
C     02/18/95 D. Jennings, modified code to reflect changes in XFF 
C                           directory structure (XFF/94-003)
C
C  NOTES:
C
C
C  ARGUMENTS: see the parameter file xtetape.par for a list of parameter
C  arguments accepted by this utility
C
C  This routine may be used as part of the FTOOL xtetape or as a
C  standalone program. Setting the STANDALONE logical will cause the
C  proper code to be used in either case.
C
C  VARIABLE DESCRIPTION:
C
C     MAXFILES        (parameter) maximum number of tape files handled
C     filename        complete name of output filename
C     load_dir        directory to unload selected files
C     tape_drive      name of tape drive to read from
C     namelist        array of tape file names
C     pathlist        array of tape file "standard" XTE paths
C     context         general purpose string
C     taskname        holds name of this utility
C     loadlist        logical array of file types to be unloaded from tape
C     picklist        logical array of files to be unloaded from tape, one
C                     picklist element per tape file
C     dirlist         integer array specifying the subdirectory of the
C                     file
C     verbose         true if status output to screen is desried
C     none            true if user selected no file types to be unloaded
C     unwrap          true if user wants the FITS wrapped files unwrapped
C     status          routine return status code
C     space           string place holder
C     nfiles          number of files on tape
C     npicked         number of files picked for loading
C     i,j,k           general counter variables
C     standalone      true ==> this program is being run as a standalone
C                     routine, false ==> this program is being run as
C                     a FTOOL
C
C  CALLED SUBROUTINES:
C
C      mount_drive    mount the tape drive for reading  
C      dismount_drive dismount the tape drive
C      read_file      read a file from the tape drive
C      dir_verify     make sure the load directory is a valid directory name
C      get_params     get parameter file values using the HOST interface
C      get_answers    get parameters by asking the user via prompts
C      get_file_names get the tape file names
C      pick_files     decide what files to load from tape
C      unwrap_file    unwrap a FITS wrapped file
C      build_xte_db   make XTE/XFF subdirectory structure 
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      subroutine xtetae
C
C     define variables 
C
      implicit      none
C
      integer*4     MAXFILES,MAX_LOAD_LIST,NUMSUBDIRS
      parameter     (MAXFILES=5000)
      parameter     (MAX_LOAD_LIST=17)
      parameter     (NUMSUBDIRS=21)
      integer*4     BASE,ASM,SLEW,PCA,HEXTE,EDS,ACS,ACE,FDS,GSACE,
     *              IPSDU,SPSDU,PSE,IFOG,STDPROD,ORBIT,CLOCK,CAL,
     *              CAL_PCA,CAL_HEXTE,CAL_ASM
      parameter     (BASE=1,ASM=2,SLEW=3,PCA=4,HEXTE=5,EDS=6,ACS=7,
     *               ACE=8,FDS=9,GSACE=10,IPSDU=11,SPSDU=12,PSE=13,
     *               IFOG=14,STDPROD=15,ORBIT=16,CLOCK=17,CAL=18,
     *               CAL_PCA=19,CAL_HEXTE=20,CAL_ASM=21)
C
      character(180) filename
      character(120) new_dir
      character(100) load_dir,tape_drive,pathlist(MAXFILES),subdir
      character(80)  namelist(MAXFILES),context
      character(40)  taskname
      character(7)   cunload_kb,cunload_vms_blocks
      character(4)   cnpicked,ck
      logical*4     loadlist(MAX_LOAD_LIST),picklist(MAXFILES),verbose,
     *              none,unwrap,standalone,check_size,make_dirs
      integer*4     status,space,nfiles,i,j,kbytes(MAXFILES),unload_kb,
     *              unload_vms_blocks,subspace,newspace,npicked,k
      common        /task/ taskname 
      external      mount_drive      
      external      dismount_drive
      external      read_file
      external      dir_verify
      external      build_xte_db
C
C     initialize variables
C
      taskname   = 'xtetape1.0'
      verbose    = .false.
      unwrap     = .false.
      none       = .true.
      standalone = .false.
      npicked    = 0
      k          = 0
      unload_kb  = 0
      do i = 1,MAXFILES
         picklist(i) = .false.
      end do
C
C     get parameter values from parameter file
C
      if(standalone)then
         call get_answers(tape_drive,load_dir,loadlist,verbose,unwrap,
     *                check_size,make_dirs,status)
         else
            call get_params(tape_drive,load_dir,loadlist,verbose,unwrap,
     *                check_size,make_dirs,status)
         endif
      if(status .ne. 0)then
         context = 'parameters missing'
         call fcerr(context)
         goto 998
      endif
C
C     make sure the user specified at least one file catagory to unload
C
      do i = 1,MAX_LOAD_LIST
         if(loadlist(i)) none = .false.
      end do
C       if none = .true. then the user specified no files be loaded
      if(none)then
         context = 'no files have been chosen for loading'
         call fcecho(context)
         goto 998
      endif
C
C     make sure a valid load directory was specified
C
      call dir_verify(load_dir,status)
      if(status .ne. 0)then
         context = 'Invalid load directory specified'
         call fcerr(context)
         goto 998
      endif
C
C     mount tape drive
C
      space = index(tape_drive,' ')
      if(space .ne. 0)then
         space = space - 1
      else
         space = len(tape_drive)
      endif
      call mount_drive(tape_drive(1:space),status)
      if(status .ne. 0)then
         context = 'Could not mount specified tape drive:'
         call fcerr(context)
         call fcerr(tape_drive(1:space))
         goto 998
      endif
C
C     get the file names for each file on tape by reading the first tape
C     file
C
      call get_file_names(namelist,kbytes,pathlist,nfiles,load_dir,
     *                    verbose,status)
      if(status .ne. 0) goto 999
C
C     decide what files from the tape we need to read off
C
      call pick_files(pathlist,namelist,picklist,nfiles,loadlist)
C
C     count up the number of files picked for loading and report it to
C     the user
C
      do i = 1,nfiles
         if(picklist(i)) npicked = npicked + 1
      end do
      write(cnpicked,'(I4)')npicked
      context = 'User selected ' // cnpicked // ' files for loading'
      call fcecho(context)
C
C     if the make_dirs parameter was set to true then make the XTE/XFF
C     subdirectory structure
C
      if(make_dirs .and. .not. check_size)then
         call build_xte_db(load_dir,picklist,pathlist,nfiles,status)
         if(status .ne. 0)then
            context = 'Could not make subdirectories'
            call fcerr(context)
            goto 999
         endif
      endif
C
C     if the check_size parameter is true then just add up the file 
C     sizes and report that value; else unload files from tape
C
      if(check_size)then
         do i = 1,nfiles
            if(picklist(i)) unload_kb = unload_kb + kbytes(i)
         end do
         unload_vms_blocks = unload_kb*2
         write(cunload_kb,'(I7)')unload_kb
         write(cunload_vms_blocks,'(I7)')unload_vms_blocks
         context = 'Selected files require ' // cunload_kb //
     *              ' Kbytes to load'
         call fcecho(context)
         context = 'Selected files require ' // cunload_vms_blocks //
     *             ' VMS blocks to load'
         call fcecho(context)
      else
C
C        read all picked files from the tape 
C
         space = index(load_dir,' ')
         if(space .eq. 0)then 
            space = len(load_dir)
         else   
            space = space - 1
         endif
         do i = 1,nfiles
            if(picklist(i))then
               k = k + 1
               write(ck,'(I4)')k
               context = 'Loading file ' // ck //' of ' // cnpicked
               call fcecho(context)
               if(make_dirs)then
                  subdir = pathlist(i)
                  subspace = index(subdir,' ')
                  if(subspace .eq. 0)then
                     subspace = len(subdir)
                  else
                     subspace = subspace - 1
                  endif
                  if(subdir(subspace:subspace) .ne. '/')then
                     subspace = subspace + 1
                     subdir(subspace:subspace) = '/'
                  endif
                  new_dir  = load_dir(1:space) // subdir(1:subspace)
                  newspace = space + subspace 
               else
                  new_dir  = load_dir(1:space)
                  newspace = space
               endif
               filename = new_dir(1:newspace) // namelist(i)
               call read_file(filename,i,verbose,status)
               if(status .ne. 0)then
                  context = 'Unable to read file from tape:'
                  call fcerr(context)
                  call fcerr(namelist(i))
                  goto 999
               endif
               if(index(namelist(i),'.wrap') .ne. 0 .and. unwrap)then
                  call unwrap_file(namelist(i),new_dir(1:newspace),
     *                             verbose,status)
                  if(status .ne. 0)then
                     context = 'Unable to unwrap file:'
                     call fcerr(context)
                     j = index(filename,' ')
                     if(j .ne. 0)then
                        j = j - 1
                     else
                        j = len(filename)
                     endif
                     call fcerr(filename(1:j))
                  endif
               endif
            endif
         end do    
      endif
C
C     dismount the tape drive
C
 999  if(verbose)then
         context = 'rewinding tape drive'
         call fcecho(context)
      endif
      call dismount_drive(status)
      if(status .ne. 0)then
         context = 'Unable to unmount tape drive'
         call fcerr(context)
      endif
C
C     exit
C
 998  return
      end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     This routine unwraps a FITS wrapped text file, assuming that the
C     texts resides in an FITS table and that there is only 1 extension
C     per FITS file.
C
      subroutine unwrap_file(filename,load_dir,verbose,status)
C
      implicit      none
      integer       ROWBUFF,BUFFSIZE
      parameter     (ROWBUFF=512)
      parameter     (BUFFSIZE=50000)

      character*(*)   filename,load_dir
      character(50000) buff
      character(512)   row
      character(180)   newfile,orgfile
      character(80)    context,newname
      character(1)     cdummy
      integer*4       status,blksize,hdutype,i,nrows,j,space,
     *                nbytes,lun,flun,naxes(10),bitpix,size
      logical*4       dummy,verbose,done
C
      external        create_file
      external        write_data
      external        close_the_file
C
C     initialize variables
C
      flun   = 1
      lun    = 2
      nbytes = 1
      cdummy = char(0)
C
C     open the FITS file
C
      space = index(load_dir,' ')
      if(space .eq. 0)then
         space = len(load_dir)
      else
         space = space - 1
      endif
      orgfile = load_dir(1:space) // filename 
      call ftopen(flun,orgfile,0,blksize,status)
      if(status .ne. 0)then
         context = 'Unable to open FITS file'
         call fcerr(context)
         status = 1
         goto 997
      endif
C
C     construct the unwrapped file name: remove the '.wrap' and add
C     '.unwrapped' or the last '_' seperated segment of the filename 
C
      i = index(filename,'.wrap')
      if(i .eq. 0)then
         context = 'Unable to construct file name for unwrapped file'
         status = 1
         goto 999
      else
         i = i - 1
      endif
      j = i
      do while(j .gt. 0 .and. filename(j:j) .ne. '_')
         j = j - 1
      end do
      if(j .ne. 1)then
         newname = filename(1:j-1) // '.' // filename(j+1:i)
       else
        newname = filename(1:i) // '.unwrapped'
      endif
      newfile = load_dir(1:space) // newname
C
C     if verbose is set, tell user what we are about to do
C
      if(verbose)then
         context = 'Unwrapping file:'
         call fcecho(context)
         j = index(filename,' ')
         if(j .ne. 0)then
            j = j - 1
         else
            j = len(filename)
         endif
         call fcecho(filename(1:j))
         context = 'Unwrapped file name is'
         call fcecho(context)
         j = index(newname,' ')
         if(j .ne. 0)then
            j = j - 1
         else
            j = len(newname)
         endif
         call fcecho(newname(1:j))
      endif
C
C     if the primary array is not empty then we want to unwrap the contents
C     of it, else we want to move to the first extension and unwrap
C 
      call ftghpr(flun,10,dummy,bitpix,nrows,naxes,i,i,dummy,status)
      if(status .ne. 0)then
         context = 'Unable to get primary header keyword values'
         call fcerr(context)
         status = 1
         goto 999
      endif
      if(nrows .eq. 0)then
C
C     move to the table extension
C
         call ftmahd(flun,2,hdutype,status)
         if(status .ne. 0)then
            context = 'Unable to find first table extension'
            call fcerr(context)
            status = 1
            goto 999
         endif
         if(hdutype .ne. 1)then
            context = 'First extension not an ASCII extension'
            call fcerr(context)
            status = 1
            goto 999
         endif
C
C     get NAXIS1 and NAXIS2 keyword values, read data from the table
C
         call ftgkyj(flun,'NAXIS2',nrows,context,status)
         if(status .ne. 0)then
            context = 'Unable to find NAXIS2 keyword'
            call fcerr(context)
            status = 1
            goto 999
         endif
         call ftgkyj(flun,'NAXIS1',size,context,status)
         if(status .ne. 0)then
            context = 'Unable to find NAXIS1 keyword'
            call fcerr(context)
            status = 1
            goto 999
         endif
         if(hdutype .eq. 1 )then
            if(size .gt. ROWBUFF)then
               context = 'table width to great for reading'
               call fcerr(context)
               status = 1
               goto 999
            endif
            call create_file(newfile,lun,hdutype,status)
            if(status .ne. 0)then
               context='Unable to create output file'
               call fcerr(context)
               goto 999
            endif
C     read each row from the table and write it out to the output file
            do i = 1,nrows
                call ftgtbs(flun,i,1,size,row,status)
               if(status .ne. 0)then
                  context = 'Error reading from ASCII table extension'
                  call fcerr(context)
                  status = 1
                  goto 999
               endif
               j = size
               done = .false.
c MJT 15July96 (g77/linux) change to .eqv./.neqv. from .eq./.ne.
c     16Aug  -- treating logicals in a more sensible way
               do while((j .gt. 0) .and. (.not. done))
                  if(row(j:j) .ne. ' ')then
                     done = .true.
                  else
                     j = j - 1
                  endif
               end do
               call write_data(lun,hdutype,row(1:j),status)
               if(status .ne. 0)then
                  context = 'ERROR writing to output file'
                  call fcerr(context)
                  goto 999
               endif
            end do
         endif
      else
C
C     if the wrapped file is stored in the primary array then read the array
C     into a buffer and write it out to the output file
C
         hdutype = 0
         if(bitpix .ne. 8)then
            context='Bad BITPIX value in Primary header, cannot unwrap'
            call fcerr(context)
            goto 999
         endif
         do i = 1,nrows
            nbytes = nbytes*naxes(i)
         end do
         if(nbytes .gt. BUFFSIZE)then
            context='Primary array too large to fit in buffer'
            call fcerr(context)
            goto 999
         endif
         call create_file(newfile,lun,hdutype,status)
         if(status .ne. 0)then
            context='Unable to create output file'
            call fcerr(context)
            goto 999
         endif
         call ftgpvb(flun,1,1,nbytes,cdummy,buff,dummy,status)
         if(status .ne. 0)then
            context = 'Error reading from primary array'
            call fcerr(context)
            status = 1
            goto 999
         endif
         call write_data(lun,hdutype,buff(1:nbytes),status)
         if(status .ne. 0)then
            status = 1
            context = 'ERROR writing to output file'
            call fcerr(context)
            goto 999
         endif
      endif
C
C     close the FITS file and output file
C
 999  call close_the_file(lun,hdutype)
 998  call ftclos(flun,j)
C
C     return to calling program
C
 997  return
C
      end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     This routine gets all the parameters associated with the xtetape
C     utility by asking the user.
C
      subroutine get_answers(tape_drive,load_dir,loadlist,verbose,
     *                      unwrap,check_size,make_dirs,status)
C
      implicit      none
C
      integer*4     MAX_LOAD_LIST
      parameter     (MAX_LOAD_LIST=17)
      integer*4     ALL,PCA,HEXTE,EDS,ACS,ACE,FDS,GSACE,IPSDU,SPSDU,
     *              PSE,IFOG,STDPROD,ORBIT,CLOCK,CAL,INDX
      Parameter     (ALL=1,PCA=2,HEXTE=3,EDS=4,ACS=5,ACE=6,FDS=7,
     *               GSACE=8,IPSDU=9,SPSDU=10,PSE=11,IFOG=12,
     *               STDPROD=13,ORBIT=14,CLOCK=15,CAL=16,INDX=17)
C
      character*(*) tape_drive,load_dir
      character(80)  context
      character(1)   answer
      logical*4     loadlist(MAX_LOAD_LIST),verbose,unwrap,check_size,
     *              make_dirs,status
C
C     get the tape drive name
C
 10   format(A80)
      write(6,*)'Please enter name of tape drive (example /dev/nrst1):'
      read(5,10)tape_drive
C
C     get the load directory parameter
C
      write(6,*)'Please enter name of data directory:'
      read(5,10)load_dir
C
C     get the check_size boolean parameter
C
 15   format(A1)
      write(6,*)'Report on file space requirements only (y/n):'
      read(5,15)answer
      if(answer .eq. 'y')then
         check_size = .true.
         write(6,*)'*** No files execpt for manifest will be loaded'
         write(6,*)'*** size of selected file set will be reported'
      else
         check_size = .false.
      endif
C
C     get the make_dirs boolean parameter
C
      write(6,*)'Make and/or use "defualt" directorys (y/n):'
      read(5,15)answer
      if(answer .eq. 'y')then
         make_dirs = .true.
      else
         make_dirs = .false.
      endif
C
C     get the file loading boolean values
C
      write(6,*)'Load all files from tape? (y/n):'
      read(5,15)answer
      if(answer .eq. 'y')then
         loadlist(ALL) = .true.
      else
         write(6,*)'Load standard products? (y/n):'
         read(5,15)answer
         if(answer .eq. 'y') loadlist(STDPROD) = .true.
         write(6,*)'Load XFF index files? (y/n):'
         read(5,15)answer
         if(answer .eq. 'y') loadlist(INDX) = .true.
         write(6,*)'Load PCA data files? (y/n):'
         read(5,15)answer
         if(answer .eq. 'y') loadlist(PCA) = .true.
         write(6,*)'Load HEXTE data files? (y/n):'
         read(5,15)answer
         if(answer .eq. 'y') loadlist(HEXTE) = .true.
         write(6,*)'Load EDS data files? (y/n):'
         read(5,15)answer
         if(answer .eq. 'y') loadlist(EDS) = .true.
         write(6,*)'Load ACS data files? (y/n):'
         read(5,15)answer
         if(answer .eq. 'y') loadlist(ACS) = .true.
         write(6,*)'Load ACE data files? (y/n):'
         read(5,15)answer
         if(answer .eq. 'y') loadlist(ACE) = .true.
         write(6,*)'Load FDS data files? (y/n):'
         read(5,15)answer
         if(answer .eq. 'y') loadlist(FDS) = .true.
         write(6,*)'Load any of the spacecraft files? (y/n):'
         read(5,15)answer
         if(answer .eq. 'y')then
            write(6,*)'Load GSACE spacecraft files? (y/n):'
            read(5,15)answer
            if(answer .eq. 'y') loadlist(GSACE) = .true.
            write(6,*)'Load ISPDU spacecraft files? (y/n):'
            read(5,15)answer
            if(answer .eq. 'y') loadlist(IPSDU) = .true.
            write(6,*)'Load SPSDU spacecraft files? (y/n):'
            read(5,15)answer
            if(answer .eq. 'y') loadlist(SPSDU) = .true.
            write(6,*)'Load PSE spacecraft files? (y/n):'
            read(5,15)answer
            if(answer .eq. 'y') loadlist(PSE) = .true.
            write(6,*)'Load IFOG spacecraft files? (y/n):'
            read(5,15)answer
         endif
         if(answer .eq. 'y') loadlist(IFOG) = .true.
         write(6,*)'Load orbit data? (y/n):'
         read(5,15)answer
         if(answer .eq. 'y') loadlist(ORBIT) = .true.
         write(6,*)'Load clock data?  (y/n):'
         read(5,15)answer
         if(answer .eq. 'y') loadlist(CLOCK) = .true.
         write(6,*)'Load calibration data? (y/n):'
         read(5,15)answer
         if(answer .eq. 'y') loadlist(CAL) = .true.
      endif
C
C     get the verbose parameter
C
      write(6,*)'Do you want me to show you what I am doing? (y/n):'
      read(5,15)answer
      if(answer .eq. 'y') verbose = .true.
C
C     get the unwrap parameter
C
      write(6,*)'Do you want FITS wrapped files unwrapped? (y/n):'
      read(5,15)answer
      if(answer .eq. 'y') unwrap = .true.
C
C     if verbose was set to .true. then report on the files to
C     extract
C
      if(verbose)then
         if(make_dirs)then
           context = 'making and/or using default Load directories'
           write(6,*)context
         endif
         if(loadlist(ALL))then
            context = 'loading all file sets from tape'
            write(6,*)context
         else
            if(loadlist(INDX))then
               context = 'loading all XFF index files from tape'
               write(6,*)context
            endif
            if(loadlist(PCA))then
               context = 'loading PCA data files'
               write(6,*)context
            endif
            if(loadlist(HEXTE))then
               context = 'loading HEXTE data files'
               write(6,*)context
            endif
            if(loadlist(EDS))then
               context = 'loading EDS data files'
               write(6,*)context
            endif
            if(loadlist(ACS))then
               context = 'loading ACS files'
               write(6,*)context
            endif
            if(loadlist(ACE))then
               context = 'loading ACE files'
               write(6,*)context
            endif
            if(loadlist(FDS))then
               context = 'loading FDS files'
               write(6,*)context
            endif
            if(loadlist(GSACE))then
               context = 'loading GSACE files'
               write(6,*)context
            endif
            if(loadlist(IPSDU))then
               context = 'loading IPSDU files'
               write(6,*)context
            endif
            if(loadlist(SPSDU))then
               context = 'loading SPSDU files'
               write(6,*)context
            endif
            if(loadlist(PSE))then
               context = 'loading PSE files'
               write(6,*)context
            endif
            if(loadlist(IFOG))then
               context = 'loading IFOG files'
               write(6,*)context
            endif
            if(loadlist(STDPROD))then
               context = 'loading processed product files'
               write(6,*)context
            endif
            if(loadlist(ORBIT))then
               context = 'loading orbit data files'
               write(6,*)context
            endif
            if(loadlist(CLOCK))then
               context = 'loading clock data files'
               write(6,*)context
            endif
            if(loadlist(CAL))then
               context = 'loading calibration files'
               write(6,*)context
            endif
         endif
      endif
 999  continue

      return
      end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     This routine gets all the parameters associated with the xtetape
C     utility using the HOST parameter interface.
C
      subroutine get_params(tape_drive,load_dir,loadlist,verbose,
     *                      unwrap,check_size,make_dirs,status)
C
      implicit      none
C
      integer*4     MAX_LOAD_LIST
      parameter     (MAX_LOAD_LIST=17)
      integer*4     ALL,PCA,HEXTE,EDS,ACS,ACE,FDS,GSACE,IPSDU,SPSDU,
     *              PSE,IFOG,STDPROD,ORBIT,CLOCK,CAL,INDX
      Parameter     (ALL=1,PCA=2,HEXTE=3,EDS=4,ACS=5,ACE=6,FDS=7,
     *               GSACE=8,IPSDU=9,SPSDU=10,PSE=11,IFOG=12,
     *               STDPROD=13,ORBIT=14,CLOCK=15,CAL=16,INDX=17)
C
      character*(*) tape_drive,load_dir
      character(80)  context
      logical*4     loadlist(MAX_LOAD_LIST),verbose,unwrap,check_size,
     *              make_dirs
      integer*4     status

C
C     get the tape drive name
C
      call uclgst('tape_drive',tape_drive,status)
      if (status .ne. 0) then
         context = 'could not get tapedrive parameter'
         call fcerr(context)
         goto 999
      endif
C
C     get the load directory parameter
C
      call uclgst('load_dir',load_dir,status)
      if (status .ne. 0) then
         context = 'could not get load_dir parameter'
         call fcerr(context)
         goto 999
      endif
C
C     get the check_size boolean parameter
C
      call uclgsb('check_size',check_size,status)
      if (status .ne. 0) then
         context = 'could not get check_size parameter'
         call fcerr(context)
         goto 999
      endif
C
C     get the make_dirs boolean parameter
C
      call uclgsb('make_dirs',make_dirs,status)
      if (status .ne. 0) then
         context = 'could not get make_dirs parameter'
         call fcerr(context)
         goto 999
      endif
C
C     get the file loading boolean values
C
      call uclgsb('load_all',loadlist(ALL),status)
      if (status .ne. 0) then
         context = 'could not get load_all parameter'
         call fcerr(context)
         goto 999
      endif
      if(loadlist(1)) goto 1000
      call uclgsb('load_index',loadlist(INDX),status)
      if (status .ne. 0) then
         context = 'could not get load_index parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_pca',loadlist(PCA),status)
      if (status .ne. 0) then
         context = 'could not get load_pca parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_hexte',loadlist(HEXTE),status)
      if (status .ne. 0) then
         context = 'could not get load_hexte parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_eds',loadlist(EDS),status)
      if (status .ne. 0) then
         context = 'could not get load_eds parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_acs',loadlist(ACS),status)
      if (status .ne. 0) then
         context = 'could not get load_acs parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_ace',loadlist(ACE),status)
      if (status .ne. 0) then
         context = 'could not get load_ace parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_fds',loadlist(FDS),status)
      if (status .ne. 0) then
         context = 'could not get load_fds parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_gsace',loadlist(GSACE),status)
      if (status .ne. 0) then
         context = 'could not get load_gasce parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_ipsdu',loadlist(IPSDU),status)
      if (status .ne. 0) then
         context = 'could not get load_ipsdu parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_spsdu',loadlist(SPSDU),status)
      if (status .ne. 0) then
         context = 'could not get load_spsdu parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_pse',loadlist(PSE),status)
      if (status .ne. 0) then
         context = 'could not get load_pse parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_ifog',loadlist(IFOG),status)
      if (status .ne. 0) then
         context = 'could not get load_ifog parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_prod',loadlist(STDPROD),status)
      if (status .ne. 0) then
         context = 'could not get load_prod parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_orbit',loadlist(ORBIT),status)
      if (status .ne. 0) then
         context = 'could not get load_orbit parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_clock',loadlist(CLOCK),status)
      if (status .ne. 0) then
         context = 'could not get load_clock parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_cal',loadlist(CAL),status)
      if (status .ne. 0) then
         context = 'could not get load_cal parameter'
         call fcerr(context)
         goto 999
      endif
 1000 continue
C
C     get the verbose parameter
C
      call uclgsb('verbose',verbose,status)
      if (status .ne. 0) then
         context = 'could not get verbose parameter'
         call fcerr(context)
         goto 999
      endif
C
C     get the unwrap parameter
C
      call uclgsb('unwrap',unwrap,status)
      if (status .ne. 0) then
         context = 'could not get unwrap parameter'
         call fcerr(context)
         goto 999
      endif
C
C     if verbose was set to .true. then report on the files to
C     extract
C
      if(verbose)then
         if(check_size)then
           context = '***** Checking selected file set sizes *****'
           call fcecho(context)
           context = '***** Only manifest file will be Loaded *****'
           call fcecho(context)
         else
            if(make_dirs)then
               context = 'making and/or using default loading dirs'
               call fcecho(context)
            endif
            if(loadlist(ALL))then
               context = 'loading all files from tape'
               call fcecho(context)
            else
               if(loadlist(INDX))then
                  context = 'loading all XFF index files from tape'
                  call fcecho(context)
               endif
               if(loadlist(PCA))then
                  context = 'loading all PCA files from tape'
                  call fcecho(context)
               endif
               if(loadlist(HEXTE))then
                  context = 'loading all HEXTE files from tape'
                  call fcecho(context)
               endif
               if(loadlist(EDS))then
                  context = 'loading all EDS files from tape'
                  call fcecho(context)
               endif
               if(loadlist(ACS))then
                  context = 'loading all ACS files from tape'
                  call fcecho(context)
               endif
               if(loadlist(ACE))then
                  context = 'loading all ACE files from tape'
                  call fcecho(context)
               endif
               if(loadlist(FDS))then
                  context = 'loading all FDS files from tape'
                  call fcecho(context)
               endif
               if(loadlist(GSACE))then
                  context = 'loading all GSACE files from tape'
                  call fcecho(context)
               endif
               if(loadlist(IPSDU))then
                  context = 'loading all IPSDU files from tape'
                  call fcecho(context)
               endif
               if(loadlist(SPSDU))then
                  context = 'loading all SPSDU files from tape'
                  call fcecho(context)
               endif
               if(loadlist(PSE))then
                  context = 'loading all PSE files from tape'
                  call fcecho(context)
               endif
               if(loadlist(IFOG))then
                  context = 'loading all IFOG files from tape'
                  call fcecho(context)
               endif
               if(loadlist(STDPROD))then
                  context = 'loading all std. product files from tape'
                  call fcecho(context)
               endif
               if(loadlist(ORBIT))then
                  context = 'loading all orbit files from tape'
                  call fcecho(context)
               endif
               if(loadlist(CLOCK))then
                  context = 'loading all SC clock files from tape'
                  call fcecho(context)
               endif
               if(loadlist(CAL))then
                  context = 'loading all calibration files from tape'
                  call fcecho(context)
               endif
            endif
         endif
      endif
 999  continue      
      return
      end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     read the first file on tape and extract the names and sizes of all the
C     files residing on tape. This function assumes that the mounted tape 
C     conforms to the NASA Code 631 FITS tape standard for data tapes, plus
C     contains a row giving the "standard" relative path of the file.
C
      subroutine get_file_names(namelist,kbytes,pathlist,nfiles,
     *                          load_dir,verbose,status)
C
      implicit      none
      integer*4     MAXFILES
      parameter     (MAXFILES=5000)
      character*(*) namelist(MAXFILES)
      character*(*) pathlist(MAXFILES)
      character*(*) load_dir
      character(180) filename,stmp
      character(80)  context
      character(5)   sfiles
      character(1)   blank
      integer*4     status,space,col1,col2,hdutype,funit,nfiles,i,j,
     *              orderlist(MAXFILES),jtmp,blksize,kbytes(MAXFILES),
     *              col3,null,col4
      logical*4     dummy,ooo,verbose
C
C     initialize variables
C
      funit = 1
      ooo   = .false.
      dummy = .false.
      blank = ' '
      null  = 0
C
C     read the frist file off the tape drive
C
      space = index(load_dir,' ')
      if(space .eq. 0)then
         space = len(load_dir)
      else
         space = space - 1
      endif
      filename = load_dir(1:space) // 'manifest.fits'
      call read_file(filename,1,verbose,status)
      if(status .ne. 0)then
         context = 'Could not read tape manifest file from tape'
         call fcerr(context)
         goto 999
      endif
C
C     use FITSIO calls to read ASCII table from tape manifest file
C
      call ftopen(funit,filename,1,blksize,status)
      if(status .ne. 0)then
         context = 'Unable to open tape manifest file as a FITS file'
         call fcerr(context)
         goto 999
      endif
      call ftmahd(funit,2,hdutype,status)
      if(status .ne. 0)then
         context = 'Unable to access first FITS extension in FITS file'
         call fcerr(context)
         goto 999
      endif
      if(hdutype .ne. 1)then
         context = 'First extension of FITS file not an ASCII table'
         call fcerr(context)
         goto 999
      endif
      call ftgcno(funit,.false.,'FILENAME',col1,status)
      if(status .ne. 0)then
         context = 'Unable to find FILENAME column in FITS extension 1'
         call fcerr(context)
         goto 999
      endif
      call ftgcno(funit,.false.,'FILENUM',col2,status)
      if(status .ne. 0)then
         context = 'Unable to find FILENUM column in FITS extension 1'
         call fcerr(context)
         goto 999
      endif
      call ftgcno(funit,.false.,'FILESIZE',col3,status)
      if(status .ne. 0)then
         context = 'Unable to find FILESIZE column in FITS extension 1'
         call fcerr(context)
         goto 999
      endif
      call ftgcno(funit,.false.,'FILEPATH',col4,status)
      if(status .ne. 0)then
         context = 'Unable to find FILEPATH column in FITS extension 1'
         call fcerr(context)
         goto 999
      endif
      call ftgkyj(funit,'NAXIS2',nfiles,context,status)
      if(status .ne. 0)then
         context = 'Unable to find NAXIS keyword in FITS extension 1'
         call fcerr(context)
         goto 999
      endif
      call ftgcvs(funit,col1,1,1,nfiles,blank,namelist,dummy,status)
      if(status .ne. 0)then
         context = 'Unable to extract filenames in FITS extension 1'
         call fcerr(context)
         goto 999
      endif
      call ftgcvj(funit,col2,1,1,nfiles,null,orderlist,dummy,status)
      if(status .ne. 0)then
         context = 'Unable to extract filenums in FITS extension 1'
         call fcerr(context)
         goto 999
      endif
      call ftgcvj(funit,col3,1,1,nfiles,null,kbytes,dummy,status)
      if(status .ne. 0)then
         context = 'Unable to extract file sizes in FITS extension 1'
         call fcerr(context)
         goto 999
      endif
      call ftgcvs(funit,col4,1,1,nfiles,blank,pathlist,dummy,status)
      if(status .ne. 0)then
         context = 'Unable to extract file paths in FITS extension 1'
         call fcerr(context)
         goto 999
      endif
      call ftclos(funit,status)
C
C     make sure that the elements of the namelist array are in tape
C     position assending order
C
      i = 0
      do while (.not. ooo .and. i .lt. nfiles)
         i = i + 1
         if(orderlist(i) .ne. i) ooo = .true.
      end do
C
C     if the namelist array is not in order then sort it into order with
C     the following bubble sort
C
      if(ooo)then
         do i = 1,nfiles-1
            do j = nfiles,i+1,-1
               if(orderlist(j-1) .gt. orderlist(j))then
                  jtmp           = orderlist(j-1)
                  orderlist(j-1) = orderlist(j)
                  orderlist(j)   = jtmp
                  stmp           = namelist(j-1)
                  namelist(j-1)  = namelist(j)
                  namelist(j)    = stmp
                  stmp           = pathlist(j-1)
                  pathlist(j-1)  = pathlist(j)
                  pathlist(j)    = stmp
               end if
            end do
         end do
      end if
C
C     if in verbose mode, tell user the number of data files found
C
      if(verbose)then
         write(sfiles,'(I4)')nfiles
         context = 'number of files found on tape: ' // sfiles
         call fcecho(context)
      endif
C
C     return to calling program
C
 999  return
      end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     This routine decides, based upon the input parameter list, what
C     files to read off the tape
C
      subroutine pick_files(pathlist,namelist,picklist,nfiles,loadlist)
C
      implicit      none
C
      integer*4     MAX_LOAD_LIST
      parameter     (MAX_LOAD_LIST=17)
      integer*4     MAXFILES
      parameter     (MAXFILES=5000)
      integer*4     ALL,PCA,HEXTE,EDS,ACS,ACE,FDS,GSACE,IPSDU,SPSDU,
     *              PSE,IFOG,STDPROD,ORBIT,CLOCK,CAL,INDX
      Parameter     (ALL=1,PCA=2,HEXTE=3,EDS=4,ACS=5,ACE=6,FDS=7,
     *               GSACE=8,IPSDU=9,SPSDU=10,PSE=11,IFOG=12,
     *               STDPROD=13,ORBIT=14,CLOCK=15,CAL=16,INDX=17)
C
      character*(*) pathlist(MAXFILES)
      character*(*) namelist(MAXFILES)
      integer*4     nfiles,i
      logical*4     picklist(MAXFILES),loadlist(MAX_LOAD_LIST)

C
C     begin the pick process
C
      if(loadlist(ALL))then
         do i = 1,nfiles
            picklist(i) = .true.
         end do
         goto 999
      endif
C
C     see if user wants the XFF index files
C
      if(loadlist(INDX))then
C         all names with 'FMI','FIPC','FIHX','FIED','FICA','FIAC','FIFD',
C         'FISC','FIOE','FICC', or 'FISP' belong to this set
         do i = 1,nfiles
            if(.not. picklist(i))then
               if(index(namelist(i),'FMI')  .ne. 0 .or.
     *            index(namelist(i),'FIPC') .ne. 0 .or.
     *            index(namelist(i),'FIHX') .ne. 0 .or.
     *            index(namelist(i),'FIED') .ne. 0 .or.
     *            index(namelist(i),'FIAC') .ne. 0 .or.
     *            index(namelist(i),'FIAE') .ne. 0 .or.
     *            index(namelist(i),'FIFD') .ne. 0 .or.
     *            index(namelist(i),'FIGS') .ne. 0 .or.
     *            index(namelist(i),'FIIP') .ne. 0 .or.
     *            index(namelist(i),'FISP') .ne. 0 .or.
     *            index(namelist(i),'FIPS') .ne. 0 .or.
     *            index(namelist(i),'FIIG') .ne. 0 .or.
     *            index(namelist(i),'FIOE') .ne. 0 .or.
     *            index(namelist(i),'FICC') .ne. 0 .or.
     *            index(namelist(i),'FICA') .ne. 0 .or.
     *            index(namelist(i),'FIST') .ne. 0)
     *         picklist(i) = .true.
            endif
         enddo
      endif
C
C     see if user wants PCA data files
C
      if(loadlist(PCA))then
C         all pathnames with 'PCA' but without 'CAL' belong to this set
         do i = 1,nfiles
            if(.not. picklist(i))then
               if((index(pathlist(i),'PCA/')     .ne. 0  .or.
     *                 index(pathlist(i),'pca/') .ne. 0) .and.
     *            (index(pathlist(i),'CAL/')     .eq. 0  .or.
     *                 index(pathlist(i),'cal/') .eq. 0))
     *         picklist(i) = .true.
            endif
         enddo
      endif
C 
C     see if the user wants the HEXTE data files
C
      if(loadlist(HEXTE))then
         do i = 1,nfiles
            if(.not. picklist(i))then
C     all pathnames with 'HEXTE' but without 'CAL' belong to this set
               if((index(pathlist(i),'HEXTE/')     .ne. 0  .or.
     *                 index(pathlist(i),'hexte/') .ne. 0) .and.
     *            (index(pathlist(i),'CAL/')       .eq. 0  .or.
     *                 index(pathlist(i),'cal/')   .eq. 0))
     *         picklist(i) = .true.
            endif
         enddo
      endif
C
C     see if the user wants the EDS data files
C
      if(loadlist(EDS))then
         do i = 1,nfiles
            if(.not. picklist(i))then
C     all pathnames with 'EDS' belong to this set
               if(index(pathlist(i),'EDS/') .ne. 0 .or.
     *            index(pathlist(i),'eds/') .ne. 0)
     *         picklist(i) = .true.
            end if
         end do
      end if
C
C     see if the user wants the ACS data files
C
      if(loadlist(ACS))then
         do i = 1,nfiles
            if(.not. picklist(i))then
C     all pathnames with 'ACS' belong to this set
               if(index(pathlist(i),'ACS/') .ne. 0 .or.
     *            index(pathlist(i),'aca/') .ne. 0)
     *         picklist(i) = .true.
            end if
         end do
      end if
C
C     see if the user wants the ACE data files
C
      if(loadlist(ACE))then
         do i = 1,nfiles
            if(.not. picklist(i))then
C     all pathnames with 'ACE' belong to this set
               if(index(pathlist(i),'ACE/') .ne. 0 .or.
     *            index(pathlist(i),'ace/') .ne. 0)
     *         picklist(i) = .true.
            end if
         end do
      end if
C
C     see if the user wants the FDS data files
C
      if(loadlist(FDS))then
         do i = 1,nfiles
            if(.not. picklist(i))then
C     all pathnames with 'FDS' belong to this set
               if(index(pathlist(i),'FDS/') .ne. 0 .or.
     *            index(pathlist(i),'fds/') .ne. 0)
     *         picklist(i) = .true.
            end if
         end do
      end if
C
C     see if the user wants the GSACE data files
C
      if(loadlist(GSACE))then
         do i = 1,nfiles
            if(.not. picklist(i))then
C     all pathnames with 'GSACE' belong to this set
               if(index(pathlist(i),'GSACE/') .ne. 0 .or.
     *            index(pathlist(i),'gsace/') .ne. 0)
     *         picklist(i) = .true.
            end if
         end do
      end if
C
C     see if the user wants the IPSDU data files
C
      if(loadlist(IPSDU))then
         do i = 1,nfiles
            if(.not. picklist(i))then
C     all pathnames with 'IPSDU' belong to this set
               if(index(pathlist(i),'IPSDU/') .ne. 0 .or.
     *            index(pathlist(i),'ipsdu/') .ne. 0)
     *         picklist(i) = .true.
            end if
         end do
      end if
C
C     see if the user wants the SPSDU data files
C
      if(loadlist(SPSDU))then
         do i = 1,nfiles
            if(.not. picklist(i))then
C     all pathnames with 'SPSDU' belong to this set
               if(index(pathlist(i),'SPSDU/') .ne. 0 .or.
     *            index(pathlist(i),'spsdu/') .ne. 0)
     *         picklist(i) = .true.
            end if
         end do
      end if
C
C     see if the user wants the PSE data files
C
      if(loadlist(PSE))then
         do i = 1,nfiles
            if(.not. picklist(i))then
C     all pathnames with 'PSE' belong to this set
               if(index(pathlist(i),'PSE/') .ne. 0 .or.
     *            index(pathlist(i),'pse/') .ne. 0)
     *         picklist(i) = .true.
            end if
         end do
      end if
C
C     see if the user wants the IFOG data files
C
      if(loadlist(IFOG))then
         do i = 1,nfiles
            if(.not. picklist(i))then
C     all pathnames with 'IFOG' belong to this set
               if(index(pathlist(i),'IFOG/') .ne. 0 .or.
     *            index(pathlist(i),'ifog/') .ne. 0)
     *         picklist(i) = .true.
            end if
         end do
      end if
C
C     see if the user wants the STDPROD data files
C
      if(loadlist(STDPROD))then
         do i = 1,nfiles
            if(.not. picklist(i))then
C     all pathnames with 'STDPROD' belong to this set
               if(index(pathlist(i),'STDPROD/') .ne. 0 .or.
     *            index(pathlist(i),'stdprod/') .ne. 0 .or.
     *            index(pathlist(i),'stdProd/') .ne. 0)
     *         picklist(i) = .true.
            end if
         end do
      end if
C
C     see if the user wants the ORBIT data files
C
      if(loadlist(ORBIT))then
         do i = 1,nfiles
            if(.not. picklist(i))then
C     all pathnames with 'ORBIT' belong to this set
               if(index(pathlist(i),'ORBIT/') .ne. 0 .or.
     *            index(pathlist(i),'orbit/') .ne. 0 .or.
     *            index(pathlist(i),'Orbit/') .ne. 0)
     *         picklist(i) = .true.
            end if
         end do
      end if
C
C     see if the user wants the CLOCK data files
C
      if(loadlist(CLOCK))then
         do i = 1,nfiles
            if(.not. picklist(i))then
C     all pathnames with 'CLOCK' belong to this set
               if(index(pathlist(i),'CLOCK/') .ne. 0 .or.
     *            index(pathlist(i),'clock/') .ne. 0 .or.
     *            index(pathlist(i),'Clock/') .ne. 0)
     *         picklist(i) = .true.
            end if
         end do
      end if
C
C     see if the user wants the CAL data files
C
      if(loadlist(CAL))then
         do i = 1,nfiles
            if(.not. picklist(i))then
C     all pathnames with 'CAL' belong to this set
               if(index(pathlist(i),'CAL/') .ne. 0 .or.
     *            index(pathlist(i),'cal/') .ne. 0 .or.
     *            index(pathlist(i),'Cal/') .ne. 0)
     *         picklist(i) = .true.
            end if
         end do
      end if
C
C     return to calling program
C
 999  return
      end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
