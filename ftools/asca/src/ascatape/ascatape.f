CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C TASK: ascatape        Version 2.0
C
C FILE: ascatape.f
C
C DESCRIPTION: ascatape reads files from FITS standard ASCA data
C distribution tapes. Only tapes following the NASA Astrophysics Data Facility
C code 631 FITS tape standard are understandable by this utility.
C
C Original Authors:
C
C     Don Jennings 05/03/94
C
C---------------------------------------------------------------------------
C  MODIFICATION HISTORY:
C
C     04/03/94 D. Jennings, module completed
C     06/20/94 D. Jennings, renamed module to ascatape from readtape.
C     06/20/94 D. Jennings, added code to allow user to see size of
C              files before unloading anything.
C     06/20/94 D. Jennings, added code to build default directories
C              that different file types are placed in.
C     06/21/94 D. Jennings, added a picked file counter; ascatape now
C              reports to the user how many files out of the number
C              picked have been loaded.
C     07/11/94 D. Jennings added gif unwrapping capabilities to the
C              unwrap_file subroutine; assumes that gif is stored
C              in the primary header.
C     07/31/94 D. Jennings made changes in function pick_files to reflect
C              "new" REV1 naming conventions. Now look for "frf.cat",
C              "evt.cat", "unf.cat" and ."dfe" files instead of ".mcat", 
C              ".ccat" and ".cat" files.
C     07/31/94 D. Jennings search options in pick_files routine now look
C              for all name substring matches in lower case as well as
C              upper case where required.
C     08/17/94 D. Jennings, moved all of the sub directory codes to a
C              common block named subdircodes
C     08/17/94 D. Jennings placed the code that determines the subdirectory
C              type of a file into its own subroutine, subdir_sort, so
C              that it may be used by other programs.
C     08/19/94 D. Jennings, added code to handle YET another REV1 file type,
C              the dif file type.
C     10/24/94 D. Jennings, preformed FINAL REV1 file name modifications
C              to the pick_files subroutine
C     10/24/94 D. Jennings, make code Solaris compatable
C     01/06/95 D. Jennings, added support for the new modal config data
C              product.
C     01/06/95 D. Jennings, added support for possible GIF replacement files;
C              ie., jpeg, pds and a generic "pic" file type.
C     09/03/96 Z. Huang and E. Pier 
C              (1) added support for new wrap file names, eg.
C                  *.ps.wrap -> *.ps;
C                  for VMS, *.*.wrap --> *_*.wrap
C              (2) classify a file according to fileclas flag and create
C                  file subdirs dynamically.
C                  One machine dependent subroutine dir_check_create() was
C                  added in utility_unix(vms).f. 
C                  For compatibility, the program also works on old fashioned
C                  tapes with no fileclas column in manifest.fits.
C----------------------------------------------------------------------------
C
C  NOTES:
C
C
C  ARGUMENTS: see the parameter file ascatape.par for a list of parameter
C  arguments accepted by this utility
C
C  This routine may be used as part of the FTOOL ascatape or as a
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
C     fileclas        an array holding file classes.
C     fileclasflag    a flag to check if the manifest.fits file contains
C                     fileclas column        
C
C  CALLED SUBROUTINES:
C
C      mount_drive       mount the tape drive for reading  
C      dismount_drive    dismount the tape drive
C      read_file         read a file from the tape drive
C      dir_verify        make sure the load directory is a valid directory name
C      dir_check_create  check if a sub directory exists, if not creates one
C      get_params        get parameter file values using the HOST interface
C      get_answers       get parameters by asking the user via prompts
C      get_file_names    get the tape file names
C      pick_files        decide what files to load from tape
C      unwrap_file       unwrap a FITS wrapped file
C      subdirs           make subdirectories and sort picked files into them 
C   
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      subroutine ascate
C
C     define variables 
C
      implicit      none
      integer*4     MAXFILES,NUM_SUBDIRS,MAX_LOAD_LIST
      parameter     (MAXFILES=5000)
      parameter     (NUM_SUBDIRS=7)
      parameter     (MAX_LOAD_LIST=21)
C
      character(180) filename
      character(120) new_dir
      character(100) load_dir,tape_drive
      character(80)  namelist(MAXFILES),fileclas(MAXFILES),context
      character(40)  taskname
      character(7)   cunload_kb,cunload_vms_blocks
      character(20)  subdir_names(NUM_SUBDIRS),subdir
      character(4)   cnpicked,ck
      logical*4     loadlist(MAX_LOAD_LIST),picklist(MAXFILES),verbose,
     *              none,unwrap,standalone,check_size,make_dirs,
     *              fileclasflag
      integer*4     status,space,nfiles,i,j,kbytes(MAXFILES),unload_kb,
     *              unload_vms_blocks,dirlist(MAXFILES),subspace,
     *              newspace,npicked,k,slash
      common        /task/ taskname 

C     save the old file classification for very old ASCA tapes
      integer*4     RAW,UNSCREENED,SCREENED,PROD,AUX,WORK,TELEM
      common        /subdircodes/ RAW,UNSCREENED,SCREENED,PROD,
     *                            AUX,WORK,TELEM
C
      external      mount_drive      
      external      dismount_drive
      external      read_file
      external      dir_verify
      external      dir_check_create
C
C     initialize variables
C
      taskname   = 'ascatape2.0'
      verbose    = .false.
      unwrap     = .false.
      none       = .true.
      standalone = .false.
C      standalone = .true.
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
     *        check_size,make_dirs,status)
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
      call get_file_names(namelist,fileclas,kbytes,nfiles,
     *                    load_dir,fileclasflag,verbose,status)
      if(status .ne. 0) goto 999
C
C     decide what files from the tape we need to read off
C     this sub routine should be replaced later on usiong the
C     direct fileclas instead of filename
C
      call pick_files(namelist,picklist,nfiles,loadlist)
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
C     if fileclas column does not exist (fileclasflag == .false.)
C     and the make_dirs parameter was set to true, then try to make
C     the subdirectories and sort the picked files into the right
C     subdirectories.
C     otherwise, create subdirs when necessary
C
      if(.not. fileclasflag .and. make_dirs)then
         call subdirs(namelist,picklist,dirlist,nfiles,
     *                subdir_names,load_dir,status)
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
            status = 0
            if(picklist(i)) then
               k = k + 1
               write(ck,'(I4)')k
               context = 'Loading file ' // ck //' of ' // cnpicked
               call fcecho(context)
               if(make_dirs)then
                  if(fileclasflag) then
C
C                    if the file has its class type, classify it into 
C                    relevant directory, and create it if not exist already;
C                    otherwise, use old scheme
C
                     slash = index(fileclas(i),' ') 
                     if(slash .eq. 0)then
                        slash = len(fileclas(i))
                     else
                        slash = slash - 1
                     endif
                     subdir = fileclas(i)(1:slash)
                     call dir_check_create(load_dir,subdir,status) 
                  else
                     subdir = subdir_names(dirlist(i))
                  endif
                  subspace = index(subdir,' ')
                  if(subspace .eq. 0) then
                     subspace = len(subdir)
                  else
                     subspace = subspace - 1
                  endif
                  new_dir  = load_dir(1:space-1) // subdir(1:subspace)
                  newspace = space + subspace - 1
               else
                  new_dir = load_dir(1:space)
                  newspace = space
               endif
               filename = new_dir(1:newspace) // namelist(i)
               call read_file(filename,newspace,namelist(i),i,
     *                        verbose,status)
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
C     This routine makes the default unloading subdirectories (if not
C     already present) and sorts all of the picked files into the proper
C     subdirectory bins
C
      subroutine subdirs(namelist,picklist,dirlist,nfiles,
     *                   subdir_names,load_dir,status)
C

      implicit      none
      integer*4     MAXFILES,NUM_SUBDIRS
      parameter     (MAXFILES=5000)
      parameter     (NUM_SUBDIRS=7)
C
      character(80)  namelist(MAXFILES),context
      character*(*) subdir_names(NUM_SUBDIRS),load_dir
      integer*4     dirlist(MAXFILES),nfiles,status,i
      integer*4     RAW,UNSCREENED,SCREENED,PROD,AUX,WORK,TELEM
      logical*4     picklist(MAXFILES)
C
      common        /subdircodes/ RAW,UNSCREENED,SCREENED,PROD,
     *                            AUX,WORK,TELEM
C
      external      build_subdirs
      external      subdir_sort
C
      status = 0
C
C     sort each picked file into its appropiate subdirectory bin
C
      do i = 1, nfiles
         if(picklist(i))then
	    call subdir_sort(namelist(i),dirlist(i),status)
            if(status .ne. 0)then
               context = 'unable to sort file into subdir'
               call fcerr(context)
               return
            endif
         endif
      enddo
C      
C     call system dependent routine to build subdirectories
C
      call build_subdirs(load_dir,subdir_names,status)
C
C     return to calling program
C
      return
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
      do while(j .gt. 0 .and. filename(j:j) .ne. '_'
     *                  .and. filename(j:j) .ne. '.')
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
C     This routine gets all the parameters associated with the ascatape
C     utility by asking the user.
C
      subroutine get_answers(tape_drive,load_dir,loadlist,verbose,
     *                      unwrap,check_size,make_dirs,status)
C
      implicit      none
      character*(*) tape_drive,load_dir
      character(80)  context
      character(1)   answer
      logical*4     loadlist(21),verbose,unwrap,check_size,make_dirs
      integer*4     status
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
      write(6,*)'Load all files from tape (y/n):'
      read(5,15)answer
      if(answer .eq. 'y')then
         loadlist(1) = .true.
      else
         write(6,*)'Load minimum data set necessary for analysis (y/n):'
         read(5,15)answer
         if(answer .eq. 'y')then
            loadlist(2) = .true.
         else
            write(6,*)'Load minimum science data files (y/n):'
            read(5,15)answer
            if(answer .eq. 'y')loadlist(3) = .true.
            write(6,*)'Load Mkfilter files (y/n):'
            read(5,15)answer
            if(answer .eq. 'y') loadlist(4) = .true.
         endif
         write(6,*)'Load raw event and catalog files (y/n):'
         read(5,15)answer
         if(answer .eq. 'y') loadlist(19) = .true.
         write(6,*)'Load unscreened events/catalogs (y/n):'
         read(5,15)answer
         if(answer .eq. 'y') loadlist(20) = .true.
         write(6,*)'Load screened events/catalogs (y/n):'
         read(5,15)answer
         if(answer .eq. 'y') loadlist(21) = .true.
         write(6,*)'Load necessary auxiliary data set (y/n):'
         read(5,15)answer
         if(answer .eq. 'y') loadlist(18) = .true.
         write(6,*)'Load necessary science data product set (y/n):'
         read(5,15)answer
         if(answer .eq. 'y') loadlist(5) = .true.
         write(6,*)'Load optional auxiliary and product data (y/n):'
         read(5,15)answer
         if(answer .eq. 'y')then
            loadlist(6) = .true.
            write(6,*)'Load housekeeping files (y/n):'
            read(5,15)answer
            if(answer .eq. 'y') loadlist(7) = .true.
            write(6,*)'Load FITS wrapped text files (y/n):'
            read(5,15)answer
            if(answer .eq. 'y') loadlist(8) = .true.
            write(6,*)'Load calibration data files (y/n):'
            read(5,15)answer
            if(answer .eq. 'y') loadlist(9) = .true.
            write(6,*)'Load attitude files (y/n):'
            read(5,15)answer
            if(answer .eq. 'y') loadlist(10) = .true.
            write(6,*)'Load processing related files (y/n):'
            read(5,15)answer
            if(answer .eq. 'y') loadlist(11) = .true.
         endif
         write(6,*)'Load original telemetry data set(y/n):'
         read(5,15)answer
         if(answer .eq. 'y')loadlist(12) = .true.
         write(6,*)'Load enhanced Mkfilter file (y/n):'
         read(5,15)answer
         if(answer .eq. 'y')loadlist(15) = .true.
         write(6,*)'Load SIS0 data files (y/n):'
         read(5,15)answer
         if(answer .eq. 'y') loadlist(13) = .true.
         write(6,*)'Load SIS1 data files (y/n):'
         read(5,15)answer
         if(answer .eq. 'y') loadlist(14) = .true.
         write(6,*)'Load GIS2 files (y/n):'
         read(5,15)answer
         if(answer .eq. 'y') loadlist(16) = .true.
         write(6,*)'Load GIS3 files (y/n):'
         read(5,15)answer
         if(answer .eq. 'y') loadlist(17) = .true.
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
         if(loadlist(1))then
            context = 'loading all file sets from tape'
            write(6,*)context
         else
            if(loadlist(2))then
               context = 'loading minimum science data set files'
               write(6,*)context
            else
               if(loadlist(3))then
                  context = 'loading science data files'
                  write(6,*)context
               endif
               if(loadlist(4))then
                  context = 'loading Mkfilter files'
                  write(6,*)context
               endif
            endif
            if(loadlist(18))then
               context = 'loading necessary auxiliary data set'
               write(6,*)context
            endif
            if(loadlist(19))then
               context = 'loading raw event list and catalogs'
               write(6,*)context
            endif
            if(loadlist(20))then
               context = 'loading unscreened events '
     *              // 'and catalogs'
               write(6,*)context
            endif
            if(loadlist(21))then
               context = 'loading screened events '
     *              // 'and catalogs'
               write(6,*)context
            endif
            if(loadlist(5))then
               context = 'loading necessary science product set'
               write(6,*)context
            endif
            if(loadlist(7))then
               context = 'loading housekeeping data files'
               write(6,*)context
            endif
            if(loadlist(8))then
               context = 'loading FITS wrapped data files'
               write(6,*)context
            endif
            if(loadlist(9))then
               context = 'loading calibration data files'
               write(6,*)context
            endif
            if(loadlist(10))then
               context = 'loading attitude files'
               write(6,*)context
            endif
            if(loadlist(11))then
               context = 'loading processing related data files'
               write(6,*)context
            endif
            if(loadlist(12))then
               context = 'loading original telemetry files'
               write(6,*)context
            endif
            if(loadlist(15))then
               context = 'loading enhanced Mkfilter files'
               write(6,*)context
            endif
            if(loadlist(13))then
               context = 'loading SIS0 instrument data files'
               write(6,*)context
            endif
            if(loadlist(14))then
               context = 'loading SIS1 instrument data files'
               write(6,*)context
            endif
            if(loadlist(16))then
               context = 'loading GIS2 instrument data files'
               write(6,*)context
            endif
            if(loadlist(17))then
               context = 'loading GIS3 instrument data files'
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
C     This routine gets all the parameters associated with the ascatape
C     utility using the HOST parameter interface.
C
      subroutine get_params(tape_drive,load_dir,loadlist,verbose,
     *                      unwrap,check_size,make_dirs,status)
C
      implicit      none
      character*(*) tape_drive,load_dir
      character(80)  context
      logical*4     loadlist(21),verbose,unwrap,check_size,make_dirs
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
      call uclgsb('load_all',loadlist(1),status)
      if (status .ne. 0) then
         context = 'could not get load_all parameter'
         call fcerr(context)
         goto 999
      endif
      if(loadlist(1)) goto 1000
      call uclgsb('load_min',loadlist(2),status)
      if (status .ne. 0) then
         context = 'could not get load_min parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_sci',loadlist(3),status)
      if (status .ne. 0) then
         context = 'could not get load_sci parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_filters',loadlist(4),status)
      if (status .ne. 0) then
         context = 'could not get load_filters parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_raw',loadlist(19),status)
      if (status .ne. 0) then
         context = 'could not get load_raw parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_unscreened',loadlist(20),status)
      if (status .ne. 0) then
         context = 'could not get load_unscreened parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_screened',loadlist(21),status)
      if (status .ne. 0) then
         context = 'could not get load_screened parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_aux',loadlist(18),status)
      if (status .ne. 0) then
         context = 'could not get load_aux parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_prod',loadlist(5),status)
      if (status .ne. 0) then
         context = 'could not get load_prod parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_hc',loadlist(7),status)
      if (status .ne. 0) then
         context = 'could not get load_hc parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_wrapped',loadlist(8),status)
      if (status .ne. 0) then
         context = 'could not get load_wrapped parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_calib',loadlist(9),status)
      if (status .ne. 0) then
         context = 'could not get load_calib parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_att',loadlist(10),status)
      if (status .ne. 0) then
         context = 'could not get load_att parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_proc_rel',loadlist(11),status)
      if (status .ne. 0) then
         context = 'could not get load_proc_rel parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_optional',loadlist(6),status)
      if (status .ne. 0) then
         context = 'could not get load_optional parameter'
         call fcerr(context)
         goto 999
      endif
      if(loadlist(6))then
         if(.not. loadlist(7))then
            call uclgsb('load_hc1',loadlist(7),status)
            if (status .ne. 0) then
               context = 'could not get load_hc parameter'
               call fcerr(context)
               goto 999
            endif
         endif
         if(.not. loadlist(8))then
            call uclgsb('load_wrapped1',loadlist(8),status)
            if (status .ne. 0) then
               context = 'could not get load_paper parameter'
               call fcerr(context)
               goto 999
            endif
         endif
         if(.not. loadlist(9))then
            call uclgsb('load_calib1',loadlist(9),status)
            if (status .ne. 0) then
               context = 'could not get load_calib parameter'
               call fcerr(context)
               goto 999
            endif
         endif
         if(.not. loadlist(10))then
            call uclgsb('load_att1',loadlist(10),status)
            if (status .ne. 0) then
               context = 'could not get load_att parameter'
               call fcerr(context)
               goto 999
            endif
         endif
         if(.not. loadlist(11))then
            call uclgsb('load_proc_rel1',loadlist(11),status)
            if (status .ne. 0) then
               context = 'could not get load_proc_rel parameter'
               call fcerr(context)
               goto 999
            endif
         endif
      endif
      call uclgsb('load_tel',loadlist(12),status)
      if (status .ne. 0) then
         context = 'could not get load_tel parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_imkf',loadlist(15),status)
      if (status .ne. 0) then
         context = 'could not get load_imkf parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_sis0',loadlist(13),status)
      if (status .ne. 0) then
         context = 'could not get load_sis0 parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_sis1',loadlist(14),status)
      if (status .ne. 0) then
         context = 'could not get load_sis1 parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_gis2',loadlist(16),status)
      if (status .ne. 0) then
         context = 'could not get load_gis2 parameter'
         call fcerr(context)
         goto 999
      endif
      call uclgsb('load_gis3',loadlist(17),status)
      if (status .ne. 0) then
         context = 'could not get load_gis3 parameter'
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
            if(loadlist(1))then
               context = 'loading all file sets from tape'
               call fcecho(context)
            else
               if(loadlist(2))then
                  context = 'loading minimum science data set'
                  call fcecho(context)
               else
                  if(loadlist(3))then
                     context = 'loading raw science data files'
                     call fcecho(context)
                  endif
                  if(loadlist(4))then
                     context = 'loading Mkfilter files'
                     call fcecho(context)
                  endif
               endif       
               if(loadlist(19))then
                  context = 'loading raw event and catalog files'
                  call fcecho(context)
               endif
               if(loadlist(20))then
                  context = 'loading unscreened event and catalog files'
                  call fcecho(context)
               endif
               if(loadlist(21))then
                  context = 'loading screened event and catalog files'
                  call fcecho(context)
               endif
               if(loadlist(18))then
                  context = 'loading necessary auxiliary files'
                  call fcecho(context)
               endif
               if(loadlist(5))then
                  context = 'loading necessary science product files'
                  call fcecho(context)
               endif
               if(loadlist(7))then
                  context = 'loading housekeeping data files'
                  call fcecho(context)
               endif
               if(loadlist(8))then
                  context = 'loading FITS wrapped data files'
                  call fcecho(context)
               endif
               if(loadlist(9))then
                  context = 'loading calibration data files'
                  call fcecho(context)
               endif
               if(loadlist(10))then
                  context = 'loading attitude files'
                  call fcecho(context)
               endif
               if(loadlist(11))then
                  context = 'loading processing related data files'
                  call fcecho(context)
               endif
               if(loadlist(12))then
                  context = 'loading original telmetry files'
                  call fcecho(context)
               endif
               if(loadlist(13))then
                  context = 'loading SIS0 instrument data files'
                  call fcecho(context)
               endif
               if(loadlist(14))then
                  context = 'loading SIS1 instrument data files'
                  call fcecho(context)
               endif
               if(loadlist(15))then
                  context = 'loading enhanced Mkfilter files'
                  call fcecho(context)
               endif
               if(loadlist(16))then
                  context = 'loading GIS2 instrument data files'
                  call fcecho(context)
               endif
               if(loadlist(17))then
                  context = 'loading GIS3 instrument data files'
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
C     conforms to the NASA Code 631 FITS tape standard for ASCA tapes
C
      subroutine get_file_names(namelist,fileclas,kbytes,nfiles,
     *                          load_dir,fileclasflag,verbose,status)
C
      implicit      none
      integer*4     MAXFILES
      parameter     (MAXFILES=5000)
      character*(*) namelist(MAXFILES),fileclas(MAXFILES)
      character*(*) load_dir
      character(180) filename,stmp
      character(80)  context
      character(5)   sfiles
      character(1)   blank
      integer*4     status,space,col1,col2,hdutype,funit,nfiles,i,j,
     *              orderlist(MAXFILES),jtmp,blksize,kbytes(MAXFILES),
     *              col3,col4,null
      logical*4     dummy,ooo,verbose,fileclasflag
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
      call read_file(filename,space,' ',1,verbose,status)
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
      call ftgcno(funit,.false.,'FILENUM',col1,status)
      if(status .ne. 0)then
         context = 'Unable to find FILENUM column in FITS extension 1'
         call fcerr(context)
         goto 999
      endif
      call ftgcno(funit,.false.,'FILENAME',col2,status)
      if(status .ne. 0)then
         context = 'Unable to find FILENAME column in FITS extension 1'
         call fcerr(context)
         goto 999
      endif
      call ftgcno(funit,.false.,'FILESIZE',col3,status)
      if(status .ne. 0)then
         context = 'Unable to find FILESIZE column in FITS extension 1'
         call fcerr(context)
         goto 999
      endif
      call ftgcno(funit,.false.,'FILECLAS',col4,status)
      if(status .ne. 0)then
         context = 'Unable to find FILECLAS column in FITS extension 1,
     *using old file classification scheme.'
         call fcecho(context)
         fileclasflag = .false.
         status = 0
      else
         fileclasflag = .true.
      endif
      call ftgkyj(funit,'NAXIS2',nfiles,context,status)
      if(status .ne. 0)then
         context = 'Unable to find NAXIS keyword in FITS extension 1'
         call fcerr(context)
         goto 999
      endif
      call ftgcvj(funit,col1,1,1,nfiles,null,orderlist,dummy,status)
      if(status .ne. 0)then
         context = 'Unable to extract filenums in FITS extension 1'
         call fcerr(context)
         goto 999
      endif
      call ftgcvs(funit,col2,1,1,nfiles,blank,namelist,dummy,status)
      if(status .ne. 0)then
         context = 'Unable to extract filenames in FITS extension 1'
         call fcerr(context)
         goto 999
      endif
      call ftgcvj(funit,col3,1,1,nfiles,null,kbytes,dummy,status)
      if(status .ne. 0)then
         context = 'Unable to extract file sizes in FITS extension 1'
         call fcerr(context)
         goto 999
      endif
      if(fileclasflag) then
         call ftgcvs(funit,col4,1,1,nfiles,blank,fileclas,dummy,status)
         if(status .ne. 0)then
            context = 'Unable to extract fileclass in FITS extension 1'
            call fcerr(context)
            goto 999
         endif
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
      subroutine pick_files(namelist,picklist,nfiles,loadlist)
C
      implicit      none
      integer*4     MAXFILES
      parameter     (MAXFILES=5000)

      character*(*) namelist(MAXFILES)
      integer*4     nfiles,i
      logical*4     picklist(MAXFILES),loadlist(21)
C
C     Initialize i, the do loop counter, to zero, to kill compiler warning
C                   (KM, 2001 December)
C
      i = 0
C
C     begin the pick process
C
      if(loadlist(1))then
         do i = 1,nfiles
            picklist(i) = .true.
         end do
         goto 999
      endif
C
C     see if user wants the minimum science data set, basic science data set
C     and/or Xselect filter files
C
      if(loadlist(2))then
C         all files starting with 'fa' or 'ft', ending in '.fits'  or
C         '.mkf' or 'frf.cat' or 'raw.cat' but without 'HK.' and 'hk.' belong 
C         to the minimum science data set
         do i = 1,nfiles
            if(((index(namelist(i),'ft')    .ne. 0  .and.
     *           index(namelist(i),'.fits') .ne. 0) .or.
     *           index(namelist(i),'frf.')  .ne. 0  .or.
     *           index(namelist(i),'raw.')  .ne. 0  .or.
     *           index(namelist(i),'.mkf')  .ne. 0) .and.
     *          index(namelist(i),'HK.')    .eq. 0  .and.      
     *          index(namelist(i),'hk.')    .eq. 0        )
     *        picklist(i) = .true.
C            the following checks for catalog files that used the REV0 
C            naming scheme
            if(index(namelist(i),'.cat')    .ne. 0 .and.
     *         index(namelist(i),'ad')      .ne. 0 .and.
     *         index(namelist(i),'unf.cat') .eq. 0 .and.
     *         index(namelist(i),'evt.cat') .eq. 0      )
     *        picklist(i) = .true.
         end do
      else
C          load just the basic science data set
         if(loadlist(3))then
            if(.not. picklist(i))then
C            all files beginning with 'fa' or 'ft' and ending in '.fits'
C            or 'frf.cat' or 'raw.cat' but without 'HK.' and 'hk.' belong to 
C            the basic science data set
               do i = 1,nfiles
                  if((index(namelist(i),'ft')    .ne. 0   .and.
     *                index(namelist(i),'.fits') .ne. 0   .and.
     *                index(namelist(i),'HK.')   .eq. 0   .and.
     *                index(namelist(i),'hk.')   .eq. 0 ) .or.
     *               index(namelist(i),'frf.')   .ne. 0   .or.
     *               index(namelist(i),'raw.')   .ne. 0        )then
                    picklist(i) = .true.
                  else
C                 the following checks for catalog files that used the REV0 
C                 naming scheme
                     if(index(namelist(i),'.cat')    .ne. 0 .and.
     *                  index(namelist(i),'ad')      .ne. 0 .and.
     *                  index(namelist(i),'unf.cat') .eq. 0 .and.
     *                  index(namelist(i),'evt.cat') .eq. 0      )
     *                 picklist(i) = .true.
                  endif
               end do
            endif
         endif
C          load just the Mkfilter data set
         if(loadlist(4))then
C             all files with '.mkf' belong to the Mkfilter
C             filter data set
            do i = 1,nfiles
               if(.not. picklist(i))then
                  if(index(namelist(i),'.mkf')  .ne. 0)
     *                 picklist(i) = .true.
               endif
            end do
         endif
      endif
C 
C     see if the user wants the event list data set
C
      if(loadlist(19))then
         do i = 1,nfiles
            if(.not. picklist(i))then
C     all files with 'ft'+'.fits', and 'frf.cat' or 'raw.cat' belong to the 
C     raw event list file set
               if((index(namelist(i),'ft')      .ne. 0  .and.
     *              index(namelist(i),'.fits')  .ne. 0  .and.
     *              index(namelist(i),'HK')     .eq. 0  .and.
     *              index(namelist(i),'hk')     .eq. 0) .or.
     *              index(namelist(i),'frf.')   .ne. 0  .or.
     *              index(namelist(i),'raw.')   .ne. 0       )
     *           picklist(i) = .true.
C                the following checks for catalog files that used the REV0 
C                naming scheme
               if(index(namelist(i),'.cat')    .ne. 0 .and.
     *            index(namelist(i),'ad')      .ne. 0 .and.
     *            index(namelist(i),'unf.cat') .eq. 0 .and.
     *            index(namelist(i),'evt.cat') .eq. 0      )
     *           picklist(i) = .true.
            end if
         end do
      end if
      if(loadlist(20))then
         do i = 1,nfiles
            if(.not. picklist(i))then
C     all files with 'unf' belong to the unscreened event 
C     list file set
               if(index(namelist(i),'unf') .ne. 0) picklist(i) = .true.
            end if
         end do
      end if
      if(loadlist(21))then
         do i = 1,nfiles
            if(.not. picklist(i))then
C     all files with 'evt' belong to the screened list file set
               if(index(namelist(i),'evt') .ne. 0) picklist(i) = .true.
            end if
         end do
      end if
C
C     see if the user wants the necessary auxiliary data file set
C 
      if(loadlist(18))then
         do i = 1,nfiles
            if(.not. picklist(i))then
C     all files with '.orbit', '.ghf', '.mkf' and 'config.' belong to 
C     the necessary auxiliary file set
               if(index(namelist(i),'.orbit')     .ne. 0 .or.
     *            index(namelist(i),'.mkf')       .ne. 0 .or. 
     *            index(namelist(i),'config.')    .ne. 0 .or. 
     *            index(namelist(i),'.ghf')       .ne. 0      )
     *            picklist(i) = .true.
            endif
         end do
      endif
C
C     see if the user wants the science data products data set
C
      if(loadlist(5))then
         do i = 1,nfiles
            if(.not. picklist(i))then
C               all files with 'pxl','img','detimg', 'gif', 'jpeg.', 'pds.'
C               and 'pic.' extensions belong to the analysis data products set
               if(index(namelist(i),'pxl')     .ne. 0 .or.
     *            index(namelist(i),'img')     .ne. 0 .or.
     *            index(namelist(i),'detimg')  .ne. 0 .or.
     *            index(namelist(i),'jpeg.')   .ne. 0 .or.
     *            index(namelist(i),'pds.')    .ne. 0 .or.
     *            index(namelist(i),'pic.')    .ne. 0 .or.
     *            index(namelist(i),'gif')     .ne. 0     )
     *            picklist(i) = .true.
            endif
         end do
      endif
C
C     see if the user wants any of the 'Optional' data set files
C
C          load the housekeeping data set
      if(loadlist(7))then
C     all files with  'HK' or 'hk' belong to the housekeeping 
C     data set
         do i = 1,nfiles
            if(.not. picklist(i))then
               if(index(namelist(i),'HK')  .ne. 0 .or.
     *            index(namelist(i),'hk')  .ne. 0     )
     *           picklist(i) = .true.
            endif
         end do
      endif
C     load the FITS wrapped products
      if(loadlist(8))then
C     all files with '.wrap' to the FITS wrapped products data set
         do i = 1,nfiles
            if(.not. picklist(i))then
               if(index(namelist(i),'.wrap')     .ne. 0)
     *           picklist(i) = .true.
            endif
         end do
      endif
C     load the calibration data set
      if(loadlist(9))then
C     all files with 'rigid', 'flf', 'ascalin', 'ano', 'ghf' and
C     'leaptable' belong to the calibration data set
         do i = 1,nfiles
            if(.not. picklist(i))then
               if(index(namelist(i),'rigid')       .ne. 0 .or.
     *              index(namelist(i),'flf')       .ne. 0 .or.
     *              index(namelist(i),'ascalin')   .ne. 0 .or.
     *              index(namelist(i),'ano')       .ne. 0 .or.
     *              index(namelist(i),'leaptable') .ne. 0 .or.
     *              index(namelist(i),'ghf')       .ne. 0     )
     *              picklist(i) = .true.
            endif
         end do
      endif
C     load the raw attitude data set
      if(loadlist(10))then
C     all files with 'fa' and but without '.fits' belong
C     to the original attitude data set
         do i = 1,nfiles
            if(.not. picklist(i))then
               if(index(namelist(i),'fa')      .ne. 0 .and.
     *              index(namelist(i),'.fits') .eq. 0       )
     *              picklist(i) = .true.
            endif
         end do
      endif
C     load the processing by-product data set
      if(loadlist(11))then
C     all files with 'joblog', 'hdr_page', 'dfe', 'tape.cat', 'log'
C     'txt', 'ascamode' or 'ascalog'  belong to the processing related data set
         do i = 1,nfiles
            if(.not. picklist(i))then
               if(index(namelist(i),'joblog')     .ne. 0 .or.
     *              index(namelist(i),'job_log')  .ne. 0 .or.
     *              index(namelist(i),'hdr_page') .ne. 0 .or.
     *              index(namelist(i),'tape.cat') .ne. 0 .or.
     *              index(namelist(i),'dfe')      .ne. 0 .or.
     *              index(namelist(i),'txt')      .ne. 0 .or.
     *              index(namelist(i),'ascamode') .ne. 0 .or.
     *              index(namelist(i),'ascalog')  .ne. 0 .or.
     *              index(namelist(i),'log')      .ne. 0     )
     *              picklist(i) = .true.
            endif
         end do
      endif
C
C     load the original telemetry files
C
      if(loadlist(12))then
C          all the files with 'ft' but with out '.fits' belong to the original
C          telemetry file set
         do i = 1,nfiles
            if(.not. picklist(i))then
               if(index(namelist(i),'ft')    .ne. 0 .and.
     *            index(namelist(i),'.fits') .eq. 0      )
     *            picklist(i) = .true.
            endif
         end do
      endif
C
C     load the enhanced mkfilter file
C
      if(loadlist(15))then
C          all the files with ',imkf' belong to the ehnanced mkfilter
C          file sets
         do i = 1,nfiles
            if(.not. picklist(i))then
               if(index(namelist(i),'.imkf') .ne. 0) 
     *              picklist(i) = .true.
            end if
         end do
      end if
C
C     see if the user wants the SIS0 data set
C
      if(loadlist(13))then
C     all files with 'S0', 's0', 'SIS0', and/or 'sis0' minus 'HK.' and 'hk.'
C     belong to the SIS 0 data set
         do i = 1,nfiles
            if(.not. picklist(i))then
               if((index(namelist(i),'S0')   .ne. 0 .or.
     *             index(namelist(i),'s0')   .ne. 0 .or.
     *             index(namelist(i),'SIS0') .ne. 0 .or.
     *             index(namelist(i),'sis0') .ne. 0).and.
     *            index(namelist(i),'HK.')  .eq. 0  .and.
     *            index(namelist(i),'hk.')  .eq. 0       )
     *           picklist(i) = .true.
            endif
         end do
      end if
C
C     See if the user wants SIS1 data set
C
      if(loadlist(14))then
C     all files with 'S1', 's1', 'SIS1', and/or 'sis1' minus 'HK.' and 'hk.'
C     belong to the SIS 1 data set
         do i = 1,nfiles
            if(.not. picklist(i))then
               if((index(namelist(i),'S1')   .ne. 0  .or.
     *             index(namelist(i),'s1')   .ne. 0  .or.
     *             index(namelist(i),'SIS1') .ne. 0  .or.
     *             index(namelist(i),'sis1') .ne. 0) .and.
     *            index(namelist(i),'HK.')   .eq. 0  .and.
     *            index(namelist(i),'hk.')   .eq. 0       )
     *           picklist(i) = .true.
            endif
         end do
      endif
C
C     see if the user wants the GIS2 data set
C
      if(loadlist(16))then
C     all files with 'G2', 'g2', 'GIS2', and/or 'gis2' minus 'HK.' and 'hk.'
C     belong to the GIS 2 data set
         do i = 1,nfiles
            if(.not. picklist(i))then
               if((index(namelist(i),'G2')   .ne. 0  .or.
     *             index(namelist(i),'g2')   .ne. 0  .or.
     *             index(namelist(i),'GIS2') .ne. 0  .or.
     *             index(namelist(i),'gis2') .ne. 0) .and.
     *            index(namelist(i),'HK.')   .eq. 0  .and.
     *            index(namelist(i),'hk.')   .eq. 0       )
     *           picklist(i) = .true.
            endif
         end do
      end if
C
C     see if the user wants the GIS3 data set
C
      if(loadlist(17))then
C     all files with 'G3', 'g3', 'GIS3', and/or 'gis3' minus 'HK.' and 'hk.'
C     belong to the GIS 3 data set
         do i = 1,nfiles
            if(.not. picklist(i))then
               if((index(namelist(i),'G3')   .ne. 0  .or.
     *             index(namelist(i),'g2')   .ne. 0  .or.
     *             index(namelist(i),'GIS2') .ne. 0  .or.
     *             index(namelist(i),'gis2') .ne. 0) .and.
     *            index(namelist(i),'HK.')   .eq. 0  .and.
     *            index(namelist(i),'hk.')   .eq. 0       )
     *           picklist(i) = .true.
            endif
         end do
      endif
C
C     return to calling program
C
 999  return
      end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC



