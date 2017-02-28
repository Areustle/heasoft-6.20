CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     UTILITY_UNIX: General unix-specific utility library for the ascatape
C     and xtetape utilities. 
C     Unix version 2.0
C
C--------------------------------------------------------------------------
C     Original authors:
C         Don Jennings, Code 631, NASA/HSTX
C         Date:    05/04/94
C--------------------------------------------------------------------------
C     Modifications:
C
C     Date:     [Authors]  descriptions
C     09/03/96  [Z. Huang] Added one subroutine dir_check_create() to check
C                          if a directory exists; if not, creates one;
C                          Modified read_file (), build_subdirs()
C                         
C               note: build_xte_db() is not used.
C----------------------------------------------------------------------
C     version 1.3     
C
C     Author:  Don Jennings, Code 631, NASA/HSTX
C     Date:    05/04/94
C
C     This module contains 11 functions:
C
C       dir_verify     make sure passed string is a valid system directory
C       mount_drive    mount the named tape drive 
C       dismount_drive dismount the named tape drive
C       rewind_drive   rewind the tape in the named tape drive
C       read_file      read the file at position N from the mounted tape drive
C       build_subdirs  make "default" subdirectories for recommended ASCA
C                      data file tape unloads
C       build_xte_db   builds the XTE/XFF subdirectory/database structure
C       create_file    open a new file for writing, depending upon the file
C                      type (ascii or binary)
C       write_data     write a buffer of data to an ascii or binary file
C       close_the_file close a file opened for ascii or binary writes
C       dir_check_create check if a directory exists, 
C                        creates one if necesaary 
C
C     Most of these routines communicate with the tapeio common block. This CB
C     contains the following parameters:
C
C       dp             logical unit number of the opened tape drive
C       fp             logical unit number of the opened output file
C       pos            current tape position (1 = first tape file)
C
C     Some routines use a common block called subdircodes. Subdircodes
C     contains the following information:
C
C     RAW              code for the RAW file class
C     UNSCREENED       code for the UNSCREENED file class
C     SCREENED         code for the SCREENED file class
C     PROD             code for the PROD product file class
C     AUX              code for the AUX auxiliary file class
C     WORK             code for the WORK work area file class
C     TELEM            code for the TELEM telemetry file class
C
C
C     The build_xte_db routine uses the xteparam common block.
C
C  
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     NOTE: this routine does not use the tapeio common block; it is used
C     by the unwrap_file routine only.
C
      subroutine create_file(name,lun,type,status)
C
      implicit      none
      integer       ASCII,BINARY,PRIMARY
      parameter     (PRIMARY=0,ASCII=1,BINARY=2)

      character*(*) name
      integer*4     lun,type,status,createfile
C
      if(type .eq. ASCII)then
         open(lun,file=name,status='unknown',err=999)
      endif
      if(type .eq. BINARY .or. type .eq. PRIMARY)then
         lun = createfile(name)
         if(lun .le. 0) goto 999
      endif
      status = 0
      goto 1000
 999  status = 1
 1000 return
      end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     NOTE: this routine does not use the tapeio common block; it is used
C     by the unwrap_file routine only.
C
      subroutine write_data(lun,type,buff,status)
C
      implicit      none
      integer       ASCII,BINARY,PRIMARY
      parameter     (PRIMARY=0,ASCII=1,BINARY=2)
C
      character*(*) buff
      integer*4     lun,type,size,status,writebuffer
      character(10)  iform
C
      size = len(buff)
      if(type .eq. ASCII)then
         if (size .le. 9)then
            write(iform,1001)size
 1001       format('(A',I1,')')
         else if (size .le. 99)then
            write(iform,1002)size
 1002       format('(A',I2,')')
         else if (size .le. 999)then
            write(iform,1003)size
 1003       format('(A',I3,')')
         else if (size .le. 9999)then
            write(iform,1004)size
 1004       format('(A',I4,')') 
         end if
         write(lun,iform,err=999)buff(1:size)
      endif
      if(type .eq. BINARY .or. type .eq. PRIMARY)then
         status = writebuffer(lun,buff,size)
         if(status .ne. 0) goto 999
      endif
      status = 0
      goto 1000
 999  status = 1
 1000 return
      end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     NOTE: this routine does not use the tapeio common block; it is used
C     by the unwrap_file routine only.
C
      subroutine close_the_file(lun,type)
C
      implicit none
      integer       ASCII,BINARY,PRIMARY
      parameter     (PRIMARY=0,ASCII=1,BINARY=2)
C     
      integer*4 lun,type,status,closefile
C
      if(type .eq. BINARY .or. type .eq. PRIMARY) 
     *                                   status = closefile(lun)
      if(type .eq. ASCII) close(lun)
      return
      end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     NOTE: this routine does not make use of the tapeio common block.
C
C     This routine tries to make the "default" unload directories if not
C     present under the load_dir and returns the names of the subdirectories
C     to the calling routine
C
      subroutine build_subdirs(load_dir,subdir_names,status)
C
      implicit none
      integer*4     MAXFILES,NUM_SUBDIRS
      parameter     (MAXFILES=5000)
      parameter     (NUM_SUBDIRS=7)
C
      character*(*)  load_dir
      character(120)  dir
      character(80)   context
      character*(*)  subdir_names(NUM_SUBDIRS)
      character(20)   subdir
      integer*4      status,i,space,createdirectory,slash
      integer*4      RAW,UNSCREENED,SCREENED,PROD,AUX,WORK,TELEM
C
      common        /subdircodes/ RAW,UNSCREENED,SCREENED,PROD,
     *                            AUX,WORK,TELEM

C
C     initialize the subdirectory names
C
      subdir_names(RAW)         = '/raw/'
      subdir_names(UNSCREENED)  = '/unscreened/'
      subdir_names(SCREENED)    = '/screened/'
      subdir_names(PROD)        = '/product/'
      subdir_names(AUX)         = '/aux/'
      subdir_names(WORK)        = '/work/'
      subdir_names(TELEM)       = '/telem/'

C
C     check to see if the subdirectories exist; if not then create them
C
      space = index(load_dir,' ')
      if(space .eq. 0)then
         space = len(load_dir)
      else
         space = space - 1
      endif
      do i = 1,NUM_SUBDIRS
         subdir = subdir_names(i)
         slash  = index(subdir,' ') 
         if(slash .eq. 0)then
            slash = len(subdir)
         else
            slash = slash - 1
         endif
         dir = load_dir(1:space-1) // subdir(1:slash-1)
         status = createdirectory(dir)
         if(status .ne. 0)then
            call errorlookup(status,context)
            call fcerr(context)
            context = 'Unable to make or access subdirectory '// dir
            call fcerr(context)
            goto 999
         endif
      end do
C
C     return to calling routine
C
 999  return
      end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     NOTE: this routine does not make use of the tapeio common block.
C
C     This routine tries to make the "default" unload directories if not
C     present under the load_dir and returns the names of the subdirectories
C     to the calling routine
C
C     note: this routine needs to be modified since the change of ascatape.f
C           subdir:   */ --> /*/
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine build_xte_db(load_dir,picklist,pathlist,nfiles,status)
C
      implicit none
      integer*4     MAXFILES
      parameter     (MAXFILES=5000)
      integer*4     NUMSUBDIRS
      parameter     (NUMSUBDIRS=21)
      integer*4     BASE,ASM,SLEW,PCA,HEXTE,EDS,ACS,ACE,FDS,GSACE,
     *              IPSDU,SPSDU,PSE,IFOG,STDPROD,ORBIT,CLOCK,CAL,
     *              CAL_PCA,CAL_HEXTE,CAL_ASM
      parameter     (BASE=1,ASM=2,SLEW=3,PCA=4,HEXTE=5,EDS=6,ACS=7,
     *               ACE=8,FDS=9,GSACE=10,IPSDU=11,SPSDU=12,PSE=13,
     *               IFOG=14,STDPROD=15,ORBIT=16,CLOCK=17,CAL=18,
     *               CAL_PCA=19,CAL_HEXTE=20,CAL_ASM=21)
C
      character*(*)  load_dir
      character*(*)  pathlist(MAXFILES)
      character(180)  dir,dir2
      character(120)  tmpstr,tmpstr2
      character(80)   varpath(20)
      character(40)   calpath(20)
      character(80)   context
      character(20)   subdir_names(NUMSUBDIRS),tmpbase
      logical*4      picklist(MAXFILES),done
      integer*4      status,i,space,createdirectory,nfiles,size,
     *               st,ed,j,ncombos,calcombos,k,baseSpace
C
C     initialize the subdirectory names
C
      subdir_names(BASE)        = 'FD/'
      subdir_names(ASM)         = 'ASMData/'
      subdir_names(SLEW)        = 'SlewData/'
      subdir_names(PCA)         = 'pca/'
      subdir_names(HEXTE)       = 'hexte/'
      subdir_names(EDS)         = 'eds/'
      subdir_names(ACS)         = 'acs/'
      subdir_names(ACE)         = 'ace/'
      subdir_names(FDS)         = 'fds/'
      subdir_names(GSACE)       = 'gsace/'
      subdir_names(IPSDU)       = 'ipsdu/'
      subdir_names(SPSDU)       = 'spsdu/'
      subdir_names(PSE)         = 'pse/'
      subdir_names(IFOG)        = 'ifog/'
      subdir_names(STDPROD)     = 'stdprod/'
      subdir_names(ORBIT)       = 'orbit/'
      subdir_names(CLOCK)       = 'clock/'
      subdir_names(CAL)         = 'cal/'
      subdir_names(CAL_PCA)     = 'pca/'
      subdir_names(CAL_HEXTE)   = 'hexte/'
      subdir_names(CAL_ASM)     = 'asm/'
C
C     initalize other variables
C
      ncombos   = 0
      calcombos = 0

C
C     search for all unique combinations of [observation phase, proposal ID
C     obs ID] and [cal era] in the file paths of the selected files
C
      do i = 1,nfiles
C        only consider paths of files that have been picked for unloading
         if(picklist(i)) then
            tmpstr = pathlist(i)
            size   = len(tmpstr)
            st     = index(tmpstr(1:size),'/')
            st     = st + 1
            ed     = st
             j     = 0
C           find the third directory segment of the path
            do while(ed .le. size .and. j .lt. 3)
               if(tmpstr(ed:ed) .eq. '/') j = j + 1
               ed = ed + 1
            end do
            ed = ed - 1
C           if the path does not have at least three path segments then
C           ignore it
            if(j .eq. 3)then
C              if no combos have been found yet accept the first one found
               if(ncombos .eq. 0)then
                  ncombos = 1
                  varpath(ncombos) = tmpstr(st:ed)
               else
C                 add the path segments to the varpath list only if they
C                 do not already belong
                  done = .false.
                  j = 1
                  do while(j .le. ncombos .and. (.not. done))
                     if(index(varpath(j),tmpstr(st:ed)) .ne. 0)then
                        done = .true.
                     endif
                     j = j + 1
                  enddo
                  if(.not. done)then
                     ncombos = ncombos + 1
                     varpath(ncombos) = tmpstr(st:ed)
                  endif
               endif
            endif
C           check for unique calibration directory path segments
            st = index(tmpstr(1:size),subdir_names(CAL))
            if(st .ne. 0)then
               st = st + len(subdir_names(CAL))
               ed = index(tmpstr(st:size),'/')
C              if this is the first calibration directory segment found then
C              add it to the calpath list
               if(calcombos .eq. 0)then
                  calcombos = 1
                  calpath(calcombos) = tmpstr(st:ed)
               else
C                 if the calpath list is non-empty then add the new calibration
C                 path segment only if it is not already in the list
                  done = .false.
                  j = 1
                  do while(j .le. calcombos .and. (.not. done))
                     if(index(calpath(j),tmpstr(st:ed)) .ne. 0)then
                        done = .true.
                     endif
                     j = j + 1
                  enddo
                  if(.not. done) then
                     calcombos = calcombos + 1
                     calpath(calcombos) = tmpstr(st:ed)
                  endif
               endif
            endif
         endif
      enddo
C
C     create the BASE, ASM and SLEW directories, if they do not already exist
C
      baseSpace = index(subdir_names(BASE),' ')
      if(baseSpace .eq. 0)then
         baseSpace = len(subdir_names(BASE))
      else
         baseSpace = baseSpace - 1
      endif
      tmpbase = subdir_names(BASE)
      space = index(load_dir,' ')
      if(space .eq. 0)then
         space = len(load_dir)
      else
         space = space - 1
      endif
      tmpstr = load_dir(1:space) // tmpbase(1:baseSpace)
      status = createdirectory(tmpstr)
      if(status .ne. 0)then
         call errorlookup(status,context)
         call fcerr(context)
         context = 'Unable to make or access subdirectory '// tmpstr
         call fcerr(context)
         goto 999
      endif
      ed = index(tmpstr,' ')
      if(ed .eq. 0)then
         ed = len(tmpstr)
      else
         ed = ed - 1
      endif
      dir = tmpstr(1:ed) // subdir_names(ASM)
      status = createdirectory(dir)
      if(status .ne. 0)then
         call errorlookup(status,context)
         call fcerr(context)
         context = 'Unable to make or access subdirectory '// dir
         call fcerr(context)
         goto 999
      endif
      dir = tmpstr(1:ed) // subdir_names(SLEW)
      status = createdirectory(dir)
      if(status .ne. 0)then
         call errorlookup(status,context)
         call fcerr(context)
         context = 'Unable to make or access subdirectory '// dir
         call fcerr(context)
         goto 999
      endif
C
C     for each of the phase/proposal/obsid directory combinations found,
C     complete the XTE/XFF subdirectory structure
C
      do i = 1,ncombos
C        make the first three directory levels under the BASE
         st = 1
         ed = 1
         tmpstr = varpath(i)
         size = index(tmpstr,' ')
         if(size .eq. 0)then
            size = len(tmpstr)
         else
            size = size - 1
         endif
         do j = 1,3
            do while(tmpstr(ed:ed) .ne. '/' .and. ed .lt. size) 
               ed = ed + 1
            end do
            dir = load_dir(1:space) // tmpbase(1:baseSpace) //
     *                                 tmpstr(1:ed)
            ed = ed + 1
            status = createdirectory(dir)
            if(status .ne. 0)then
               call errorlookup(status,context)
               call fcerr(context)
               context = 'Unable to make or access subdir '// dir
               call fcerr(context)
               goto 999
            endif
         enddo
C        make the rest of the standard XTE/XFF directory structure
         do j = PCA,CAL
            dir = load_dir(1:space) // tmpbase(1:baseSpace) // 
     *            tmpstr(1:size) // subdir_names(j)
            status = createdirectory(dir)
            if(status .ne. 0)then
               call errorlookup(status,context)
               call fcerr(context)
               context = 'Unable to make or access subdir '// dir
               call fcerr(context)
               goto 999
            endif
         end do
C        if any CAL directories were found then build them
         tmpstr2 = subdir_names(CAL)
         ed = index(tmpstr2,' ')
         if(ed .eq. 0)then
            ed = len(tmpstr2)
         else
            ed = ed - 1
         endif
         do j = 1,calcombos
            dir = load_dir(1:space) // tmpbase(1:baseSpace) //
     *            tmpstr(1:size) // tmpstr2(1:ed) // calpath(j)
            status = createdirectory(dir)
            if(status .ne. 0)then
               call errorlookup(status,context)
               call fcerr(context)
               context = 'Unable to make or access '// dir
               call fcerr(context)
               goto 999
            endif
            k = index(dir,' ')
            if(k.eq. 0)then
               k = len(dir)
            else
               size = k - 1
            endif
            dir2 = dir(1:k) // subdir_names(CAL_PCA)
            status = createdirectory(dir2)
            if(status .ne. 0)then
               call errorlookup(status,context)
               call fcerr(context)
               context = 'Unable to make or access subdirectory '// dir2
               call fcerr(context)
               goto 999
            endif
            dir2 = dir(1:k) // subdir_names(CAL_HEXTE)
            status = createdirectory(dir2)
            if(status .ne. 0)then
               call errorlookup(status,context)
               call fcerr(context)
               context = 'Unable to make or access subdirectory '// dir2
               call fcerr(context)
               goto 999
            endif
            dir2 = dir(1:k) // subdir_names(CAL_ASM)
            status = createdirectory(dir2)
            if(status .ne. 0)then
               call errorlookup(status,context)
               call fcerr(context)
               context = 'Unable to make or access subdirectory '// dir2
               call fcerr(context)
               goto 999
            endif
         enddo
      enddo
C
C     return to calling routine
C
 999  return
      end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     This routine makes sure that the passed string load_dir is a valid
C     system directory path
C
      subroutine dir_verify(load_dir,status)
C
      implicit      none
      character*(*) load_dir
      character(80)  context
      integer*4     status,verifytapedrive,space,slash,size,i
C
C     use the tapeio.c function verifyTapeDrive to check for the existance
C     of the load directory; just ignore the -1001 return code, which means
C     that the directory is not a character-special device
C
      status = verifytapedrive(load_dir)
      if(status .ne. 0 .and. status .ne. -1001)then
         context = 'Specified load directory not found'
         call fcerr(context)
      else
C
C     see if the load_dir has a slash (/) appended to the end; if not then
C     append one
C
         status = 0
         space  = index(load_dir,' ')
         size   = len(load_dir)
         slash  = 0
         i      = size
         do while(slash .eq. 0 .and. i .gt. 0)
            if(load_dir(i:i) .eq. '/') slash = i
            i = i - 1
         end do
         if(space .eq. 0)then
            if(slash .ne. size)then
               context = 'load directory not slash (/) terminated'
               call fcerr(context)
               status = 1
            endif
         else
            if(slash .ne. space-1) 
     *           load_dir = load_dir(1:space-1) // '/'
         endif
      endif
C
C     return to the calling program
C
      return
      end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     This routine checks if a given directory exists.
C     If not, creates one 
C     see: dir_verify(), subdirs(), and build_subdir()
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine dir_check_create(load_dir,sub_dir,status)
C
      implicit      none
      character*(*) load_dir,sub_dir
      character(120) dir
      character(80)  context
      integer*4     status,verifytapedrive,createdirectory,space,slash
C
C     use the tapeio.c function verifyTapeDrive to check for the existance
C     of the sub directory; just ignore the -1001 return code, which means
C     that the directory is not a character-special device
C
      space = index(load_dir,' ')
      if(space .eq. 0)then
         space = len(load_dir)
      else
         space = space - 1
      endif
      slash  = index(sub_dir,' ') 
      if(slash .eq. 0)then
         slash = len(sub_dir)
      else
         slash = slash - 1
      endif
C     
C     To conform with the calling routine, add the slash to the end
C     of sub_dir (see the calling routine in ascatape.f)
C
      sub_dir= '/' // sub_dir(1:slash) // '/'
C     
C     load_dir already has a slash at the end
C
      dir = load_dir(1:space-1) // sub_dir(1:slash+1)
      status = verifytapedrive(dir)
C
C     If the directory does not exist, create one
C
      if(status .ne. 0 .and. status .ne. -1001) then
         status = createdirectory(dir)
         if(status .ne. 0)then
            call errorlookup(status,context)
            call fcerr(context)
            context = 'Unable to make or access subdirectory '// dir
            call fcerr(context)
         endif
      endif
      return
      end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     This routine mounts the tape drive named by the drive variable, so
C     that it may be read from
C
      subroutine mount_drive(drive,status)
C
      implicit      none
      character*(*) drive
      character(80)  context
      integer*4     status,fp,dp,pos,opentapedrive,verifytapedrive,
     *              norewind,i,len,size
      logical*4     done
      common        /tapeio/ dp,fp,pos
C
C    make sure a valid device name was passed to this routine
C
      status = verifytapedrive(drive)
      if(status .lt. 0)then
         call errorlookup(status,context)
         call fcerr(context)
         status = 1
         goto 999
      endif
C
C     make sure a no-rewind device was specified
C
      size = len(drive)
      i = size
      done = .false.
      do while(i .gt. 0 .and. .not. done)
         if(drive(i:i) .eq. '/')then
            done = .true.
         else
            i = i - 1
         endif
      end do
      if(i .eq. 0) i = 1         
      norewind = index(drive(i:size),'n')
      if(norewind .eq. 0)then
         context = 'must specify a "no-rewind" tape device'
         call fcerr(context)
         status = 1
         goto 999
      endif
C
C     make call to the tapeio.c routine to open the tape drive for reading
C
      dp = opentapedrive(drive,-1)
      if(dp .le. 0)then
         call errorlookup(dp,context)
         call fcerr(context)
         dp = 0
         status = 1
      endif
C
C     set the current tape position to 1
C
      pos = 1
C
C     return to calling routine
C
 999  return
      end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     This routine dismounted a perviously mounted tape drive
C
      subroutine dismount_drive(status)
C
      implicit      none
      character(80)  context
      integer*4     status,fp,dp,pos,closetapedrive
      common        /tapeio/ dp,fp,pos
C
C     call the tapeio.c routine to close the drive
C
      status = closetapedrive(dp)
      if(status .lt. 0)then
         call errorlookup(status,context)
         call fcerr(context)
         dp = 0
         status = 1
      endif
C
C     return to calling program
C
      return
      end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     This routine rewinds the tape in the currently mounted tape drive
C
      subroutine rewind_drive(status)
C
      implicit     none
      character(80) context
      integer*4    status,fp,dp,pos,rewindtapedrive
      common       /tapeio/ dp,fp,pos
C
      status = rewindtapedrive(dp)
      if(status .lt. 0)then
         call errorlookup(status,context)
         call fcerr(context)
         dp = 0
         status = 1
      else
         pos = 1
      endif
C
C     return to calling program
C
      return
      end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     This routine reads the file at position POSITION and puts its contents
C     into file FILENAME
C     dirlen,shortname is not used for the unix version
C 
      subroutine read_file(filename,dirlen,shortname,position,
     *                     verbose,status)
C
      implicit      none
      character*(*) filename,shortname
      character(80)  context
      character(5)   spos
      integer*4     position,status,dp,fp,pos,direction,skip,skipfiles,
     *              createfile,readfile,closefile,space,dirlen
      logical*4     verbose
      common        /tapeio/ dp,fp,pos
C
C     make sure that the tape drive pointer is defined
C
      if(dp .le. 0)then
         context = 'uninitalized tape drive pointer'
         call fcerr(context)
         status = 1
         goto 999
      endif
C
C     move the tape to the desired file
C
      if(pos .ne. position)then
         if(pos .lt. position)then
            skip      = position - pos
            direction = 1
         else
            skip      = pos - position
            direction = -1
         endif
         status = skipfiles(dp,skip,direction)
         if(status .lt. 0)then
            call errorlookup(status,context)
            call fcerr(context)
            status = 1
            goto 999
         endif
         pos = position
         if(verbose)then
            write(spos,'(I4)')pos
            context = 'moved tape to position ' // spos
            call fcecho(context)
         endif
      endif
C
C     create the output file
C
      fp = createfile(filename)
      if(fp .le. 0)then
         call errorlookup(fp,context)
         call fcerr(context)
         status = 1
         goto 999
      endif
C
C     read the file off the tape
C
      if(verbose)then
         space = index(filename,' ')
         if(space .eq. 0)then
            space = len(filename)
         else
            space = space - 1
         endif
         context = 'Reading file ' // filename(1:space)
         call fcecho(context)
      endif
      status = readfile(dp,fp,28800)
      if(status .lt. 0)then
         call errorlookup(status,context)
         call fcerr(context)
         status = 1
         goto 999
      endif
C
C     since we just read a file, the drive pointer position must be
C     incremented
C
      pos = pos + 1
      if(verbose)then
         write(spos,'(I4)')pos
         context = 'tape position after file read is ' // spos
         call fcecho(context)
      endif
C
C     close the file
C
      status = closefile(fp)
      if(status .lt. 0)then
         call errorlookup(status,context)
         call fcerr(context)
         status = 1
         goto 999
      endif
C
C     return to calling program
C
 999  return
      end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC




