*+PPTHNM
        subroutine ppthnm(path, disk, dir, file)
        implicit none
        character*(*) path, disk, dir, file

C-----------------------------------------------------------------------
C Description: Parses a path string into its disk, directory and
C              filename parts.  The directory name is returned with '/'
C              characters separating the directory names.  For example,
C              if the VMS version of this routine is used, and the path
C              passed to PPTHNM was DISK:[DIR1.DIR2]FILE.TXT, then
C              disk = 'DISK', dir = 'dir1/dir2', and file = 'file.txt'
C              would be returned.  The UNIX version of this program
C              requires that the first character of the pathname be '/'.
C              In addition, the UNIX version will return the first
C              directory after the leading '/' as the disk.  For
C              example, if the path passed to PPTHNM was
C              /dir1/dir2/dir3/file.txt, then disk = 'dir1', 
C              dir = 'dir2/dir3', and file = 'file.txt' would be
C              returned.
C
C Arguments:   path  (i): the pathname to be parsed
C              disk  (r): the diskname of the path
C              dir   (r): the directory part of the path
C              file  (r): the filename part of the path
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C              Ron Zellar (1993 June 10) Original Version
C
C-----------------------------------------------------------------------
*-Version 1.0

        integer i, j, pthlen, dirlen, fcstln, index

C       get actual path length of path
        pthlen = fcstln(path)

C       find the ':'.  everything before it is disk
        i = index(path,':')
        disk = path(1:i-1)

C       if the path has no filename at the end, set file to ' ' and skip
C       the do loop
        if(path(pthlen:pthlen).eq.']')then
             file = ' '
             goto 200
        endif

C       look for the end of the dir name.  Everything after it is file
        do 100 j=0,pthlen-1
          if(path(pthlen-j:pthlen-j).eq.']')then
               file = path(pthlen-j+1:pthlen)
               goto 200
          endif
100     continue
200     continue

C       everything between disk:[... and ...]file is dir
        dir = path(i+2:pthlen-j-1)

C       change all '.' to '/' in dir
        dirlen = fcstln(dir)
        do 300 j=1,dirlen
             if(dir(j:j).eq.'.')then
                  dir(j:j) = '/'
             endif
300     continue

        return
        end
