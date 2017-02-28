*+GTDIR
        subroutine gtdir(dir)
        character*(*)dir
C-----------------------------------------------------------------------
C Description: Gets the current working directory and returns it to the
C              argument dir.  The VMS version will return the disk and
C              directory.  If the current working directory is not valid
C              a blank directory string is returned.
C
C Arguments:   dir (r): the current working directory
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C              Nick White May 5, 1991 Original Version
C              Song Yom Aug 8, 1992 incorporated SYS$SETDDIR routine
C              Ron Zellar June 17, 1993 Original Version
C-----------------------------------------------------------------------
*-Version 1.0

         integer*4 SYS$SETDDIR
         integer*4 errstat, ios
         integer*4 dirlen
         character(255) dirspc, fulldir
         integer*4 fcstln, fdirlen, i, j

C        Get the current directory without the disk name
         errstat = SYS$SETDDIR (,dirlen,dirspc)

C        Inquire will return the full directory name with the 
C        disk name attached and some garbage at the end if the 
C        directory exists

         inquire (FILE=dirspc(:dirlen),NAME=fulldir)
         fdirlen = fcstln(fulldir)

C        If fulldir is the same as dirspc, then the current directory
C        is not valid.  If they are different, assign the the disk and 
C        directory part of the path to dir.
 
         If (fulldir(:fdirlen).ne.dirspc(:dirlen)) then
              j = index(fulldir, ']')
              dir = fulldir(:j)
         else
              dir = ' '
         endif

         return
         end 
