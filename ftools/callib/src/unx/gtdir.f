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
C              Ron Zellar June 17, 1993 Original Version
C-----------------------------------------------------------------------
*-Version 1.0
c MJT 05July96 g77/linux unhappy with getcwd
c        call getcwd(dir)
c MJT 23July96 g77/linux -- now added a working getdir to xanlib
c                           so the current routine would appear
c                           to be superfluous -- just call getdir!
        call getdir(dir)

        return
        end
