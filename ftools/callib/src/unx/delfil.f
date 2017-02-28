*+DELFIL
        SUBROUTINE DELFIL(FILNAM)
        
        character*(*) FILNAM
C********************************************************************
C Description:  Deletes the file named FILNAM.  FILNAM can contain
C               the directory path to the file.
C
C Arguments:    FILNAM (i) : The name of the file to be removed
C
C Origin:       Written for the Calibrtion Database
C
C Authors/Modification History:
C               Ron Zellar (1993 Mar 16), original version
C
C********************************************************************
*-Version 1.0 

        integer FCSTLN, LENGTH, IERR
        character(250) tmpstr

        tmpstr = 'rm -f '//filnam
        LENGTH = FCSTLN(tmpstr)
        CALL CSPAWN(tmpstr, LENGTH, IERR)
        RETURN
        END
