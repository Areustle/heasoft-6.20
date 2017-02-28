

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

        integer FCSTLN, LENGTH, FILLEN, IERR

        FILLEN = FCSTLN(FILNAM)
        LENGTH = FCSTLN('delete/nolog '//FILNAM(:FILLEN)//';')
        CALL CSPAWN('delete/nolog '//FILNAM(:FILLEN)//';', LENGTH, IERR)
        RETURN
        END
