*+RENAME
	subroutine mvfile(file1,file2)

	character*(*) file1,file2

C-----------------------------------------------------------------------
C Description: Spawns a 'mv file1 file2' on UNIX systems or a 
C              'rename file1 file2' on VMS systems.
C
C Arguments:   file1 (i) : the file to be renamed
C              file2 (i) : the file name file1 will have
C
C Origin:      Written for the Caldb
C
C Authors/Modification History:
C              Ron Zellar Oct 1, 1993
C-----------------------------------------------------------------------
*+ version 1.0

	character(200) str
	integer length,errstat,fcstln

	str = 'rename '//file1(:fcstln(file1))//' '//file2(:fcstln(file2))
	length = fcstln(str)
	call cspawn(str,length,errstat)
	return
	end
