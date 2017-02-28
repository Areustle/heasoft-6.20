*+CPTHNM
	subroutine cpthnm(disk,dir,file,status)

	implicit none
	character*(*) disk,dir,file
	integer status

C-----------------------------------------------------------------------
C Description: Constructs a system dependent path from the arguments
C              disk, dir, and file.  The completed path is written to 
C              the file argument.  
C
C              The completed path looks like
C
C                   /<disk>/<dir>/<file>
C
C              for UNIX systems, and like
C
C                   <disk>:[<dir>]<file>
C
C              for VMS systems.
C
C              If the input file value is blank then the completed path
C              will look like
C
C                   /<disk>/<dir>
C 
C              on UNIX systems, or like
C
C                   <disk>:[<dir>]
C
C              on VMS systems
C
C              The dir argument can contain more than one directory name
C              separated by '/' characters.  The VMS version of this
C              routine will replace the '/' characters with '.' 
C              characters while assembling the completed path.  The UNIX
C              version will not modify the dir argument during assembly.
C
C              In addition, the UNIX version of this routine will 
C              attempt to translate the disk argument value as an 
C              environment variable.  If the translation is a success,
C              the environment variable value is used in place of the
C              '/<disk>' part of the completed path.  Otherwise, the
C              value of the disk argument is used as shown above.
C
C              Examples:
C              If disk = 'caldb', dir = 'data/asca/gis' and 
C              file = 'caldb.indx', then the completed unix path would be
C
C                   /caldb/data/asca/gis/caldb.indx
C
C              and the completed VMS path would be
C
C                   CALDB:[DATA.ASCA.GIS]CALDB.INDX
C
C              If the disk argument was 'CALDB' and the user had defined
C              CALDB as an environment variable with the value 
C              '/my/home/directory/caldb', then the above unix path
C              would be
C
C                   /my/home/directory/caldb/data/asca/gis/caldb.indx
C
C              while the VMS path would remain unchanged.
C
C Arguments:   disk   (i): root level directory/environment variable/disk
C                          to be used in the completed path
C              dir    (i): directory (and subdirectories) beneath disk
C              file   (i): file in the directory dir
C
C Origin:      Written for the Calibration Database, but based on 
C              XANADU routine ptend.
C
C Authors/Modification History:
C              AFT        Aug  2, 1988 -- Original Version (ptend)
C              KAA        Sep 23, 1990 -- added TRLOG check. (ptend)
C              Ron Zellar Apr  7, 1993 -- modified for Caldb 
C                                         (ptend --> cptend)
C              Ron Zellar Aug  4, 1994 -- rewrote to satisfy Caldb needs
C-----------------------------------------------------------------------
*- Version 1.0

	character(160) root,contxt
	integer   fcstln,len,disklen,dirlen,filelen,rootlen,totlen,i
	character(4) cval,cval1
	
C	Get lengths of arguments
	disklen = fcstln(disk)
	filelen = fcstln(file)
	dirlen  = fcstln(dir)

C	Try translating the disk argument as an environment variable
C       and using the translation as the disk.  If not successful,
C       use disk argument with '/' prepended
	call ctrlog(disk,disklen,root,rootlen)
	if (rootlen .eq. 0) then
             root = '/'//disk(:disklen)
	endif

C	Get the length of root
	rootlen = fcstln(root)

C	Check the total length of completed path to make sure that
C       it doesn't overflow allocated memory. (Use +2 because we'll 
C       be adding a '/' after root and a '/' after dir)
	totlen = rootlen + dirlen + filelen + 2
	if (totlen .gt. len(file)) then
	     write(cval,'(I4)')len(file)
	     write(cval1,'(I4)')totlen
	     contxt='cpthnm : Not enough room in file buffer'
	     call fcerr(contxt)
	     contxt=cval//' bytes allocated, need '//cval1//' bytes'
	     call fcerr(contxt)
	     status = 1
	     return
	endif

C	Move file value to it's final position in completed path
C       This is done in reverse, from end of file value to beginning
	do 100 i=0,filelen-1
	     file(totlen-i:totlen-i)=file(filelen-i:filelen-i)
100	continue

C	Write root and directory in front of file value
C       (Use +2 because adding '/' after root and dir)

C	If file value is blank, then leave off trailing '/' so
C	that we form a directory path.

	if (filelen .eq. 0) then
	     file(1:rootlen+dirlen+1)=root(:rootlen)//
     &	     '/'//dir(:dirlen)
	else
	     file(1:rootlen+dirlen+2)=root(:rootlen)//
     &	     '/'//dir(:dirlen)//'/'
	endif

	return
	end
