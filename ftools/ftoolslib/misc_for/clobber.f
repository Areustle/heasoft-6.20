
C******************************************************************************
C SUBROUTINE:
C     clobber
C
C DESCRIPTION:
C     This routine clears the way to use the file named filenam.
C     It deletes the file named filenam (under VMS, it deletes
C     the highest numbered version of filenam.) Thus, it leaves
C     filenam available for an OPEN(...,STATUS='NEW') statement.
C
C AUTHOR/DATE:
C     Lawrence Brown 7/12/94
C
C MODIFICATION HISTORY:
C
C NOTES:
C     To add clobber (overwrite) capability to an ftool, put lines like
C     the following in the parameter fetching routine:
C
C      LOGICAL DELEXIST
C      character(160) OUTFILE
C      INTEGER STATUS
C      CALL UCLGSB('CLOBBER', DELEXIST, STATUS)
C      IF (STATUS .NE. 0) THEN
CC     Probably means there wasn't a clobber field in the .par file
C         STATUS=0
C      ELSE IF(DELEXIST) THEN
C         CALL CLOBBER(OUTFILE,STATUS)
C         IF(STATUS.NE.0) THEN
CC     Do something appropriate. outfile is probably read only.
C      ENDIF
C
C    Then add:
C
C    clobber,b,h,no,,,"Overwrite existing output file? (CAUTION)"
C
C    to the par file.
C
C USAGE:
C     call clobber(filenam,status)
C
C ARGUMENTS:
C     filenam - the file to be "clobbered"
C     status  - returned error status
C
C PRIMARY LOCAL VARIABLES:
C     exists - logical for inquire statements
C     lun - logical unit number for clobbering
C
C CALLED ROUTINES:
C
C******************************************************************************
      subroutine clobber(filenam,status)
      character*(*) filenam
      integer status
C
      logical exists,opened
      integer lun

      if(status.ne.0) return

      inquire(file=filenam,exist=exists)
      if(exists) then
C     get rid of it
C     first look for a free logical unit number to use to commit the act with
         do 10 lun=99,10,-1
            inquire(lun,opened=opened)
            if(.not.opened) goto 20
 10      continue
C     failed to find free lun
         status=1
         goto 999
 20      continue
         open(lun,file=filenam,status='old',err=30)
         close(lun,status='delete',err=40)
C     we're done
         goto 999
 30      continue
C     error opening file
         status=2
         goto 999
 40      continue
C     error closing and deleting file (This could really mess up the main
C     code, so check for it
         status=3
      endif
 999  continue
      return
      end
