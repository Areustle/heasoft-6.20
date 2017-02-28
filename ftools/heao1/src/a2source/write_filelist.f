C----------------------------------------------------------------------------
C This subroutine writes to the output file listings
C
C Author: Lorraine Breedon (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Version 1.0 Oct 26 1998 

      subroutine write_filelist(ounit1,ounit2,hed_outlist,med_outlist,
     &                 start1,stop1,start2,stop2,errstat)

      implicit none
      character*(*) hed_outlist,med_outlist
      integer errstat,ounit1,ounit2
      integer start1,stop1,start2,stop2
      

C Local variables
      integer i
      character(160) message
      logical there1,there2

C Initialise variables

      errstat=0


c now write med and hed output file listings

      DO 400 i = start1 , stop1
         WRITE (message,'(''a2_xrate'',i4.4,''med_128.raw'' )') i
         WRITE (ounit2,99006) message
 400  CONTINUE
      DO 500 i = start2 , stop2
         WRITE (message,'(''a2_xrate'',i4.4,''med_128.raw'' )') i
         WRITE (ounit2,99006) message
 500  CONTINUE
 
      DO 600 i = start1 , stop1
         WRITE (message,'(''a2_xrate'',i4.4,''med_512.raw'' )') i
         WRITE (ounit2,99006) message
 600  CONTINUE
      DO 700 i = start2 , stop2
         WRITE (message,'(''a2_xrate'',i4.4,''med_512.raw'' )') i
         WRITE (ounit2,99006) message
 700  CONTINUE
 
      DO 800 i = start1 , stop1
         WRITE (message,'(''a2_xrate'',i4.4,''hed_128.raw'' )') i
         WRITE (ounit1,99006) message
 800  CONTINUE
      DO 900 i = start2 , stop2
         WRITE (message,'(''a2_xrate'',i4.4,''hed_128.raw'' )') i
         WRITE (ounit1,99006) message
 900  CONTINUE
 
      DO 1000 i = start1 , stop1
         WRITE (message,'(''a2_xrate'',i4.4,''hed_512.raw'' )') i
         WRITE (ounit1,99006) message
 1000 CONTINUE
      DO 1100 i = start2 , stop2
         WRITE (message,'(''a2_xrate'',i4.4,''hed_512.raw'' )') i
         WRITE (ounit1,99006) message
 1100 CONTINUE

      INQUIRE (FILE=hed_outlist,EXIST=there1)
      INQUIRE (FILE=med_outlist,EXIST=there2)
      IF ( there1 .EQV. .TRUE. ) THEN
         message = 'Output file created : '//hed_outlist
         CALL XWRITE(message,8)
      ENDIF
      IF ( there2 .EQV. .TRUE. ) THEN
         message = 'Output file created : '//med_outlist
         CALL XWRITE(message,8)
      ENDIF
 
 
C close the listing files
      CLOSE (ounit1)
      CALL XFRLUN(ounit1,errstat)
      IF ( errstat.NE.0 ) THEN
         message = ' Problem releasing unit number'
         CALL XAERROR(message,1)
         return
      ENDIF
 
      CLOSE (ounit2)
      CALL XFRLUN(ounit2,errstat)
      IF ( errstat.NE.0 ) THEN
         message = ' Problem releasing unit number'
         CALL XAERROR(message,1)
         return
      ENDIF


99006 FORMAT (A23)

      return

      end


