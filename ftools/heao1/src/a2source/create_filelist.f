C----------------------------------------------------------------------------
C This subroutine creates the output file listings
C
C Author: Lorraine Breedon (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Version 1.0 Oct 26 1998 

      subroutine create_filelist(source,clobber,ounit1,ounit2,
     &                          hed_outlist,med_outlist,errstat)

      implicit none
      character*(*) hed_outlist,med_outlist,source
      integer errstat,ounit1,ounit2
      logical clobber
      

C Local variables

      integer lenact,outlen
      character(160) message
      logical there

C Initialise variables

      errstat=0
      


C listing for HED data

      CALL XGTLUN(ounit1,errstat)
      IF ( errstat.NE.0 ) THEN
         message = ' Problem obtaining free unit number'
         CALL XAERROR(message,1)
         return
      ENDIF

      hed_outlist = source(:LENACT(source))//'_hedfiles.lis'
      outlen = LENACT(hed_outlist)
      IF ( clobber ) THEN
         CALL XDELFIL(hed_outlist(:outlen),errstat)
         IF ( errstat.NE.0 ) THEN
            message = ' Failure to delete existing file '//hed_outlist
            CALL XAERROR(message,1)
            return
         ENDIF
         errstat = 0
         OPEN (ounit1,FILE=hed_outlist,IOSTAT=errstat,STATUS='new')
         IF ( errstat.NE.0 ) THEN
            message = 'Failure to open output list file '//hed_outlist
            CALL XAERROR(message,1)
            return
         ENDIF
 
      ELSE
C no clobber option is set, therefore check if the file already exists
C ...if so, error out
         INQUIRE (FILE=hed_outlist,EXIST=there)
         IF ( there .EQV. .TRUE. ) THEN
            message = 'Error:output file already exists '//hed_outlist
            CALL XAERROR(message,1)
            return
         ELSE
            OPEN (ounit1,FILE=hed_outlist,IOSTAT=errstat,STATUS='new')
            IF ( errstat.NE.0 ) THEN
               message = 'Failure to open output list file '//
     &                   hed_outlist
               CALL XAERROR(message,1)
               return
            ENDIF
         ENDIF
 
      ENDIF

C listing for MED data
 
 
      CALL XGTLUN(ounit2,errstat)
      IF ( errstat.NE.0 ) THEN
         message = ' Problem obtaining free unit number'
         CALL XAERROR(message,1)
         return
      ENDIF
 
      med_outlist = source(:LENACT(source))//'_medfiles.lis'
      outlen = LENACT(med_outlist)
      IF ( clobber ) THEN
         CALL XDELFIL(med_outlist(:outlen),errstat)
         IF ( errstat.NE.0 ) THEN
            message = ' Failure to delete existing file '//med_outlist
            CALL XAERROR(message,1)
            return
         ENDIF
         errstat = 0
         OPEN (ounit2,FILE=med_outlist,IOSTAT=errstat,STATUS='new')
         IF ( errstat.NE.0 ) THEN
            message = 'Failure to open output list file '//med_outlist
            CALL XAERROR(message,1)
            return
         ENDIF
      ELSE
C no clobber option is set, therefore check if the file already exists
C ...if so, error out
         INQUIRE (FILE=med_outlist,EXIST=there)
         IF ( there .EQV. .TRUE. ) THEN
            message = 'Error:output file already exists '//med_outlist
            CALL XAERROR(message,1)
            return
         ELSE
            OPEN (ounit2,FILE=med_outlist,IOSTAT=errstat,STATUS='new')
            IF ( errstat.NE.0 ) THEN
               message = 'Failure to open output list file '//
     &                   med_outlist
               CALL XAERROR(message,1)
               return
            ENDIF
         ENDIF
 
 
      ENDIF
 
C write header line in outfiles
 
      message = '# File listing for :'
      WRITE (ounit1,99007) message
      WRITE (ounit2,99007) message

 
99007 FORMAT (A20)

      return

      end


