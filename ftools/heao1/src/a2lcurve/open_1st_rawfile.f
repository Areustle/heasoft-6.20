C----------------------------------------------------------------------------
C This subroutine opens the input listing file and places all raw data
C filenames in memory.\
C Opens the first raw data file in the array.\
C Sets the values of logicals hedflag and medflag\
C
C Author: Lorraine Breedon (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Version 1.0 Oct 20 1998 

      subroutine open_1st_rawfile(infile, hdet,hds,medflag,hedflag,nf,
     &                     arrsz,infmx,rawunit,rawfiles,
     &                     rawfile,ichat,errstat)

      implicit none
      integer arrsz
      character*(*) rawfiles(arrsz),infile,rawfile
   
      integer errstat,rawunit,nf,infmx,ichat
      integer*2 hdet,hds(4)
      logical medflag, hedflag
      

C Common block declarations

      common /TASK/ taskname
      
      character(40) taskname

C Local variables
      integer hdutype
      character(160) message,fileinlist,raw
      integer kunit,status,lenact,nfiles,blocksize,i
      logical isthere
      
C Initialise variables

      
      errstat=0
      status=0
      nfiles=0
      do i=1,arrsz
           rawfiles(i)=' '
      enddo
    


 
c open listing file
      CALL XGTLUN(kunit,errstat)
      IF ( errstat.NE.0 ) THEN
         message = ' Problem obtaining free unit number'
         CALL XAERROR(message,1)
         return
      ENDIF
 
      OPEN (kunit,FILE=infile,IOSTAT=errstat,STATUS='OLD')
      IF ( errstat.NE.0 ) THEN
         INQUIRE (FILE=infile,EXIST=isthere)
         IF ( isthere .NEQV. .TRUE. ) THEN
            message = 'cant find input listing file !!'
            GOTO 50
         ENDIF
         message = ' can find but cant open listing file '
 50      CALL XAERROR(message,1)
         return
      ENDIF
      DO WHILE ( .TRUE. )
 
 
 
C get .raw filenames  from listing  file
 
         READ (kunit,'(A)',IOSTAT=errstat,END=100) fileinlist
         IF ( errstat.NE.0 ) THEN
            message = 'error reading input listing file '
            CALL XAERROR(message,1)
 
 
            return 
         ENDIF
 
 
C skip lines beginning with '#' characters.
C ** skip 128 data, since  THIS PROGRAM VERSION ONLY DEALS WITH 5.12 sec data
 
         IF ( (fileinlist(1:1).NE.'#') .AND. 
     &        (fileinlist(17:19).NE.'128') ) THEN
            nfiles = nfiles + 1
            rawfiles(nfiles) = fileinlist(:LENACT(fileinlist))
         ENDIF
      ENDDO
 
 
 
 100  IF ( nfiles.EQ.0 ) THEN
         message = 'input listing file empty !! '
         CALL XAERROR(message,1)
         return 
      ENDIF
 
C close the listing file
      CLOSE (kunit)
      CALL XFRLUN(kunit,errstat)
      IF ( errstat.NE.0 ) THEN
         message = ' Problem releasing unit number'
         CALL XAERROR(message,1)
         return
      ENDIF
 
C nf will initially be set to the first file in filename array
C and infmx will be set to the total number of files in the array

      nf = 1
      infmx = nfiles

C Open the raw data FITS files for MED and HED as required.
C Only opens the 5.12 data for this program version.
 
      status = 0
      medflag = .FALSE.
   
      rawunit = 0
      IF ( hdet.EQ.1 ) medflag = .TRUE.
      hedflag = .FALSE.
      IF ( hdet.GT.1 ) hedflag = .TRUE.

         raw=rawfiles(nf)
         rawfile=raw(:LENACT(raw))
    
         CALL FTGIOU(rawunit,status)
            CALL FTOPEN(rawunit,rawfile,0,blocksize,status)
         IF ( status.NE.0 ) THEN
            WRITE (message,'('' Error opening '', A100)') rawfile
            CALL XWRITE(message,ichat)
            CALL FTGMSG(message)
            CALL XWRITE(message,ichat)
            WRITE (message,'('' FITSIO status = '', I3)') status
            CALL XWRITE(message,ichat)
            status = 0
            CALL FTCLOS(rawunit,status)
            CALL FTFIOU(rawunit,status)
            return
         ENDIF
         CALL FTMRHD(rawunit,1,hdutype,status)
         IF ( status.NE.0 ) THEN
            WRITE (message,
     &       '('' Failure to open binary extension in  '',         A40)'
     &       ) rawfile
            CALL XWRITE(message,ichat)
            CALL FTGMSG(message)
            CALL XWRITE(message,ichat)
            WRITE (message,'('' FITSIO status = '', I3)') status
            CALL XWRITE(message,ichat)
            return
          ENDIF
       
 




         return
         end


