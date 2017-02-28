
CH  Lorraine Breedon (1.0.0 14 Mar 1999) Original working version


C  This is a subroutine to readOSO-8 B/C detector raw PHA data
C
      SUBROUTINE OSO_RDRAW(rawunit,qtm,detector,row,totrows,
     &                 isect,id,imillisec,
     &                 sp_ra,sp_dec,sn_ra,sn_dec,pha,
     &                 finished,Status)

 
      IMPLICIT NONE
 
      LOGICAL anyf,qtm,finished
      integer*4 row,rawunit,totrows,Status
      integer*4 imillisec, id,detector
      REAL sp_ra,sp_dec,sn_ra,sn_dec,pha(63)
      double precision sect
      integer*4 isect
      integer*2 iid
      
      Status = 0
      id = 0
      iid=0
      imillisec = 0
      sp_ra = 0
      sp_dec = 0
      sn_ra = 0
      sn_dec = 0
      
c      type *, 'IN RDRAW'
      IF (row .gt. totrows) then
         finished = .TRUE.
         goto 999
      ENDIF

      IF (detector .ne. 0) then
         isect = 0
      ELSE
         CALL FTGCVD(rawunit,1,row,1,1,0.0,sect,anyf,Status)
         isect=int(sect)
      ENDIF
      CALL FTGCVI(rawunit,2,row,1,1,0,iid,anyf,Status)
      IF (Status .ne. 0) goto 999
      id=iid
      CALL FTGCVJ(rawunit,3,row,1,1,0,imillisec,anyf,Status)
      IF (Status .ne. 0) goto 999
      
c      IF (.NOT. qtm ) goto 999
      CALL FTGCVE(rawunit,4,row,1,1,0.0,sp_ra,anyf,Status)
      IF (Status .ne. 0) goto 999
      CALL FTGCVE(rawunit,5,row,1,1,0.0,sp_dec,anyf,Status)
      IF (Status .ne. 0) goto 999
      CALL FTGCVE(rawunit,6,row,1,1,0.0,sn_ra,anyf,Status)
      IF (Status .ne. 0) goto 999
      CALL FTGCVE(rawunit,7,row,1,1,0.0,sn_dec,anyf,Status)
      IF (Status .ne. 0) goto 999
      CALL FTGCVE(rawunit,8,row,1,63,0.0,pha,anyf,Status)
      IF (Status .ne. 0) goto 999
      
c      type *, isect,id,imillisec,
c     &                 sp_ra,sp_dec,sn_ra,sn_dec
 999  CONTINUE
      RETURN
 
      END

