      SUBROUTINE GETGTI(Lun,Gti,Maxgti,Imaxgti,Status)
      implicit none
c
c  Get GTIs from event file
c
c  I  lun     (i) Logical unit of open event file
c  O  gti     (d) Good time intervals
c  I  maxgti  (i) Maximum number of GTIs
c  O  imaxgti (i) Number of GTIs
c  O  status  (i) Error flag (0=OK)
c
      INTEGER*4 Lun , Maxgti , Status , Imaxgti
      REAL*8 Gti(2,Maxgti)
c
c  Local variables
c
      INTEGER*4 i
      INTEGER*4 hdutype
 
      character(20) extname, hduclas1, comment
 
      INTEGER*4 nrows , tfields
 
      character(20) ttype(20)
      character(20) tunit(20)
      character(10) tform(20)
      INTEGER*4 v
      REAL*8 flag
      LOGICAL*4 anyf
 
      INTEGER starti , stopi
      IF ( Status.NE.0 ) RETURN
 
* find the gti table
 
      i = 1
      extname = ' '
      DO WHILE (Status.EQ.0)
         CALL FTMAHD(Lun,i,hdutype,Status)
         IF ( hdutype.EQ.2 ) THEN
            CALL FTGBNH(Lun,nrows,tfields,ttype,tform,tunit,extname,v,
     &                  Status)
            CALL UPC(extname)
c
c try extname first
c
            IF(extname.NE.'GTI'.AND.extname.NE.'STDGTI')THEN 
              CALL FTGKYS(Lun,'hduclas1',hduclas1,comment,status)
c
c try hduclas second time
c
              IF ( status.NE.0 ) THEN
                  status = 0
              ELSEIF ( hduclas1.EQ.'GTI' ) THEN
                  status = -10
              ENDIF
            ELSE
              status= -10
            ENDIF
         ENDIF
         i=i+1
      ENDDO  
c
c      write(*,*)'status,i',status,i     
      IF ( Status.EQ.0 .or. Status.ne.-10 ) THEN
         CALL XWRITE(' GTI table not found',5)
         Status = -1
         Imaxgti = 0
         RETURN
      ELSE 
         CALL XWRITE(' Found GTI table ',10)
         Status=0 
      ENDIF
 
* else we found the gti table
 
      IF ( nrows.GT.Maxgti ) THEN
         Status = -1
         CALL XWRITE(' maxgti is too small',5)
         RETURN
      ENDIF
 
      CALL FTGCNO(Lun,.FALSE.,'start',starti,Status)
      CALL FTGCNO(Lun,.FALSE.,'stop',stopi,Status)
 
      DO 100 i = 1 , nrows
         CALL FTGCFD(Lun,starti,i,1,1,Gti(1,i),flag,anyf,Status)
         CALL FTGCFD(Lun,stopi,i,1,1,Gti(2,i),flag,anyf,Status)
 100  CONTINUE
 
      Imaxgti = nrows
 
      RETURN
      END
