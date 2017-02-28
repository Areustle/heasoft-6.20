      SUBROUTINE xcmget(string,iparse,savstrg,maxcom,nccom,nacom,ierr)
      INTEGER*4 iparse,nccom,maxcom,nacom,ierr
      CHARACTER*(*) string,savstrg(maxcom)

c      fwj haberl   16-FEB-1989 10:14:17 
c      xparse routine to recall back saved command strings
c      arguments
c      string          c*            i:   command string to save
c      iparse          i4            i:   current parse position
c      savstrg          c*(maxcom)      i/r: character array for command storage
c      maxcom           i4            i:   maximum no of saved commands
c      nccom          i4            i/r: current no of saved commands
c      ncaom          i4            i/r: no of recalled commands

      INCLUDE 'writebuf.inc'
      INTEGER*4 MAXRCA
      PARAMETER (MAXRCA=30)
      INTEGER*4 NRANGE
      PARAMETER (NRANGE=1)

      INTEGER*4 icom(MAXRCA)
      INTEGER*4 irnglw(NRANGE),irnghi(NRANGE),idefrg(NRANGE)
      INTEGER*4 lenact,iflag,idelim,ibeg,iend,ic,ictot

      LOGICAL*4 qskip
      character(255) savstr
      character(14) rngnam(NRANGE)
      character(12) descr

      DATA rngnam/'command no'/
      DATA descr/'command no'/

      IF(nccom.EQ.0) RETURN

c      come directly from recall command:

      IF(nacom.EQ.0) THEN
         CALL xgtarg(string,iparse,ibeg,iend,qskip,IFlag,idelim)
         IF(iflag.LT.0) THEN
            ierr=iflag
            RETURN
         ENDIF
         IF(iend.LT.ibeg) THEN
            DO ic=1,nccom
               savstr=savstrg(ic)
               WRITE(wrtstr,'(i6,1x,a)')ic,
     &            savstr(:MIN(len(wrtstr)-7,lenact(savstr)))
               CALL xwrite(wrtstr,2)
            ENDDO
         ELSE
            CALL xptarg(string,iparse,string(ibeg:iend),0,ierr)
            IF(ierr.ne.0)return
            ictot=0
            iflag=0
            DO WHILE (iflag.EQ.0)
               CALL xgtrng(string,iparse,NRANGE,rngnam,descr,irnglw,
     &                     irnghi,1,nccom,idefrg,.false.,iflag,idelim)
               IF(iflag.LT.0) THEN
                  ierr = iflag
                  RETURN
               ELSEIF (iflag.EQ.0) THEN
                  DO ic=irnglw(1),irnghi(1)
                     ictot=ictot+1
                     icom(ictot)=ic
                  ENDDO
               ENDIF
            ENDDO
            string=savstrg(icom(1))
            nacom=ictot
            IF(nccom.EQ.maxcom) THEN
               DO ic=2,nacom
                  icom(ic)=icom(ic)-1
               ENDDO
            ENDIF
            iparse=0
            ierr=0
         ENDIF
      ELSE

c come again after a recalled command

         DO ic=1,nacom-1
            icom(ic)=icom(ic+1)
         ENDDO
         nacom=nacom-1                  
         IF(nccom.EQ.maxcom) THEN
            DO ic=2,nacom
               icom(ic)=icom(ic)-1
            ENDDO
         ENDIF
         IF(icom(1).GE.1 .AND. icom(1).LE.maxcom) THEN
            string=savstrg(icom(1))
         ELSE
            string=' '
         ENDIF
         iparse=0
         ierr=0
      ENDIF

      RETURN
      END
