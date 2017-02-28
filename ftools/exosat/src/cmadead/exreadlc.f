      subroutine exreadlc(lchdu,lcrows,lcfile,time,rate,status)

C EXOSAT READ LightCurve
C This routine reads the lightcurve file and fills arrays time and rate
C
C I lchdu    HDU of lightcurve
C I lcrows   Number of rows in lightcurve
C I lcfile   Light curve file
C O time     Time array read from lcfile 
C O rate     Rate array read from lcfile 
C
      IMPLICIT NONE

C Input/Output Variables
      integer*4 lchdu,lcrows
      real*8 time(lcrows)
      real*4 rate(lcrows)
      character*(*) lcfile


C Local variables
      character subname*50,context*160,comment*80,errm*255
      character(80) string,string2
      CHARACTER errtxt*30, stat*15
      integer*4 readwrite,status,iounit,blocksize,hdutype
      integer*4 np,i,tcolnum,rcolnum
      integer*4 frow,felem,nelems,nullj
      real*8 tt,nulld
      real*4 rrate,nulle
      logical anynull,casesen

      if (status.ne.0) return
C     row1=0
      subname='exreadlc: '
      readwrite=0
      status=0
      tcolnum=0
      rcolnum=0
      string=' '
      string2=' '
      CALL FTGIOU(iounit,status)
      if(status.ne.0) then
         context="Can't get unit number"
         goto 999
      endif

C========================================================
C Open the FITS file - READ
C========================================================
      CALL FTOPEN(iounit,lcfile,readwrite,blocksize,status)
      if(status.ne.0) then 
         CALL FTGERR(status,errtxt)         
         context="Can't find "//lcfile
         goto 999
      endif

C========================================================
C Read each row of the lc file 
C
C========================================================

      CALL FTMAHD(iounit,lchdu,hdutype,status)
      if(status.ne.0) then 
         CALL FTGERR(status,errtxt)         
         context="Can't move to RATE Extension"
         goto 999
      endif
C
C Check to see if we are in the rate FITS extension
C
      CALL FTGKYS(iounit,'EXTNAME',string,comment,status)
      CALL RMVXBK(string)
      CALL FTGKYS(iounit,'HDUCLAS1',string2,comment,status)
      if(status.ne.0) status=0
      CALL RMVXBK(string2)
      if((string.eq.'RATE').or.(string.eq.'COUNTS').or.
     &     (string2.eq.'LIGHTCURVE')) then
         
         np=0
C
C Check which columns are time and rate
C
         casesen=.false.
         CALL FTGCNO(iounit,casesen,'TIM*',tcolnum,status)
         CALL FTGCNO(iounit,casesen,'RAT*',rcolnum,status)
         if(status.ne.0) then
            status=0
            CALL FTGCNO(iounit,casesen,'COUNT*',rcolnum,status)
         endif

C Read entire lightcurve table
         nullj=0
         nulle=0.0
         nulld=0.0d0
         do i=1,lcrows
            frow=1
            felem=1
            nelems=1


C Read the time column
            CALL FTGCVD(iounit,tcolnum,i,felem,nelems,nulld,tt,
     &           anynull,status)

            time(i)=tt
        
C Read the rate column    
            CALL FTGCVE(iounit,rcolnum,i,felem,nelems,nulle,rrate,
     &           anynull,status)

            rate(i)=rrate
            


            if(status.ne.0) then 
               CALL FTGERR(status,errtxt)         
               context= "Can't read TIME or RATE columns"
               goto 999
            endif
         enddo
               
      else
         errm='RATE Table not found'
         CALL xaerror(errm,5)
         goto 1000
      endif
 20   format(2F18.5)
      CALL ftclos(iounit,status)
      CALL ftfiou(iounit,status)
      if(status.eq.0) goto 1000
 999  continue
      CALL ftclos(iounit,status)
      CALL ftfiou(iounit,status)
      write(stat,*) status
      errm=subname//' '//context//', '//errtxt//' '//stat
      CALL RMVXBK(errm)
      CALL xaerror(errm,5)
 1000 continue
      return
      end
      
