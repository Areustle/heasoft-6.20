        
c      I/R   nwi = no. of windows in each window type
c      I/R   pwi,pwia,pwio = phase window(s) epoch, per., start, stop (max 10)
c      for each series separately:        
        
        subroutine rphasefile(filename,mjdref,imjdref,ephem,period,
     &     iphaseno,phases,phasee)
c        implicit none

        integer isiz
        parameter (isiz=1000)
        double precision Twia(isiz),Twio(isiz),ephem,period
        double precision phases(isiz),phasee(isiz),mjdref
        integer lup,nwito,Nwi(isiz),iphaseno,idum,status,imjdref,
     &     i
        character*(*) filename

        status=0
        
c     Assign a unit file number used in inputting file.
        call ftgiou(lup,status)
        if(status.ne.0)then
           call fcecho('Error getting input unit number')
           call fcecho('Setting to logical unit 10')
           status=0
           lup=30
        endif

        OPEN(lup,file=filename,status='UNKNOWN')

c        print*,'About to do a status check',status
        
c          print*,'Trying to do a read'
c      read header line
          READ (lup,*,err=800,end=800) Nwito
c          print*,'nwito',Nwito
c      read time windows
          READ (lup,*,err=800,end=800) Nwi(1)
c          print*,'nwi(1) is',nwi(1)
          IF ( Nwi(1).GT.0 ) THEN
            DO 420 i = 1 , Nwi(1)
              READ (lup,*,ERR=800,END=800) Twia(i) , Twio(i) , idum
c              print*,'Twia(i),Twio(i),idum',Twia(i),Twio(i),idum
420         CONTINUE

c      read phase windows
          READ (lup,*,ERR=800,END=800) Nwi(2)
c          print*,'Nwi(2) is',Nwi(2)
          IF ( Nwi(2).GT.0 ) THEN
            READ (lup,*,ERR=800,END=800) ephem,period
            ephem=ephem+40000.0d0
            ephem=ephem-(dfloat(imjdref)+mjdref)
            period=period*86400.0d0
            ephem=ephem*86400.0d0
            
            DO 440 i = 1 , Nwi(2)
              READ (lup,*,ERR=800,END=800) Phases(i),Phasee(i),iphaseno
c              print*,'Phases(i) Phasee(i) iphaseno'
c     &           ,i,Phases(i),Phasee(i),iphaseno
440         CONTINUE
          ENDIF
        endif
        
800     continue
C99001   FORMAT (/,' **** Warning : Illegal assignement for Ints/Expos',
C     &     ' Windows of Series',I2,/,'                Series ',A,
C     &     ' Ints/Expos Windows from',' file will be used !')

        close(lup)

        call ftfiou(lup,status)
        if(status.ne.0)then
          call fcecho('Error freeing output unit number')
          status=0
        endif
         
        return
        end

 
