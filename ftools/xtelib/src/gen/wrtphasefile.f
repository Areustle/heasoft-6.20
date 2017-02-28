c     This subroutine was added due to the excessive demands on GTIs if a
c phase interval selection is used. Normally, this isn't a problem but
c since people are using the extractors as crude filters is a phase interval
c is selected you can literally create hundreds of millions of GTI's. To
c account for this we are adding this routine which if phase filtering is
c turned on, we will write out a crude phase filtering file capable of
c being used by Xronos. This is based on rphasefile which is the routine
c that reads a Xronos file. We will write out the phase-interval information
c to the file OUTROOT.phase So we will perform some reads on the parameter
c file to extract this information rather than carrying it along with us.

      subroutine wrtphasefile(ephem,period,
     &   iphaseno,phases,phasee)
      implicit none
      double precision ephem,period,phases(*),phasee(*),
     &   ephem_day,period_day
      character(160) outroot
      character(250) outfile
      integer status,iphaseno,i,outlen,fcstln,ounit,izero,ione,
     &   iomode,recl

      status=0
      outfile=' '
      izero=0
      ione=1
      ephem_day=ephem/86400.0d0

c     Since Xronos works in TJD we have to convert to TJD... 
      ephem_day=ephem_day-40000.0d0
      period_day=period/86400.0d0
      
C  get the name of the output root file
      call uclgst('outroot',outroot,status)
      if (status .ne. 0) then
        call fcerr('could not get OUTROOT prefix parameter')
        stop
      endif
      outlen=fcstln(outroot)
      outfile= outroot(:outlen) // '_phase.fil'
      outlen=fcstln(outfile)

      call fcecho(' ')
      call fcecho('Output PHASE_FILE has the name:')
      call fcecho(outfile(:outlen))
      call fcecho(' ')
      call fcecho('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
      
      print*,'EPHEM and Period are ',ephem,period
      do i=1,iphaseno
        print*,'PHases are ',i,phases(i),phasee(i)
      enddo

      call ftgiou(ounit,status)
      if(status.ne.0)then
        call fcecho('Error getting output unit number')
        call fcecho('for phasefile being created.')
        call fcecho('Setting to logical unit 10')
        status=0
        ounit=30
      endif

C      call faopen (unit, filename, iomode, recl, status)
C
C ARGUMENTS:
C      unit     - input  - integer - unit number to open
C      filename - input  - string  - name of file to open ! to overwrite
C      iomode   - input  - integer - I/O mode to open file
C                                    1 - readonly
C                                    2 - new file (! overwrites if exists)
C                                    3 - read/write
C                                    4 - append
      iomode=2
C      recl     - input  - integer - maximum record length <0 system default
C                                    (133 for VMS, ignored for unix)
      recl=-1
C      status   - output - integer - status of operation
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C*****************************************************************************
      call faopen(ounit, outfile, iomode, recl, status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Could not create PHASE file!')
        call fcecho('No file will be written.')
        call fcecho('Returning...')
        status=0
        return
      endif
      
      write(ounit,110)12+iphaseno
110   format(I5,1x,' Windows in this < Xronos Window File >')
      write(ounit,111)
111   format(' 0 Time Wind.: start       stop  (days)')
      write(ounit,112)iphaseno
112   format(I5,1x,'Phase Wind.: epoch  period  (days)/ start stop (0->
     &1) phases    max   10')
      write(ounit,*)ephem_day,period_day
      do i=1,iphaseno
        write(ounit,*)phases(i),phasee(i),i
      enddo
      
      do i=1,4
        write(ounit,120)izero,i
120     format(I5,1x,'Ints. Wind. for Orig. Bins in Series '
     &     ,I5,1x,': min  max (c/s)         max   10')
        write(ounit,121)izero,i
121     format(I5,1x,'Ints. Wind. for New Bins in Series '
     &     ,I5,1x,': min  max (c/s)         max   10')
        write(ounit,122)izero,i
122     format(I5,1x,'Ints. Wind. for Intervals in Series '
     &     ,I5,1x,': min  max (c/s)         max   10')
        write(ounit,123)ione,i
123     format(I5,1x,'Exps. Wind. for Orig. Bins in Series '
     &     ,I5,1x,': min  max (0->50)       max    1')
        write(ounit,*)0.25,50.0,1
        write(ounit,124)ione,i
124     format(I5,1x,'Exps. Wind. for New Bins in Series '
     &     ,I5,1x,': min  max (0->50)       max    1')
        write(ounit,*)0.01,50.0,1
        write(ounit,*)ione,i
125     format(I5,1x,'Exps. Wind. for Intervals in Series '
     &     ,I5,1x,': min  max (0->50)       max    1')
        write(ounit,*)0.5,50.0,1
      enddo

      close(ounit)
      
      call ftfiou(ounit,status)
      if(status.ne.0)then
        call fcecho('Error freeing output unit number')
        call fcecho('for output phase file.')
        status=0
      endif

999   continue
      
      return
      end
      
