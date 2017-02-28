C**********************************************************************
C 
C Subroutine: xrgetplto
C     get plotting parameters
C
C Author:
C     Lawrence E Brown
C     Hughes STX for
C     HEASARC NASA/GSFC
C
C Modification History:
C     
C
C Notes:
C      see xronos.inc for the meaning and current size of all parameter
C      arrays
C
C      this routine is to replace xrgetplto.f calls
C
C Calling Sequence:
C     call xrgetplto(progname,nser,cqdp,iqdp,status)
C     
C
C Arguments:
C
C     I   progname = program name (to construct plotfile name)
c     I   nser = number of series (to construct plotfile name)
C     O   cqdp = chars variables for qdp (10)
C     O   iqdp = integer flags for qdp (10)
C**********************************************************************
      SUBROUTINE xrgetplto(progname,iflags,cqdp,iqdp,status)
      implicit none
      
      include '../include/io.inc'
      logical plot,loex
      character(160) plotdev, plotfile, plotfiled
      character(160) dummyfile, dpath, dummyprog
      integer plotdnum,ierr,idum
      integer lenact, iu
      external lenact
      
      include '../include/xronos.inc'
      include '../include/xronos_init.inc'
      parameter (subname = 'xrgetplto:')
      
      
      
      if (status.ne.0) return
C
      nser=iflags(10)      
      
C     Do you want to plot this?
      call uclgsb('plot',plot,status)
      if (status.ne.0) then
         context = 'Error getting PLOT parameter'
         errm = subname//' '//context
         call xaerror (errm, 5)
         goto 999
      endif
      if (plot) then
         iqdp(1) = 1
      else
         iqdp(1) = 0
C     no plot so leave routine
         goto 999
      endif
      
C     get plotting device
      call uclgst('plotdev',plotdev,status)
      if (status.ne.0) then
         context = 'Error getting PLOTDEV parameter'
         errm = subname//' '//context
         call xaerror (errm, 5)
         goto 999
      endif
      cqdp(1)=plotdev
      
c     We'll check plot dev and interact on that in the plotting routine 
c     so as to isolate the plotting stuff
c
c input the additional plotting file      
c
      cqdp(3)= ' '
      call uclgst('plotfile',plotfile,status)
      cqdp(3)=plotfile
c      write(*,*)'cqdp(3)',cqdp(3)
c
c     add .pco extension if there is not one
      CALL xrfilext(cqdp(3), '.pco', 1)
c
c Check if deafults otherwise check if the file exists
c
      If ((plotfile .eq. 'INDEF').or.(plotfile .eq. ' ') .or. 
     &     (plotfile .eq. '-').or.(plotfile.eq.'default')) then 
         context= ' No extra plotting commands added '
         call xwrite(context,15)
         context= ' Use the standard files'
         call xwrite(context,15)
         cqdp(3)= 'none'
      else 
c
c Check that file exists
        INQUIRE (FILE=cqdp(3), IOSTAT=ierr, EXIST=loex)
        context= ' Extra plotting commands added '//cqdp(3)
        call xwrite(context,15)
        IF ((ierr.NE.0).or.(loex.EQV..FALSE.)) then
           status=0
           context = ' Can''t access plot file:'//cqdp(3)
           call xwrite(context,5)
           context = ' Using simple plotting.'
           call xwrite(context,5)
           cqdp(3)= 'none'
         endif
      endif 
c
c
      status=0      
c
c     get default plot file number
c     only for multitype series output program (no cross correlation)
c
      if(nser.gt.1.and.progname(1:2).ne.'cc'.and.
     &  progname(1:2).ne.'es') then 
c
c go for the multi plot commnad file
        if(nser.eq.2) then
           context=' Three plotting styles available:'
           call xwrite(context,10)  
           context= ' Hardness [1] ; '//
     &              ' Intensity vs Time (or Phase) [2]; '
           call xwrite(context,10)  
           context=' Ratio & Intensity vs time (or Phase)[3]'
           call xwrite(context,10)  
        else
           context=' Two plotting styles available:'
           call xwrite(context,10)  
           context= ' Colour-Colour [1] ;'//
     &      ' Intensity vs Time (or Phase)[ # of series (3 or 4)];'
          call xwrite(context,10)  
        endif
        call uclgsi('plotdnum',plotdnum,status)
        if (status.ne.0) then
            context = ' Warning, couldn''t get PLOTDNUM parameter.'
            call xwrite(context,5)
            context = ' Setting PLOTDNUM = 1.'
            call xwrite(context,5)
            status=0
            plotdnum=1
        endif 
        If(nser.eq.2.and.plotdnum.eq.3)then 
           plotdnum=nser
           iqdp(5)=3
        endif
      else 
        plotdnum=1
      endif
      if(progname(1:2).eq.'es')plotdnum=nser
c      write(*,*)'be- iflags(11), plotdnum',iflags(11), plotdnum 
      iflags(11) = plotdnum
c      write(*,*)'ifla(11)plotdnumiqdp(5)',iflags(11), plotdnum,iqdp(5) 
c
c     calculate default name
c     
c XXn_m.pco where XX is the program name n=series number and m=number of plot
c Note for multicolor tasks (eg. ef and lc) the XX=mu to avoid doubling 
c the number of file in the defaults directory.
c
c
      dummyfile=' '
      dummyprog=progname
c
c
      if(progname(1:2).eq.'lc'.or.progname(1:2).eq.'ef')
     &   dummyprog='mu'
      call ftkeyn(dummyprog,nser,dummyfile,status)
      iu = lenact(dummyfile) + 1
      dummyfile(iu:iu) = '_'
      call ftkeyn(dummyfile,plotdnum,plotfiled,status)
      call uclgst('dpath',dpath,status)
      if (dpath .eq. 'XRDEFAULTS') then
         call getenv('XRDEFAULTS', dpath)
         if(index(dpath, '/') .ne. 0) then
            dpath = dpath(1:lenact(dpath))//'/'
         elseif(index(dpath, '[') .eq. 0) then
            dpath = ' '
         endif
      endif
      plotfiled = dpath(1:lenact(dpath))//
     &    plotfiled(1:lenact(plotfiled))//
     &    '.pco'
      if (status .ne. 0) then
          context = 'Couldn''t construct default plotting file.'
          errm = subname//' '//context
          call xaerror(errm, 5)
          goto 999
      endif
c
      cqdp(2) = plotfiled
c      write(*,*)'cqdp(2)',cqdp(2)
c      
c     add .pco extension if there is not one
      CALL xrfilext(cqdp(2), '.pco', 1)
c
c     check that file exists
      INQUIRE (FILE=cqdp(2), IOSTAT=ierr, EXIST=loex)
      IF ((ierr.NE.0).or.(loex.EQV..FALSE.)) then
         status=0
         context = ' Can''t access plot file:'//cqdp(2)
         call xwrite(context,5)
         context = ' Using simple plotting.'
         call xwrite(context,5)
         cqdp(2)= ' '
      endif
c     error trap
      IF(cqdp(3).NE.'none')THEN 
          context = ' Using user plot file '//cqdp(3)
          call xwrite(context,15)
      ENDIF
      context = ' Using plot file '//cqdp(2)
      call xwrite(context,15)
c      
 999  return
      end
      
