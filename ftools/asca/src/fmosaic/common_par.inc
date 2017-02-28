C
C       common_par.inc : common parametes block for mosaic analysis. 
C
C       Ilana Harrus 1999
C
c---- job parameters                                                    
      character(200) evfile
c.... output files
      character(200) keywd
      character(12) sizeunit,bckmeth
      character(3) bckdone,emapdone,enrgcut
c.... image parameters
      real imgsizex,imgsizey,cells,ra0,dec0,el,eu
      integer seed,nevtott
      common/parnames/evfile,keywd,sizeunit,bckmeth
     & ,bckdone, emapdone,enrgcut
      common/parfig/imgsizex,imgsizey,cells,ra0,dec0,el,eu
     & ,seed,nevtott











