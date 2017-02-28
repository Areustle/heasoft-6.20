      subroutine ini_events(unit,evfnami,rmfnami,mkfnami,nout,status)

c=======================================================================
c     This procedure reads given set of event files and makes basic
c     calculations necessary for further processing
c     After it finished, there are properly initialised event list
c     ready for further analysis.
c
c     Sequence of steps:                                              
c     1. read all event files
c        put the result to common_events.inc (common block)
c     2. fill energy columns for each event list                      
c     3. read mkf file for each event list and prepare rigidity       
c        histogram for each ev.list                                   
c     4. fit detector offsets for wcs using DETX,DETY and SKYX,SKYY   
c        columns of each event list                                   
c
c     unit - i/o channel # to use      
c     status - return code, currently has no meaning, 
c              since we will stop in the case of any problem.
c                                                   
c     Created: Fri May 20 13:27:42 EDT 1994
c     Modified: Thu Sep 16 15:46:24 EDT 1999 (Ilana Harrus)
c     Bypass the call to def_det_off --
c     The offset in X and Y from the detector coordinate is extracted from 
c      the optical axis --  
c=======================================================================
       
      include 'common_evts.inc'

c
c---- file name
      character*(*) evfnami,rmfnami,mkfnami
      integer unit,status,nout

      integer fiostat

c==== read event file and fill the common_evts.inc variables.
       fiostat=0
       nout=0
       call read_events(unit,evfnami,fiostat,nout)
       if (fiostat.ne.0) stop

 
c==== fill in energy columns of event lists
      if(index(rmfnami,'dummy').eq.0) then
       print*,'Fill energy column '
       call fill_en(rmfnami,nout)
      endif

      if(index(mkfnami,'dummy').eq.0) then           
       print 10
 10   format(/' Prepare and store cut-off rigidity distributions')
       hrigstp=2
       call get_rig(mkfnami,hrig,hrigstp,nhrig)
      endif

c==== determine detector offsets (results are put in common_evt.inc)
       call def_det_off(nout)
c     The following program is a try to get the detector offsets correctly. 
c       call def_detoff(nout)

      return
      end







