c==============================================================         
      subroutine get_bcg_fn(id_det,rig,fn)
c--------------------------------------------------------------         
c     Define the name of the file with background events for
c     given ID_DET (0,1,2,3)
c     given RIG (rigidity --> COR_MIN parameter)
c
c     fn - full name (with path) of the appropriate BCG.EVT file
c
c     Note:
c     2. RIG - corresponds to the average rigidity 
c     Created: Tue May 17 20:04:01 EDT 1994
c--------------------------------------------------------------         
      implicit none
      include 'common_evts.inc'
      include 'common_par.inc'
      integer id_det
      real rig
      character*(*) fn

      character(200) head,mid,tail,bcgdir
      integer ir,kh,km,kt,ki
      integer kd
      integer len_trim

         ki=len_trim(telescop)
         call locase(telescop)
c---- CALDB data base directory
         call getenv('CALDB',bcgdir) 
         kd=len_trim(bcgdir)

c---- SIS
         if(id_det.le.1) then
         ir=nint(rig/2.)*2
         if(ir.gt.14) ir=14
         if(ir.lt.6) ir=6
         write(head,15)id_det,ir
  15     format('/sis/bcf/bgd/94nov/s',i1,'bgd_',i2.2,'i.evt')
         kh=len_trim(head)
         fn=bcgdir(:kd)//'/data/'//telescop(:ki)//head(:kh)
c  problem with bcgdir and format... 
c         write(fn,10)bcgdir(:kd),telescop(:ki),id_det,ir
c 10      format (a<ki>,'/data/',a<ki>,'/sis/bcf/bgd/94nov/s',
c     > i1,'bgd_',i2.2,'i.evt')
c   Swith to fixed format... 
c         write(fn,10)telescop(:ki),id_det,ir
c 10      format ('/FTP/caldb/data/',a4,'/sis/bcf/bgd/94nov/s',
c     > i1,'bgd_',i2.2,'i.evt')
         goto 100
      endif
c---- GIS
      if(id_det.ge.2) then
c....... Head of the file name
         write(head,20)id_det
 20      format('/gis/bcf/bgd/94may/blanksky_g',i1,'_')
         kh=len_trim(head)
c....... Middle of the file name 
         if(rig.lt.4) mid='cor4'
         if(rig.ge.4.and.rig.lt.6) mid='4cor6'
         if(rig.ge.6.and.rig.lt.8) mid='6cor8'
         if(rig.ge.8.and.rig.lt.10) mid='8cor10'
         if(rig.ge.10.and.rig.lt.12) mid='10cor12'
         if(rig.ge.12.and.rig.le.14) mid='12cor14'
         if(rig.gt.14) mid='14cor'
         km=len_trim(mid)
c....... Tail of the file name 
         tail='_v2.evt'
         kt=len_trim(tail)
c....... Now construct the full name
c same problem as above. forget the variable character length...
c         write(fn,30)bcgdir(:kd),telescop(:ki),head(:kh),mid(:km)
c     >,tail(:kt)
c 30      format
c     > (a<kd>,'/data/',a<ki>,a<kh>,a<km>,a<kt>)
c
         fn=bcgdir(:kd)//'/data/'//telescop(:ki)
     &                           //head(:kh)//mid(:km)//tail(:kt)
c         if(km.eq.4) then
c         write(fn,30)telescop(:ki),head(:kh),mid(:km)
c     >,tail(:kt)
c         elseif(km.eq.5) then
c         write(fn,31)telescop(:ki),head(:kh),mid(:km)
c     >,tail(:kt)
c         elseif(km.eq.6) then
c         write(fn,32)telescop(:ki),head(:kh),mid(:km)
c     >,tail(:kt)
c         elseif(km.eq.7) then
c         write(fn,33)telescop(:ki),head(:kh),mid(:km)
c     >,tail(:kt)
c         endif

c 30     format('/FTP/caldb/data/',a4,a31,a4,a7)
c 31     format('/FTP/caldb/data/',a4,a31,a5,a7)
c 32     format('/FTP/caldb/data/',a4,a31,a6,a7)
c 33     format('/FTP/caldb/data/',a4,a31,a7,a7)


         goto 100
      endif


 100  continue
      
      return
      end

