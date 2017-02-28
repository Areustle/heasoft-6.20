c
c  Split out itel<->satellite/detector mapping so it
c  could be accessed as before
c
      include '../lib/null.inc'
      include '../include/startup.inc'
c
c  Mission Database
c
      real*8 dbl_mdb
      integer*4 int_mdb
      character(80) str_mdb
c
c  Mission Database Keys
c
      integer*4 mdbd_num, mdbs_num, mdbi_num
      parameter (mdbd_num=6, mdbs_num=8, mdbi_num=7)
      character(10) mdbd_keys(mdbd_num), mdbs_keys(mdbs_num), 
     &             mdbi_keys(mdbi_num)
      real*8 mdbd_defs(mdbd_num)
      character(80) mdbs_defs(mdbs_num)
      integer*4 mdbi_defs(mdbi_num)

      integer*4 gmchat
      parameter ( gmchat = 50 )

      data mdbd_keys /'DRPIX1','DRPIX2','PIXSIZE','DOPTIC1','DOPTIC2',
     &                'ERRRAD'/

      data mdbd_defs /DNUL, DNUL, 0., DNUL, DNUL, -1./

      data mdbs_keys /'XCOL','YCOL','ECOL','GTIEXT','EXCHAN','PSFFILE',
     &                'ENGFILE','VIGNFILE'/

      data mdbs_defs /'X','Y',' ',' ','-1,-1,-1,-1',' ',' ',' '/

      data mdbi_keys /'DEFSZX','DEFSZY','DEFZOOM','ENUM','EMIN','EMAX',
     &                'DETBOX'/

      data mdbi_defs /0, 0, 0, 0, 0, 0, 8/

      common /nmdb/     dbl_mdb(mdbd_num,ZEXpmax),
     &                  int_mdb(mdbi_num,ZEXpmax)
      common /smdb/     str_mdb(mdbs_num,ZEXpmax)
