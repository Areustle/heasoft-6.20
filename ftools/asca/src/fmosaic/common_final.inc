C
C       common_final.inc : common block for image.
C
C       Ilana Harrus 1999
C
       integer nixm,niym       
       parameter(nixm=2000,niym=2000)
       real image,bkg,emap
       common/finalar/image(nixm,niym),bkg(nixm,niym)
     > ,emap(nixm,niym)

