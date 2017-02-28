C
C       common_keywords.inc : common for the keywords.
C
C       Ilana Harrus 2000
C
c-----to adapt this ftools to all detectors.

      integer tfield,iskyx,iskyy,ipha,idetx,idety
     + ,phabins1,phabins2
      common/skeywrdi/tfield,iskyx,iskyy,ipha,idetx,idety
     + ,phabins1,phabins2

      character valuekey*80,namekey*8,namekw*8
      character tlmaxdx*8,tlmaxdy*8,tlmaxpha*8,tlminpha*8
     + ,opticdx*8,opticdy*8,opticxx*8,opticxy*8,tcrpxdx*8
     + ,tcrpxdy*8,tctypx*8
     + ,tctypy*8,tcdltx*8,tcdlty*8,tcrvlx*8,tcrvly*8
     + ,tcrpxx*8,tcrpxy*8,tcrpxcx*8,tcrpxcy*8

      common/skeywrdc/valuekey,namekey,namekw,tlmaxdx,tlmaxdy
     + ,tlmaxpha,tlminpha,opticdx,opticdy,opticxx,opticxy
     + ,tcrpxdx,tcrpxdy
     + ,tctypx,tctypy,tcdltx,tcdlty,tcrvlx,tcrvly,tcrpxx,tcrpxy
     + ,tcrpxcx,tcrpxcy