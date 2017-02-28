c
c NOTE: Always include maxvals.inc before header.inc
c
c  Headers
c
      real*8 dbl_hdr
      character(80) chr_hdr
      integer*4 int_hdr
c
c  Header keys
c
      integer*4 hdr_num, dbl_num, chr_num, int_num
      parameter (hdr_num=MAX_MAPS, dbl_num=27, chr_num=19, int_num=8)
      character(10) dbl_keys(dbl_num), 
     &             chr_keys(chr_num), int_keys(int_num)

      integer*4 ghchat 
      parameter ( ghchat = 50 )

      data dbl_keys /'BSCALE','BZERO','CRVAL1','CRVAL2','CROTA2',
     &               'CDELT1','CDELT2','DDELT1','DDELT2',
     &               'OCRPIX1','OCRPIX2','OCRVAL1','OCRVAL2',
     &               'CRPIX1','CRPIX2','DRPIX1','DRPIX2','EQUINOX',
     &               'EXPOSURE','DTIME','DATAMIN','DATAMAX',
     &               'XIMNORTH','XIMSCALE','XIMZERO','ZMX','ZMY'/

      data chr_keys /'BUNIT','EXMAPID','MAPTYPE','FILE','ROOT','OBJECT',
     &               'CTYPE1','CTYPE2','CUNIT1','CUNIT2','TELESCOP',
     &               'INSTRUME','DETNAM','FILTER','DATE-OBS','DATE-END',
     &               'MAPCODES','MAPCOPY','WCSID'/

      data int_keys /'LOADED','MAPPTR','SZX','SZY','EMIN',
     &               'EMAX','EXPMAP','VIGNAPP'/

      common /nheaders/  dbl_hdr(dbl_num,hdr_num),
     &                   int_hdr(int_num,hdr_num)
      common /cheader/   chr_hdr(chr_num,hdr_num)
