C*******************************************************************
      SUBROUTINE write_write(fits_unit,newrec1,newrec2,status)
C*******************************************************************
C     
      include 'xrrbstr.h'
C
C     Input parameters:
      integer           fits_unit
      RECORD/rnewbuf_rec_1/newrec1      
      RECORD/rnewbuf_rec_2/newrec2      
      RECORD/rnewbuf_me/newme
      RECORD/rnewbuf_gs/newgs
      RECORD/rnewbuf_le/newle
c
c     Output parameters:
      integer       status
c
      character(70) comm1
      INTEGER exp
c
      newme=newrec2.newme
      newle=newrec2.newle
c
       comm1=' '
       CALL FTPCOM(fits_unit,comm1, status)
       comm1=
     &  'This lightcurve was originally generated '//
     &  'by the EXOSAT Observatory '
       CALL FTPCOM(fits_unit,comm1, status)
       comm1=' '
       comm1=
     &  'as part of a standard processing of the EXOSAT data.'//
     &  ' It was'
       CALL FTPCOM(fits_unit,comm1, status)
       comm1=' '
       comm1='converted to FITS format by the High Energy '// 
     &       'Science Archive '
       CALL FTPCOM(fits_unit,comm1, status)
       comm1=' '
       comm1= 'Research Center. The header of this file in '//
     &        'the original '
       CALL FTPCOM(fits_unit,comm1, status)
       comm1=' '
       comm1= 'rate buffer format is listed below.'
       CALL FTPCOM(fits_unit,comm1, status)
c
       exp=newrec1.expt 
       if (exp.eq.1.or.exp.eq.2)then
               comm1=' '
              CALL FTPCOM(fits_unit,comm1, status)
          if(newle.bgflag.eq.0.and.newle.sbflag.eq.0)then
                comm1=' '
                comm1=
     &  'This file contains the light curve of the background for the'
              CALL FTPCOM(fits_unit,comm1, status)
                comm1=' '
                comm1=
     &  'imaging instrument LE. The data were obtained selecting'
              CALL FTPCOM(fits_unit,comm1, status)
                comm1=' '
                comm1=
     &  'a region in the image free of sources. The OBJECT keyword'
              CALL FTPCOM(fits_unit,comm1, status)
                comm1=' '
                comm1=
     &  'contains the name of the pointed field.'
              CALL FTPCOM(fits_unit,comm1, status)
c                                                                    
          elseif(newle.bgflag.eq.1.and.newle.sbflag.eq.1)then
                comm1=' '
                comm1=
     &  'This file contains the light curve of a detected source '
              CALL FTPCOM(fits_unit,comm1, status)
                comm1=' '
                comm1=
     &  'within a LE image. The data were obtained selecting a region'
              CALL FTPCOM(fits_unit,comm1, status)
                comm1=' '
                comm1=
     &  'region center on the X,Y position of the detection. The OBJECT'
              CALL FTPCOM(fits_unit,comm1, status)
                comm1=' '
                comm1=
     &  'keyword contains the name of the pointed field which might '
              CALL FTPCOM(fits_unit,comm1, status)
                comm1=' '
                comm1=
     &  'not correspond to the real source name.' 
              CALL FTPCOM(fits_unit,comm1, status)
          endif
       elseif(exp.eq.3)then
         if(newme.bgflag.eq.0) then
            comm1=' '
            CALL FTPCOM(fits_unit,comm1, status)
            comm1=' '
            comm1=
     & 'The observation with the ME experiment were usually carried out'
             CALL FTPCOM(fits_unit,comm1, status)
            comm1=' '
            comm1=
     & 'with one half on source and one half offset (a few degree) to'
             CALL FTPCOM(fits_unit,comm1, status)
            comm1=' '
            comm1=
     & 'monitor the background. This file contains data from the half'
             CALL FTPCOM(fits_unit,comm1, status)
            comm1=' '
            comm1=
     & 'offset. The offset value was choosen to be a region of the sky'
             CALL FTPCOM(fits_unit,comm1, status)
            comm1=' '
            comm1=
     & 'free of sources. The pointing position was not kept in the '
             CALL FTPCOM(fits_unit,comm1, status)
            comm1=' '
            comm1=
     & 'original file. In the keywords OBJECT, RA_SRC, DEC_SRC, RA_PNT,'
             CALL FTPCOM(fits_unit,comm1, status)
            comm1=' '
            comm1=
     & 'DEC_PNT are stored the value correspoding to the half '
             CALL FTPCOM(fits_unit,comm1, status)
            comm1=' '
            comm1=
     & 'on source, which is the only information available.'
             CALL FTPCOM(fits_unit,comm1, status)
         endif
      endif 
            comm1=' '
            CALL FTPCOM(fits_unit,comm1, status)
      RETURN
      END
