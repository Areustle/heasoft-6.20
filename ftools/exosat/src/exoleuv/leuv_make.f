**==LEUV_MAKE.spg  processed by SPAG 3.09I  at 15:10 on 24 Aug 1994
      SUBROUTINE LEUV_MAKE(Filter_code,Spectral_type,V,Colour_excess,
     &                     Ichat ,Ierr)
c
c I Filter_code       main filter
c I spectral_type     main Sptype
c I v                 main Vmag
c I colour_excess     main evmag
c O ierr
c
c
      REAL*4 object_class
      REAL*4 V , Colour_excess
      REAL*4 rate, Filter_code
      INTEGER*4 status, ierr, Ichat
      INTEGER*4 i, OK__,ERROR__
      PARAMETER (OK__=0)
      PARAMETER (ERROR__=1)
      character(80) o, Spectral_type
c External references :
      REAL*4 EXOSAT_UV
 
c
      READ (Spectral_type,*,IOSTAT=status) object_class
      IF ( status.NE.OK__ ) THEN
         status = OK__
         READ (Spectral_type(2:2),'(i1)',IOSTAT=status) i
         IF ( status.EQ.OK__ ) THEN
            object_class = REAL(i)*10. + 2000.
            CALL UPC(Spectral_type)
            IF ( Spectral_type(1:1).EQ.'O' ) THEN
               object_class = object_class + 100.
            ELSEIF ( Spectral_type(1:1).EQ.'B' ) THEN
               object_class = object_class + 200.
            ELSEIF ( Spectral_type(1:1).EQ.'A' ) THEN
               object_class = object_class + 300.
            ELSE
               status = ERROR__
            ENDIF
         ENDIF
      ENDIF
      IF ( status.EQ.OK__ ) THEN
         rate = EXOSAT_UV(Filter_code,object_class,V,Colour_excess)
         WRITE (o,'(''Expected count rate ='',1pe9.3)') rate
      ELSE
         o = ' Error in spectral type'
      ENDIF
      CALL XWRITE(o,ichat)
c      CALL XWRITE('test low chat',8)
c      CALL XWRITE('test high chat',20)
c      CALL XWRITE('test higher chat',30)
      END
