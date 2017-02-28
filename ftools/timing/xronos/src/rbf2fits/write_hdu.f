C*******************************************************************
      SUBROUTINE write_hdu(fits_unit,newrec1,newrec2,status)
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
c     Output parameters:
      integer       status

c
c     Local parameters:
      INTEGER exp, type, lenact
c
      character(8)   HDUCLASS_KEY
      character(40)  HDUCLASS_COM
      character(40)  HDUCLASS_VAR
      PARAMETER(HDUCLASS_KEY='HDUCLASS')
      PARAMETER(HDUCLASS_COM='format conform to the OGIP standard')
c
      character(8)   HDUVERS_KEY
      character(40)  HDUVERS_COM
      character(40)  HDUVERS_VAR
      PARAMETER(HDUVERS_KEY='HDUVERS')
      PARAMETER(HDUVERS_COM='Version of format (OGIP memo 93/003a')
c
      character(8)   HDUCLAS1_KEY
      character(45)  HDUCLAS1_COM
      character(40)  HDUCLAS1_VAR
      PARAMETER(HDUCLAS1_KEY='HDUCLAS1')
      PARAMETER(
     &   HDUCLAS1_COM='LIGHT CURVE dataset (OGIP memo 93/003)')
c
      character(8)   HDUCLAS2_KEY
      character(40)  HDUCLAS2_COM
      character(40)  HDUCLAS2_VAR
      PARAMETER(HDUCLAS2_KEY='HDUCLAS2')
c
      character(8)   HDUCLAS3_KEY
      character(40)  HDUCLAS3_COM
      character(40)  HDUCLAS3_VAR
      PARAMETER(HDUCLAS3_KEY='HDUCLAS3')
c
      character(8)   TIMVERSN_KEY
      character(40)  TIMVERSN_COM
      PARAMETER(TIMVERSN_KEY='TIMVERSN')
      PARAMETER(TIMVERSN_COM='OGIP memo number for file format')
c
      character(8)   AUTHOR_KEY
      character(20)  AUTHOR_VAL
      character(40)  AUTHOR_COM
      PARAMETER(AUTHOR_KEY='CREATOR')
      PARAMETER(AUTHOR_COM='program that created this file')
c
      HDUCLASS_VAR='OGIP'
      CALL FTPKYS(fits_unit,HDUCLASS_KEY,
     &     HDUCLASS_VAR(1:LENACT(HDUCLASS_VAR)),HDUCLASS_COM, status)
      HDUVERS_VAR='1.1.0'
      CALL FTPKYS(fits_unit,HDUVERS_KEY,
     &     HDUVERS_VAR(1:LENACT(HDUVERS_VAR)),HDUVERS_COM, status)
      HDUCLAS1_VAR='LIGHTCURVE'
      CALL FTPKYS(fits_unit,HDUCLAS1_KEY,
     &     HDUCLAS1_VAR(1:LENACT(HDUCLAS1_VAR)),HDUCLAS1_COM, status)
c
c
      newgs=newrec2.newgs
      newme=newrec2.newme
      newle=newrec2.newle
      exp=newrec1.expt
c
      if (exp.eq.1.or.exp.eq.2) then
         if(newle.bgflag.eq.0.and.newle.sbflag.eq.0)then 
           HDUCLAS2_VAR='BKG'
           HDUCLAS2_COM='Light Curve of background'
         elseif(newle.bgflag.eq.1.and.newle.sbflag.eq.1)then
           HDUCLAS2_VAR='NET'
           HDUCLAS2_COM='Bkg-subtracted Light Curve'
         endif
      elseif(exp.eq.3) then
         if(newme.bgflag.eq.0) then  
           HDUCLAS2_VAR='TOTAL'
           HDUCLAS2_COM='Light Curve source+back'
         else
           HDUCLAS2_VAR='NET'
           HDUCLAS2_COM='Bkg-subtracted Light Curve'
         endif
      elseif(exp.eq.4) then
           HDUCLAS2_VAR='NET'
           HDUCLAS2_COM='Bkg-subtracted Light Curve'
      elseif(exp.eq.5) then
           HDUCLAS2_VAR='NET'
           HDUCLAS2_COM='Bkg-subtracted Light Curve'
      elseif(exp.eq.6) then
           HDUCLAS2_VAR='NET'
           HDUCLAS2_COM='Bkg-subtracted Light Curve'
      endif
      CALL FTPKYS(fits_unit,HDUCLAS2_KEY,
     &     HDUCLAS2_VAR(1:LENACT(HDUCLAS2_VAR)),HDUCLAS2_COM, status)
c
      type=newrec1.type
      if (type.eq.0.or.type.eq.2) then
         HDUCLAS3_VAR='RATE'
         HDUCLAS3_COM='Light Curve data stored as count/s'
      elseif (type.eq.1) then
         HDUCLAS3_VAR='COUNT'
         HDUCLAS3_COM='Light Curve data stored as count'
      endif
      CALL FTPKYS(fits_unit,HDUCLAS3_KEY,
     &     HDUCLAS3_VAR(1:LENACT(HDUCLAS3_VAR)),HDUCLAS3_COM, status)
     
      CALL FTPKYS(fits_unit, TIMVERSN_KEY, 'OGIP 93/003',
     &            TIMVERSN_COM, status)
      AUTHOR_VAL='rbf2fits v1.0'
      CALL FTPKYS(fits_unit,AUTHOR_KEY, AUTHOR_VAL, 
     &            AUTHOR_COM, status)

      RETURN
      END

