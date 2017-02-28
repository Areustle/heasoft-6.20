*+GCFDIR
      SUBROUTINE GCFDIR(TELESCOP, INSTRUME, CIFDIR, LEN, STATUS)
      IMPLICIT NONE
      CHARACTER*(*) TELESCOP, INSTRUME
      CHARACTER*(*) CIFDIR
      INTEGER LEN, STATUS

C*****************************************************************
C Description:  GCFDIR uses the TELESCOP and INSTRUME values to 
C               determine the CALDB logical (environment variable)
C               and then translates the logical.  e.g.
C               TELESCOP=ASCA and INSTRUME=GIS --> asdgiscif -->
C               /caldb/data/astrod/gis/caldb.indx 
C               WARNING: Assumes that TELESCOP and INSTRUME are
C               uppercase strings!
C
C Arguments:    TELESCOP    (I) : The mission name
C               INSTRUME    (I) : The instrument name
C               CIFDIR      (o) : The translated logical
C               LEN         (o) : The length of the translated logical
C               STATUS      (o) : = 0     OK
C                                 = 1     Bad TELESCOP value
C                                 = 2     Bad INSTRUME value
C                                 = 3     logical is unassigned
C
C Origin:       Written for the Calibration Database
C
C Authors/Modification History:
C               Ron Zellar (1993 Feb 3) original version
C               Ron Zellar (1994 Feb 22) Added GENINSCIF
C
C******************************************************************
*-Version 1.0

       INTEGER LENGTH, FCSTLN, TELLEN, INSLEN, ERRSTAT
       character(80) LOGICAL

C      Initialize the status integer
       STATUS = 0

C      Translate the input telescop value into OGIP
C      CALDB standard
       CALL TRTELE(TELESCOP, TELLEN, ERRSTAT)

C      Translate the input instrument value into OGIP
C      CALDB statndard
       CALL TRINST(INSTRUME, INSLEN, ERRSTAT)

C      Determine the appropriate logical
       IF(TELESCOP(:TELLEN).EQ.'ASCA') THEN
            IF(INSTRUME(:3).EQ.'GIS')THEN
                 LOGICAL = 'ASDGISCIF'
            ELSE IF(INSTRUME(:3).EQ.'SIS') THEN
                 LOGICAL = 'ASDSISCIF'
            ELSE IF(INSTRUME(:3).EQ.'XRT') THEN
                 LOGICAL = 'ASDXRTCIF'
            ELSE 
                 STATUS = 2
                 RETURN
            ENDIF
            
       ELSE IF(TELESCOP(:TELLEN).EQ.'ROSAT') THEN
            IF(INSTRUME(:3).EQ.'HRI') THEN
                 LOGICAL = 'ROSHRICIF'
            ELSE IF(INSTRUME(:4).EQ.'PSPC') THEN
                 LOGICAL = 'ROSPSPCCIF'
            ELSE IF(INSTRUME(:3).EQ.'XRT') THEN
                 LOGICAL = 'ROSXRTCIF'
            ELSE
                 STATUS = 2
                 RETURN
            ENDIF

       ELSE IF(TELESCOP(:TELLEN).EQ.'ARIEL-V') THEN
            IF(INSTRUME(:3).EQ.'SSI') THEN
                 LOGICAL = 'ARVSSICIF'
            ELSE IF(INSTRUME(:3).EQ.'ASM') THEN
                 LOGICAL = 'ARVASMCIF'
            ELSE
                 STATUS = 2
                 RETURN
            ENDIF

       ELSE IF(TELESCOP(:TELLEN).EQ.'BBXRT') THEN
            IF(INSTRUME(:3).EQ.'XRT') THEN
                 LOGICAL = 'BBXXRTCIF'
            ELSE IF((INSTRUME(:1).EQ.'A').or.
     &              (INSTRUME(:1).EQ.'B'))THEN
                 LOGICAL = 'BBXDETCIF'
            ELSE
                 STATUS = 2
                 RETURN
            ENDIF 

       ELSE IF(TELESCOP(:TELLEN).EQ.'COS-B') THEN
            IF(INSTRUME(:5).EQ.'COS-B') THEN
                 LOGICAL = 'COSBCIF'
            ELSE
                 STATUS = 2
                 RETURN
            ENDIF 

       ELSE IF(TELESCOP(:TELLEN).EQ.'EINSTEIN') THEN
            IF(INSTRUME(:3).EQ.'HRI') THEN
                 LOGICAL = 'EINHRICIF'
            ELSE IF(INSTRUME(:3).EQ.'IPC') THEN
                 LOGICAL = 'EINIPCCIF'
            ELSE IF(INSTRUME(:3).EQ.'XRT') THEN
                 LOGICAL = 'EINXRTCIF'
            ELSE IF(INSTRUME(:3).EQ.'MPC') THEN
                 LOGICAL = 'EINMPCCIF'
            ELSE IF(INSTRUME(:3).EQ.'SSS') THEN
                 LOGICAL = 'EINSSSCIF'
            ELSE IF(INSTRUME(:3).EQ.'OGS') THEN
                 LOGICAL = 'EINOGSCIF'
            ELSE
                 STATUS = 2
                 RETURN
            ENDIF

       ELSE IF(TELESCOP(:TELLEN).EQ.'EXOSAT') THEN
            IF((INSTRUME(:4).EQ.'LEIT').or.
     &         (INSTRUME(:3).EQ.'TGS')) THEN
                 LOGICAL = 'EXOLECIF'
            ELSE IF(INSTRUME(:4).EQ.'GSPC') THEN
                 LOGICAL = 'EXOGSPCCIF'
            ELSE IF(INSTRUME(:2).EQ.'ME') THEN
                 LOGICAL = 'EXOMECIF'
            ELSE
                 STATUS = 2
                 RETURN
            ENDIF

       ELSE IF(TELESCOP(:TELLEN).EQ.'GINGA') THEN
            IF(INSTRUME(:3).EQ.'LAC') THEN
                 LOGICAL = 'GINLACCIF'
            ELSE IF(INSTRUME(:3).EQ.'ASM') THEN
                 LOGICAL = 'GINASMCIF'
            ELSE IF(INSTRUME(:3).EQ.'GBD') THEN
                 LOGICAL = 'GINGBDCIF'
            ELSE
                 STATUS = 2
                 RETURN
            ENDIF

       ELSE IF(TELESCOP(:TELLEN).EQ.'VELA5B')THEN
            IF(INSTRUME(:3).EQ.'ASM') THEN
                 LOGICAL = 'V5BASMCIF'
            ELSE
                 STATUS = 2
                 RETURN
            ENDIF

        ELSE IF(TELESCOP(:TELLEN).EQ.'HEAO-1') THEN
            IF(INSTRUME(:2).EQ.'A1') THEN
                 LOGICAL = 'HEAOA1CIF'
            ELSE IF(INSTRUME(:2).EQ.'A2') THEN
                 LOGICAL = 'HEAOA2CIF'
            ELSE IF(INSTRUME(:2).EQ.'A3') THEN
                 LOGICAL = 'HEAOA3CIF'
            ELSE IF(INSTRUME(:2).EQ.'A4') THEN
                 LOGICAL = 'HEAOA4CIF'
            ELSE
                 STATUS = 2
                 RETURN
            ENDIF

       ELSE IF(TELESCOP(:TELLEN).EQ.'CGRO') THEN
            IF(INSTRUME(:5).EQ.'BATSE') THEN
                 LOGICAL = 'GROBATCIF'
            ELSE IF(INSTRUME(:7).EQ.'COMPTEL') THEN
                 LOGICAL = 'GROCOMPCIF'
            ELSE IF(INSTRUME(:5).EQ.'EGRET') THEN
                 LOGICAL = 'GROEGRCIF'
            ELSE IF(INSTRUME(:4).EQ.'OSSE') THEN
                 LOGICAL = 'GROOSSECIF'
            ELSE
                 STATUS = 2
                 RETURN
            ENDIF

       ELSE IF(TELESCOP(:TELLEN).EQ.'GEN') THEN
            IF(INSTRUME(:3).EQ.'INS') THEN
                 LOGICAL = 'GENINSCIF'
            ELSE
                 STATUS = 2
                 RETURN
            ENDIF

       ELSE
            STATUS = 1
            RETURN       
       ENDIF

C      Translate the logical
       LENGTH = FCSTLN(LOGICAL)
       CALL CTRLOG(LOGICAL, LENGTH, CIFDIR, LEN)

       IF(LEN.EQ.0) STATUS=3
       
       RETURN
       END
