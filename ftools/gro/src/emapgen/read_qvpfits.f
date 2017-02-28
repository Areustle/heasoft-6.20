C    ROUTINE READ_QVPFITS.F
C
C    Purpose: this routine reads a qvp file and fills variables that 
C    are defined in a common block.  That common block is in the  file
C    qvp_fitsio.cmn.inc and that commmon block must be included in this 
C    routine, as well as binevt.f, bpolar.f and brectn.f.  
C
C    Notes on the qvp fits file.  This files can contain photons or
C   photons binned according to pulsar phase.  The data are in the first 
C   extension in a binary table.  The list below indicates what is in 
C   each column of this table.   
C
C
C     This is the header from a typical qvp file:  
C
C
C 
C   XTENSION= 'BINTABLE'           / Binary table extension
C   BITPIX  =                    8 / 8-bit bytes
C   NAXIS   =                    2 / 2-dimensional binary table
C   NAXIS1  =                  130 / Width of table in bytes
C   NAXIS2  =                41594 / Number of rows in table
C   PCOUNT  =                    0 / No group parameters (required keyword)
C   GCOUNT  =                    1 / One data group (required keyword)
C   TFIELDS =                   43 / Number of fields in each row
C   EXTNAME = 'EGRET_SMDB'         / Unique CGRO extension name
C   TTYPE1  = 'MILLISECONDS'       / Milliseconds since UTC midnight
C   TFORM1  = 'J       '           / Fortran-77 format of field
C   TUNIT1  = '1.E-3_SEC'          / Units of field
C   TTYPE2  = 'MICROSECONDS'       / Microseconds since last millisecond
C   TFORM2  = 'I       '           / Fortran-77 format of field
C   TUNIT2  = '1.E-6_SEC'          / Units of field
C   TTYPE3  = 'TRUNC_JULIAN_DAY'   / Truncated Julian Day of arrival
C   TFORM3  = 'I       '           / Fortran-77 format of field
C   TUNIT3  = 'DAYS    '           / Units of field
C   TTYPE4  = 'S\C_INERT_X_COORD'  / Corrected S\C inertial X coordinate
C   TFORM4  = 'E       '           / Fortran-77 format of field
C   TUNIT4  = 'Km      '           / Units of field
C   TTYPE5  = 'S\C_INERT_Y_COORD'  / Corrected S\C inertial Y coordinate
C   TFORM5  = 'E       '           / Fortran-77 format of field
C   TUNIT5  = 'Km      '           / Units of field
C   TTYPE6  = 'S\C_INERT_Z_COORD'  / Corrected S\C inertial Z coordinate
C   TFORM6  = 'E       '           / Fortran-77 format of field
C   TUNIT6  = 'Km      '           / Units of field
C   TTYPE7  = 'B_&_C_PLANE_TAGS'   / B and C plane tile tags
C   TFORM7  = 'J       '           / Fortran-77 format of field
C   TUNIT7  = 'PDB_FORMAT'         / Units of field
C   TTYPE8  = 'COINCIDENCE_MODE'   / Coincidence dir\type mode
C   TFORM8  = 'I       '           / Fortran-77 format of field
C   TUNIT8  = 'DIR\TYPE'           / Units of field
C   TTYPE9  = 'PACKET_ERROR_FLAGS' / 16 1-bit error flags
C   TFORM9  = '2B      '           / Fortran-77 format of field
C   TUNIT9  = 'BIT     '           / Units of field
C   TTYPE10 = 'TASC_#1_FLAGS'      / TASC #1 status flags
C   TFORM10 = 'B       '           / Fortran-77 format of field
C   TUNIT10 = 'NONE    '           / Units of field
C   TTYPE11 = 'TASC_#2_FLAGS'      / TASC #2 status flags
C   TFORM11 = 'B       '           / Fortran-77 format of field
C   TUNIT11 = 'NONE    '           / Units of field
C   TTYPE12 = 'SPARE_FIELD'        / Reserved for future use
C   TFORM12 = 'I       '           / Fortran-77 format of field
C   TUNIT12 = 'NONE    '           / Units of field
C   TTYPE13 = 'GAMMA_X-Z_PROJ'     / Gamma ray X-Z plane angle
C   TFORM13 = 'E       '           / Fortran-77 format of field
C   TUNIT13 = 'RAD     '           / Units of field
C   TTYPE14 = 'GAMMA_Y-Z_PROJ'     / Gamma ray Y-Z plane angle
C   TFORM14 = 'E       '           / Fortran-77 format of field
C   TUNIT14 = 'RAD     '           / Units of field
C   TTYPE15 = 'ZENITH_DIRECTION'   / Gamma ray zenith angle
C   TFORM15 = 'E       '           / Fortran-77 format of field
C   TUNIT15 = 'RAD     '           / Units of field
C   TTYPE16 = 'AZIMUTH_DIRECTION'  / Gamma ray azimuthal angle from North
C   TFORM16 = 'E       '           / Fortran-77 format of field
C   TUNIT16 = 'RAD     '           / Units of field
C   TTYPE17 = 'RIGHT_ASCENSION'    / Gamma ray right ascension
C   TFORM17 = 'E       '           / Fortran-77 format of field
C   TUNIT17 = 'RAD     '           / Units of field
C   TTYPE18 = 'DECLINATION'        / Gamma ray declination
C   TFORM18 = 'E       '           / Fortran-77 format of field
C   TUNIT18 = 'RAD     '           / Units of field
C   TTYPE19 = 'GALACTIC_LATITUDE'  / Gamma ray galactic latitude
C   TFORM19 = 'E       '           / Fortran-77 format of field
C   TUNIT19 = 'RAD     '           / Units of field
C   TTYPE20 = 'GALACTIC_LONGITUDE' / Gamma ray galactic longitude
C   TFORM20 = 'E       '           / Fortran-77 format of field
C   TUNIT20 = 'RAD     '           / Units of field
C   TTYPE21 = 'GAMMA_RAY_ENERGY'   / Energy content of gamma ray
C   TFORM21 = 'E       '           / Fortran-77 format of field
C   TUNIT21 = 'MEV     '           / Units of field
C   TTYPE22 = 'ENERGY_UNCERTAINTY' / Uncertainty in gamma ray energy
C   TFORM22 = 'E       '           / Fortran-77 format of field
C   TUNIT22 = 'MEV     '           / Units of field
C   TTYPE23 = 'SPARE_FIELD'        / Reserved for future use
C   TFORM23 = 'J       '           / Fortran-77 format of field
C   TUNIT23 = 'NONE    '           / Units of field
C   TTYPE24 = 'SPARE_FIELD'        / Reserved for future use
C   TFORM24 = 'J       '           / Fortran-77 format of field
C   TUNIT24 = 'NONE    '           / Units of field
C   TTYPE25 = 'BARY_DAY_FRAC'      / Photon arrival time at Barycenter
C   TFORM25 = 'D       '           / Fortran-77 format of field
C   TUNIT25 = 'DAY     '           / Units of field
C   TTYPE26 = 'BINARY_PHASE'       / Binary orbital phase from 0 to 1
C   TFORM26 = 'E       '           / Fortran-77 format of field
C   TUNIT26 = 'NONE    '           / Units of field
C   TTYPE27 = 'PULSAR_PHASE'       / Pulsar rotation phase from 0 to 1
C   TFORM27 = 'E       '           / Fortran-77 format of field
C   TUNIT27 = 'NONE    '           / Units of field
C   TTYPE28 = 'PULSAR_RA'          / Right ascension of pulsar
C   TFORM28 = 'E       '           / Fortran-77 format of field
C   TUNIT28 = 'RAD     '           / Units of field
C   TTYPE29 = 'PULSAR_DEC'         / Declination of pulsar
C   TFORM29 = 'E       '           / Fortran-77 format of field
C   TUNIT29 = 'RAD     '           / Units of field
C   TTYPE30 = 'BARY_VECT_X'        / Barycenter X coordinate
C   TFORM30 = 'J       '           / Fortran-77 format of field
C   TUNIT30 = 'LIGHT_MICROSEC'     / Units of field
C   TTYPE31 = 'BARY_VECT_Y'        / Barycenter Y coordinate
C   TFORM31 = 'J       '           / Fortran-77 format of field
C   TUNIT31 = 'LIGHT_MICROSEC'     / Units of field
C   TTYPE32 = 'BARY_VECT_Z'        / Barycenter Z coordinate
C   TFORM32 = 'J       '           / Fortran-77 format of field
C   TUNIT32 = 'LIGHT_MICROSEC'     / Units of field
C   TTYPE33 = 'STRUCT_ANAL_WORD'   / Stucture analysis word
C   TFORM33 = 'B       '           / Fortran-77 format of field
C   TUNIT33 = 'PROCESS_RET_CODE'   / Units of field
C   TTYPE34 = 'SINGLE  '           / SINGLE
C   TFORM34 = 'B       '           / Fortran-77 format of field
C   TUNIT34 = 'PROCESS_RET_CODE'   / Units of field
C   TTYPE35 = 'SAGE    '           / SAGE
C   TFORM35 = 'B       '           / Fortran-77 format of field
C   TUNIT35 = 'PROCESS_RET_CODE'   / Units of field
C   TTYPE36 = 'STRUCT_FLAGS_#1'    / Event structure flags #1
C   TFORM36 = 'B       '           / Fortran-77 format of field
C   TUNIT36 = 'PROCESS_RET_CODE'   / Units of field
C   TTYPE37 = 'STRUCT_FLAGS_#2'    / Event structure flags #2
C   TFORM37 = 'B       '           / Fortran-77 format of field
C   TUNIT37 = 'PROCESS_RET_CODE'   / Units of field
C   TTYPE38 = 'SCATR   '           / SCATR
C   TFORM38 = 'B       '           / Fortran-77 format of field
C   TUNIT38 = 'PROCESS_RET_CODE'   / Units of field
C   TTYPE39 = 'ENERGY  '           / ENERGY
C   TFORM39 = 'B       '           / Fortran-77 format of field
C   TUNIT39 = 'PROCESS_RET_CODE'   / Units of field
C   TTYPE40 = 'DIRCTN  '           / DIRCTN
C   TFORM40 = 'B       '           / Fortran-77 format of field
C   TUNIT40 = 'PROCESS_RET_CODE'   / Units of field
C   TTYPE41 = 'SPARE_FIELD'        / Reserved for future use
C   TFORM41 = 'I       '           / Fortran-77 format of field
C   TUNIT41 = 'NONE    '           / Units of field
C   TTYPE42 = 'SPARE_FIELD'        / Reserved for future use
C   TFORM42 = 'I       '           / Fortran-77 format of field
C   TUNIT42 = 'NONE    '           / Units of field
C   TTYPE43 = 'SPARE_FIELD'        / Reserved for future use
C   TFORM43 = 'I       '           / Fortran-77 format of field
C   TUNIT43 = 'NONE    '           / Units of field
C   END
C
C
C   Variables used in this program:
C  READ_LUN: This is the logical number of the fits file that 
C            the data will be read from.  This file was opened 
C            outside of READ_QVPFITS.
C  ROW_NUM:  This is the row of the binary table that will be read.
C  STATUS:  This stores the fitsio error flag.
C  EVTMSD:  This outputs: milliseconds since UTC midnight
C  EVTTJD:  This outputs: truncated Julian Day of arrival
C  TASC:    This 2 element vector outputs an integer*2 number fron 1 
C           to 255.  If this number were in binary form the fifth
C           bit being high in either one of the elements of the array
C           signals that there was not a 6 MEV detection tin the tasc. 
C  EVTPOS:  This * element vetor pulls columns 12 through 20 of a 
C           row.  These are the coordinates of GRO in several systems.
C  EVTENG:  This outputs:  Energy content of gamma ray
C  EVTPUL:  This outputs:  Pulsar rotation phase from 0 to 1 
C  EVTBIN:  This outputs:  Binary orbital phase from 0 to 1
C
C  Written by J. Silvis
C  August/Sept 1998 
C
C

      SUBROUTINE READ_QVPFITS(READ_LUN,ROW_NUM,STATUS,
     &   EVTMSD,EVTTJD,TASC,EVTPOS,EVTENG,EVTPUL,EVTBIN)

      implicit none 

      INTEGER READ_LUN,ROW_NUM,STATUS,i
      integer felem,nelems,nullj,colnum
      logical anynull
      real nulle      

      INTEGER    EVTMSD
      integer*2  EVTTJD,TASC(2),NULLI
      real       evtpos(8),evteng,evtbin,evtpul

C
C   This assigns the first column of the binary table the number 1.
C
      felem=1
      STATUS = 0
C
C   This tells fitsio to only pull one element from a given column.
C
      nelems=1
C
C    Sets the null values for the various datatypes.
C
      nullj=0
      nulli=0
      nulle=0.


C
C    This pulls the "microseconds since the last millesecond"
C

      colnum=1
      call ftgcvj(read_lun,colnum,row_num,felem,nelems,nullj,evtmsd,
     &                    anynull,status)
     
C
C   This pulls the truncated julian day of the arrival.
C
      colnum=3
      call ftgcvi(read_lun,colnum,row_num,felem,nelems,nulli,evttjd,
     &                    anynull,status)
C
C       
C  The bytes in the 10 and 11 column give the status of the 
C TASC.  The tasc has two divisions, so there are two bytes.
C If the fifth bit of either of these bytes is high it means
C that the tasc did not detect 6 MeV.  The code below will 
C take out one single byte and place it in a integer*2 variable
C which will take on a value from 0 to 255.  Later the fifth
C bit will be examined to see if it is high.
C
      colnum=10
      call ftgcvi(read_lun,colnum,row_num,felem,nelems,nulli,
     &       tasc(1),anynull,status)
      colnum=11
      call ftgcvi(read_lun,colnum,row_num,felem,nelems,nulli,
     &       tasc(2),anynull,status)
C
C  This code pulls the event coordinates.
C

      Do 10 i = 1, 8
         call ftgcve(read_lun,i+12,row_num,felem,nelems,nulle,
     &              evtpos(i),anynull,status)
10    continue
C
C   This pulls the event energy
C
      colnum = 21
      call ftgcve(read_lun,colnum,row_num,felem,nelems,nulle,
     &              evteng,anynull,status)      

C
C   Pulls binary orbital phase from 0 to 1
C 
C
      colnum = 26
      call ftgcve(read_lun,colnum,row_num,felem,nelems,nulle,
     &              evtbin,anynull,status)      

C
C    Pulsar rotational phase from 0 to 1
C
      colnum = 27
      call ftgcve(read_lun,colnum,row_num,felem,nelems,nulle,
     &              evtpul,anynull,status)      

      RETURN
CHCCCCCCCCCCCCCCCCCCCCC END MAPGEN.SOURCE() CCCCCCCCCCCCCCCCCCCCCC
      END
