**==readraw.spg  processed by SPAG 4.50J  at 16:32 on 17 Oct 1998
C Author: Jesse Allen
C History:
C  Version 0.0  29 Mar 1998
CH  Lorraine Breedon (1.1.0 03 Dec 1998) replaced Fitsunit with rawunit \
CH                                       accounted for ANTI_COEFF column
CH                                       being = 11 rather than 10 \

C  This is a subroutine to read a HEAO-1 A-2 raw DSDISK FITS data file
C
      SUBROUTINE READRAW(rawunit,Irn,Jtyp,Stim,Etim,Dir,Lbuf,Status)

c rawunit I  i      logical unit no for raw datafile

c Irn     I  i      Record no of raw datafile
c Jtyp    O  i*2    Value indicates the data column no 
c                   Jtyp = 0 corresponds to data cols 1-3
c                   Jtyp = 1-11 corresponds to data cols 5-15
c
c Stim    O  r*4    start time in data column (days of 1977)       
c Etim    O  r*4    end time in data column (days of 1977)
c Dir     O  r*4    average vector posn of the satellite
c Lbuf    O  i*2    data arrays for columns 5-15
 
 
      IMPLICIT NONE
 
      LOGICAL anyf
 
      INTEGER*2 Jtyp , Lbuf(1440)
      INTEGER rawunit , Irn , row , Status
 
      REAL Stim , Etim , Dir(3,3)
 
 
      Status = 0
      row = INT((Irn-3)/12) + 1
       Jtyp = Irn - ((row-1)*12) - 3
      IF ( Jtyp.EQ.0 ) THEN
         CALL FTGCVE(rawunit,2,row,1,1,0.0,Stim,anyf,Status)
         CALL FTGCVE(rawunit,3,row,1,1,0.0,Etim,anyf,Status)
         CALL FTGCVE(rawunit,4,row,1,9,0.0,Dir,anyf,Status)
      ELSEIF ( (Jtyp.GE.1) .AND. (Jtyp.LE.11) ) THEN
         CALL FTGCVI(rawunit,(4+Jtyp),row,1,1440,0,Lbuf,anyf,Status)
      ENDIF
 
      RETURN
 
      END
