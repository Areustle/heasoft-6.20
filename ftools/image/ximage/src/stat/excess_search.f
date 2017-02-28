      SUBROUTINE EXCESS_SEARCH(Map, Szx, Szy, Mapid, Pex, Bright, 
     &                         User_threshold_scal, Status)
      IMPLICIT NONE
c
c  Excess search algorithm
c
c  I  map                 (r)  Image map
c  I  szx/y               (i)  Size of map
c  I  mapid               (s)  Map id string
c  I  pex                 (i)  Whether to plot excesses
c  I  bright              (i)  Whether to use old contig search
c  I  user_threshold_scal (r)  Threshold scaling
c  O  status              (i)  Error flag (0 = OK)
c
      INTEGER*4 Szx, Szy
      real*4 Map(Szx,Szy)
      character*(*) Mapid
      LOGICAL Pex, Bright
      REAL*4 User_threshold_scal
      INTEGER*4 Status

      INCLUDE '../include/io.inc'
      INCLUDE 'backgd.inc'
      INCLUDE 'excess.inc'
c
c  Local variables
c
      INTEGER*4 tr
c
c set threshold for initial search and scale it if requested by user
c
      CALL SET_THRESHOLD(BNEw,BOXsiz,Szx,Szy,tr)
      tr = tr*User_threshold_scal
      write(ZWRite,*) ' Threshold : ', tr
      call XWRITE(ZWRite, 15)
c
c sliding cell search
c
      CALL XWRITE(' >>>>> Searching for excesses',10)
      CALL LOC_EXCESS(Map, Szx, Szy, Mapid, Pex, tr, Status)
      IF ( Status.NE.0 ) THEN
         NUMexs = 0
         Status = 1
         RETURN
      ENDIF
c
c search for contigous excesses
c
      CALL XWRITE(' >>>>> Removing contiguous sources',10)
      call contig_srch(Mapid, Bright, Pex, tr)
 
      WRITE (ZWRite,99001) NUMexs
      CALL XWRITE(ZWRite,10)

      RETURN
99001 FORMAT (' ',i10,' excesses left')
      END
