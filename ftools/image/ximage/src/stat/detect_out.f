      SUBROUTINE DETECT_OUT(Mapid, Equinox, Filedet, Fitsdet, Status)
      IMPLICIT NONE
c
c    Final part of detect
c    Writes output to screen and files
c
c  I  mapid    (s)   Map id string
c  I  equinox  (i)   XIMAGE equinox
c  I  filedet  (s)   Plain text output file
c  I  fitsdet  (s)   FITS output file (source list)
c  O  status   (i)   Error flag (0=OK)
c
      INTEGER*4 Equinox, Status
      CHARACTER*(*) Mapid, Filedet, Fitsdet

c     INCLUDE '../include/io.inc'
c     INCLUDE '../include/startup.inc'
c     INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/dynmem.inc'
c     INCLUDE 'backgd.inc'
      INCLUDE 'detect.inc'
c
c  Local variables
c
      integer p_cnt, p_err, p_x, p_y, p_vcor, p_ra, p_dec
      integer p_erad, p_hbox, p_prob, p_snr

      IF ( NUMdet.EQ.0 ) THEN
         CALL XWRITE(' No sources detected',10)
         RETURN
      ENDIF
c
c  Allocate buffers based on number of detected sources
c
      call ralloc(1, NUMdet, 1, p_cnt, status)
      call ralloc(1, NUMdet, 1, p_err, status)
      call ralloc(1, NUMdet, 1, p_x, status)
      call ralloc(1, NUMdet, 1, p_y, status)
      call ralloc(1, NUMdet, 1, p_vcor, status)
      call dalloc(1, NUMdet, 1, p_ra, status)
      call dalloc(1, NUMdet, 1, p_dec, status)
      call ralloc(1, NUMdet, 1, p_erad, status)
      call ralloc(1, NUMdet, 1, p_hbox, status)
      call ralloc(1, NUMdet, 1, p_prob, status)
      call ralloc(1, NUMdet, 1, p_snr, status)
      if ( status.ne.0 ) then
         call xwrite(' Failed to allocate temporary arrays to write'//
     &               ' detect output', 5)
         return
      endif

      call wrdetect(Mapid, Equinox, Filedet, Fitsdet, NUMdet, 
     &              memr(p_cnt), memr(p_err), memr(p_x), memr(p_y), 
     &              memr(p_vcor), memd(p_ra), memd(p_dec), memr(p_erad),
     &              memr(p_hbox), memr(p_prob), memr(p_snr), status)
c
c  Free allocated buffers
c
      status = 0
      call ralloc(0, NUMdet, 1, p_cnt, status)
      call ralloc(0, NUMdet, 1, p_err, status)
      call ralloc(0, NUMdet, 1, p_x, status)
      call ralloc(0, NUMdet, 1, p_y, status)
      call ralloc(0, NUMdet, 1, p_vcor, status)
      call dalloc(0, NUMdet, 1, p_ra, status)
      call dalloc(0, NUMdet, 1, p_dec, status)
      call ralloc(0, NUMdet, 1, p_erad, status)
      call ralloc(0, NUMdet, 1, p_hbox, status)
      call ralloc(0, NUMdet, 1, p_prob, status)
      call ralloc(0, NUMdet, 1, p_snr, status)

      END
