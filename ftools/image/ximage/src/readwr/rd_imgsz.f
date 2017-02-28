      subroutine rd_imgsz(Lun, Filetype, Imgnum, Xi, Yi, Naxes, 
     &                    Cenpix, Status)
      implicit none
c
c  Reads image size from FITS file
c    from NAXES for image file
c    from TLMIN/MAX for event file
c
c  I  Lun      (i)  Logical unit of open image FITS file
c  I  Filetype (s)  Type of file ('IMG' or 'EVT')
c I/O Imgnum   (i)  Image number
c I/O Xi       (i)  X Column index
c I/O Yi       (i)  Y Column index
c  O  Naxes    (i)  Size of axes
c  O  Cenpix   (d)  Physical center of detector
c I/O Status   (i)  Error flag (0 = OK)
c
      character*(*) Filetype
      integer*4 Lun, Imgnum, Xi, Yi
      integer*4 Naxes(3), Status
      real*8 Cenpix(2)

      include '../include/io.inc'
c
c  Local variables
c
      integer*4 bitpix, naxis, pcount, gcount
      logical simple, extend
      character(80) comment
      integer*4 i, ixmin, ixmax, iymin, iymax

      Status = 0

      if ( Filetype.eq.'IMG' ) then
         bitpix = -999
         CALL FTGHPR(Lun,3,simple,bitpix,naxis,Naxes,pcount,gcount,
     &               extend,Status)
         IF ( Status.NE.0 ) THEN
            CALL XWRITE(' Error: in getting primary header',10)
            RETURN
         ENDIF
         IF ( naxis.LT.2 ) THEN
            CALL XWRITE(' Error: Not a 2d image',10)
            Status = -1
            RETURN
         ENDIF
         IF ( naxis.GT.3 ) THEN
            CALL XWRITE(' Only the first image in the file will be read'
     &                   , 10)
            naxis = 2
         ENDIF
   
         IF ( naxis.EQ.3 ) THEN
            IF ( Imgnum.GT.Naxes(3) ) THEN
               CALL XWRITE(' Image number not in file',10)
               write (ZWRite, *) ' Reading last image: ', Naxes(3)
               CALL XWRITE(ZWRite,10)
               Imgnum = Naxes(3)
            ENDIF
         ENDIF
         if ( Naxes(1).le.0 .or. Naxes(2).le.0 ) then
            call xaerror(' NAXIS values not positive', 5)
            Status = -1
         endif

         Xi = 1
         Yi = 2
         Cenpix(1) = dfloat(int(Naxes(1)/2.0)) + 0.5
         Cenpix(2) = dfloat(int(Naxes(2)/2.0)) + 0.5
      
      elseif ( Filetype.eq.'EVT' ) then

         CALL FTGKNJ(Lun,'TLMIN',Xi,1,ixmin,i,status)
         if ( i.le.0 ) status = -1
         CALL FTGKNJ(Lun,'TLMAX',Xi,1,ixmax,i,status)
         if ( i.le.0 ) status = -1
         Naxes(1) = ixmax - ixmin + 1
         CALL FTGKNJ(Lun,'TLMIN',Yi,1,iymin,i,status)
         if ( i.le.0 ) status = -1
         CALL FTGKNJ(Lun,'TLMAX',Xi,1,iymax,i,status)
         if ( i.le.0 ) status = -1
         Naxes(2) = iymax - iymin + 1

         if ( status.eq.0 ) then
            call XWRITE(' Using TLMIN/MAX keywords', 20)
            Cenpix(1) = dfloat(ixmin + nint(Naxes(1)/2.0)) - 0.5
            Cenpix(2) = dfloat(iymin + nint(Naxes(2)/2.0)) - 0.5
         else
            status = 0
            CALL FTGKNJ(Lun,'TALEN',Xi,1,Naxes(1),i,status)
            if ( i.le.0 ) status = -1
            CALL FTGKNJ(Lun,'TALEN',Yi,1,Naxes(2),i,status)
            if ( i.le.0 ) status = -1

            if ( status.eq.0 ) then
               call XWRITE(' Using TALEN keywords', 20)
            else
               status = 0
               CALL FTGKYJ(lun,'axlen1',Naxes(1),comment,status)
               CALL FTGKYJ(lun,'axlen2',Naxes(2),comment,status)
               if ( status.eq.0 ) then
                  call XWRITE(' Using AXLEN keywords', 20)
               else
                  status = 0
                  CALL FTGKYJ(lun,'x-range',Naxes(1),comment,status)
                  CALL FTGKYJ(lun,'y-range',Naxes(2),comment,status)

                  if ( status.eq.0 ) then
                     call XWRITE(' Using X/Y-RANGE keywords', 20)
                  else
                     call XWRITE(' Failed to find any axis length info', 
     &                                                               10)
                     Naxes(1) = 0
                     Naxes(2) = 0
                     Status = 0
                  endif
   
               endif
   
            endif
            Cenpix(1) = dfloat(int(Naxes(1)/2.0)) + 0.5
            Cenpix(2) = dfloat(int(Naxes(2)/2.0)) + 0.5
         endif

      endif
c
c  If no axis length info, cenpix value is meaningless
c
      if ( Naxes(1).eq.0 .or. Naxes(2).eq.0 ) then
         Cenpix(1) = 0.
         Cenpix(2) = 0.
      endif

      write(ZWRite,'(a,i8)') ' Image X Axis: ', Naxes(1)
      call XWRITE(ZWRite, 20)
      write(ZWRite,'(a,i8)') ' Image Y Axis: ', Naxes(2)
      call XWRITE(ZWRite, 20)

      return
      end
