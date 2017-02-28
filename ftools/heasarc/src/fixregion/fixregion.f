
*+FIXREGION

      subroutine fixren

c ---------------------------------------------------------------
c this subroutine does write the extracted region file with the
c key parameters, e.g., CDELT,CRPIX,CRVAL
c input  : input region file,input image file
c output : output region file with key parameters in it
c N.B. the input image file is closed after reading the keywords
c ---------------------------------------------------------------
      implicit none
      character(180) regfile,imgfile,convfile,out_regfile
      integer chatter,errflg

c ------------ author/modifications ----------------------------
c Banashree Mitra Seifert (1996, April) 1.0.0
c
c Banashree Mitra Seifert (1996, July) 1.1.0:
c         . convfile to be input with extension number so that the task
c           can handle any type of file.
c
c  Banashree M Seifert (1997, Jan) 1.2.0:
c         . corrected the fmtstr variable which was set incorrectly by MJT 
c  Peter D. Wilson (1998, June 10) 1.2.1:
c         . bug fixes in get_sys2_coords: ext handling & a bad ftgky* call
c  Peter D. Wilson (1998, June 10) 1.2.2:
c         . Updated get_region_par for new fcpars/ftopen behavior...
c ----------------------------------------------------------------
      character(11) taskname
      parameter (taskname='fixregion')
      character(5) version
      parameter (version='1.2.2')
c --------------------- internal variables ------------------------

      character(100) subinfo
      integer status,iunitr,ounitr,maxpoints
      parameter (maxpoints=100)
      character(200) comm_line(10)
      character(10) shape(maxpoints)
      character(1) sign(maxpoints)
      real points(maxpoints,maxpoints)
      integer line_no,shape_no,npoints(maxpoints),char_no(maxpoints)
      real img_deltx,img_delty,img_pixx,img_pixy,img_valx,img_valy
      real img_crot
      real tcdltx,tcdlty,tcrpxx,tcrpxy,tcrvlx,tcrvly,tcrot
      character(10) img_ctyp,tctyp
      logical killit,imgfile_pres

c ----------------- variable definitions -----------------
c regfile     char   input name of input image file
c imgfile     char   input the IMAGE extension file
c out_regfile char   input output fixed region file
c iunitr      int    unit no. for input region file(regfile)
c ounitr      int    unit no. for output region file(out_regfile)
c img_*       real   values of respective parameters from image file
c -------------------------------------------------------------------

      subinfo='using '//taskname//' Ver :'//version
      call wtinfo(chatter,10,2,subinfo)

c ----------------- get parameter file ---------------------------

      call get_region_par(regfile,imgfile,convfile,out_regfile,
     >                          img_deltx,img_delty,img_pixx,img_pixy,
     >                          img_valx,img_valy,imgfile_pres,
     >                          chatter,killit,errflg)

      if (errflg .ne. 0) then
          subinfo = 'getting parameter from get_region_par'
          call wterrm(taskname,version,subinfo)
          return
      endif

c -----------------------------------------------------------------
c if image file is present then read relevant keywords from the file
c otherwise (if image file is not given) the relevant parameters were
c read from user input in get_region_par subroutine and passed here
c -----------------------------------------------------------------

      if(imgfile_pres) then
         call get_sys1_coords(imgfile,img_deltx,img_delty,img_pixx,
     >                   img_pixy,img_valx,img_valy,img_crot,
     >                   img_ctyp,chatter,errflg)
         if(errflg .ne. 0) then
            subinfo='getting coordinates from image file'
            call wterrm(taskname,version,subinfo)
            errflg=1
            goto 200
         endif
      endif

c -------------------------------------------------------------------
c get required keywords for conversion from the second file to which
c it needs to convert
c -------------------------------------------------------------------
      call get_sys2_coords(convfile,tcdltx,tcdlty,tcrpxx,tcrpxy,
     >                     tcrvlx,tcrvly,tcrot,tctyp,
     >                     chatter,errflg)

      if(errflg .ne. 0) then
         subinfo='getting coordinates from second file'
         call wterrm(taskname,version,subinfo)
         errflg=1
         goto 200
      endif

c --------------------------------------------------------------
c conversion parameters is obtained,
c Now read the region file
c --------------------------------------------------------------
c get lun for input region and output region files and open them
c --------------------------------------------------------------

      status = 0
      call ftgiou(iunitr,status)
      call ftgiou(ounitr,status)

      open(unit=iunitr,file=regfile,status='old')
      open(unit=ounitr,file=out_regfile,status='unknown')

c ------------------------------------------------------------------
c now read the region file each line and save them as shape_no,
c shape, no. of points for each shape, sign infront of each shape, etc
c --------------------------------------------------------------------

      call rdreg1(iunitr,char_no,maxpoints,line_no,comm_line,
     >                 shape_no,shape,npoints,points,sign,
     >                 chatter,errflg)

      if(errflg .ne. 0) then
         subinfo='returning from rdreg1'
         call wterrm(taskname,version,subinfo)
          errflg=1
         goto 200
      endif

      close(iunitr)
      call ftfiou(iunitr,status)

c copy  comment lines from the region_file to the output file

c      do i=1,line_no
c         write(ounitr,'(a70)')comm_line(i)
c      enddo

c write the keyword values as COMMENTS to the output file

c          write(ounitr,601) '## 1st Region: '
c          write(ounitr,602) '## CRPIX1 = ',img_pixx
c          write(ounitr,602) '## CRPIX2 = ',img_pixy
c          write(ounitr,602) '## CDELT1 = ',img_deltx
c          write(ounitr,602) '## CDELT2 = ',img_delty
c          write(ounitr,602) '## CRVAL1 = ',img_valx
c          write(ounitr,602) '## CRVAL2 = ',img_valy
c          write(ounitr,602) '## CROAT2 = ',img_crot
c          write(ounitr,603) '## CTYPE1 = ',img_ctyp
c          write(ounitr,601) '##             '

c 601      format(a13)
c 602      format(a12,e16.10)
c 603      format(a12,a4)

c      do i=1,shape_no
c          m1=char_no(i)
c          m2= npoints(i)-1
c          write(ounitr,702)  sign(i),shape(i),'(',points(i,1),
c     >                    (',',points(i,j),j=2,npoints(i)),')'

c 702      format(a1,a<m1>,a1,f8.2,<m2>(a1,f8.2),a1)
c      enddo


c ------------------------------------------------------------------
c Everything we need to know for conversion is known
c Now we go for conversion and write outfile
c ------------------------------------------------------------------

      call reg_converter(ounitr,shape_no,shape,npoints,points,char_no,
     >                   sign,img_deltx,img_delty,img_pixx,img_pixy,
     >                   img_valx,img_valy,img_crot,img_ctyp,
     >                   tcdltx,tcdlty,tcrpxx,tcrpxy,tcrvlx,tcrvly,
     >                   tcrot,tctyp,chatter,errflg)

      if(errflg .ne. 0) then
         subinfo='returning from reg_converter'
         call wterrm(taskname,version,subinfo)
      endif


 200  call wtendm(taskname,version,errflg,chatter)
      end

c --------------------------------------------------------------------
c                 end of main routine
c --------------------------------------------------------------------

*+GET_REGION_PAR

      subroutine get_region_par(regfile,imgfile,convfile,out_regfile,
     >                          img_deltx,img_delty,img_pixx,img_pixy,
     >                          img_valx,img_valy,imgfile_pres,
     >                          chatter,killit,errflg)

c --------------------------------------------------------------------
c this subroutine gets the parameter file for region converter
c --------------------------------------------------------------------

      character*(*) regfile,imgfile,convfile,out_regfile
      real img_deltx,img_delty,img_pixx,img_pixy,img_valx,img_valy
      real img_crot
      character(10) img_ctyp
      integer chatter,errflg
      logical killit,imgfile_pres

c ----------- authors/modifications ---------------------------------
c Banashree Mitra Seifert (1996, April 1.0.0): first release
c Banashree Mitra Seifert (1997, May   1.1.0):
c           . some tidied up
c Peter D. Wilson         (1998, June  1.1.1):
c         . Updated for new fcpars/ftopen behavior...
c           Cannot use INQUIRE on a FITS filename because of new
c           extended syntax
c -------------------------------------------------------------------
      character(5)  version
      parameter (version = '1.1.1')
      character(15) subname
*-
c --------------- internal declarations --------------------------------
      character(100) fileinfo
      character(180) filename
      character(120) subinfo
      integer  status, extnum
      logical ext
      integer option


      subname ='get_image_par'
      subinfo  = 'using '//subname//' Ver :'//version
      call wtinfo(chatter,10,2,subinfo)

c --------------------------------------------------------------------
c      subinfo='This is converter of region file which translates the '
c     >        //'region file '
c      call wtinfo(1,0,1,subinfo)
c      subinfo='FROM the imagefile it was extracted(system1)'
c      call wtinfo(1,0,1,subinfo)
c      subinfo='TO another system (system2)'
c      call wtinfo(1,0,1,subinfo)
c ----------------- chatter parameter ------------------------------

      status = 0
      call uclgsi('chatter',chatter,status)
      if (status .ne. 0) then
          subinfo = 'getting chatter parameter'
          call wterrm(subname,version,fileinfo)
          chatter = 9
          status = 0
      endif

c ----------------- read in clobber -----------------------

      status = 0
      call uclgsb('clobber', killit, status)
      if (status .ne. 0) then
          subinfo = 'getting clobber parameter'
          call wterrm(subname,version,fileinfo)
          killit =.false.
          status = 0
      endif

c ----------------- get input region file --------------------------
      status = 0
      call uclgst('regfile', regfile, status)
      if (status .ne. 0) then
          subinfo = 'getting regfile parameters !'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

      call crmvlbk(regfile)
      if (regfile(:1) .eq. ' ') then
          subinfo = 'region file has to be entered!'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

      call fcpars(regfile, filename, extnum, status)
      call crmvlbk(filename)
      ext = .true.
      INQUIRE(FILE=filename, EXIST=ext)
      if (.NOT. ext) then
          subinfo= 'region file does not exist!'
          call wterrm(subname,version,subinfo)
          subinfo = 'filename : '//filename
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

c ----------------- read the option to supply ---------------------
c      either image file or the coordinates for image file
c option=1 --> image file
c option=2 --> image coordinates
c ----------------------------------------------------------------

      status = 0
      call uclgsi('option',option,status)
      if (status .ne. 0) then
          subinfo = 'getting option parameter'
          call wterrm(subname,version,fileinfo)
          errflg = 1
          return
      endif

c ----------------- get input image file (option=1) ---------------------
      status = 0
      if (option .eq. 1) then
          imgfile_pres=.true.
          call uclgst('imgfile', imgfile, status)
          if (status .ne. 0) then
              subinfo = 'getting imgfile parameters !'
              call wterrm(subname,version,subinfo)
              errflg = 1
              return
          endif

          call crmvlbk(imgfile)
          if (imgfile(:1) .eq. ' ') then
              subinfo = 'image file has to be entered!'
              call wterrm(subname,version,subinfo)
              errflg = 1
              return
          endif

C PDW: remove file checking due to new fcpars paradigm
C          call fcpars(imgfile, filename, extnum, status)
C          call crmvlbk(filename)
C          ext = .true.
C          INQUIRE(FILE=filename, EXIST=ext)
C          if (.NOT. ext) then
C              subinfo= 'image file does not exist!'
C              call wterrm(subname,version,subinfo)
C              subinfo = 'filename : '//filename
C              call wterrm(subname,version,subinfo)
C              errflg = 1
C              return
C          endif

c -----------------------------------------------------------------
c if image file is not present then supply the relevant parameters
c ------------------------------------------------------------------

      else
          imgfile_pres=.false.
          subinfo='If no image file then reply to followings:'
          call wtinfo(chatter,0,0,subinfo)
          call uclgsr('cdelt1',img_deltx,status)
          if(status .ne. 0) then
             subinfo='getting parameter CDELTX from input'
             errflg=1
             return
          endif
          call uclgsr('cdelt2',img_delty,status)
          if(status .ne. 0) then
             subinfo='getting parameter CDELTY from input'
             errflg=1
             return
          endif
          call uclgsr('crpix1',img_pixx,status)
          if(status .ne. 0) then
             subinfo='getting parameter CRPIX1 from input'
             errflg=1
             return
          endif
          call uclgsr('crpix2',img_pixy,status)
          if(status .ne. 0) then
             subinfo='getting parameter CRPIX2 from input'
             errflg=1
             return
          endif
          call uclgsr('crval1',img_valx,status)
          if(status .ne. 0) then
             subinfo='getting parameter CRVAL1 from input'
             errflg=1
             return
          endif
          call uclgsr('crval2',img_valy,status)
          if(status .ne. 0) then
             subinfo='getting parameter CRVAL2 from input'
             errflg=1
             return
          endif
          call uclgsr('crota2',img_crot,status)
          if(status .ne. 0) then
             subinfo='getting parameter CROTA2 from input'
             errflg=1
             return
          endif
          call uclgst('ctype1',img_ctyp,status)
          if(status .ne. 0) then
             subinfo='getting parameter CTYPE1 from input'
             errflg=1
             return
          endif

      endif
c -----------------------------------------------------------------
c read the filename to get the keywords of the second system to
c which the region file is to be converted
c -----------------------------------------------------------------

      call uclgst('convfile',convfile, status)
      if (status .ne. 0) then
          subinfo = 'getting convfile parameter'
          call wterrm(subname,version,fileinfo)
      endif
      call crmvlbk(convfile)
      if (convfile(:1) .eq. ' ') then
          subinfo = 'event filename for conversion has to be entered!'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

c ---- terminate with an error mesg if user does not give convfil -------

C PDW: remove file checking due to new fcpars/ftopen paradigm
C      call fcpars(convfile, filename, extnum, status)
C      call crmvlbk(filename)
C      ext = .true.
C      INQUIRE(FILE=filename, EXIST=ext)
C      if (.NOT. ext) then
C           subinfo= 'conversion file does not exist!'
C           call wterrm(subname,version,subinfo)
C           subinfo = 'filename : '//filename
C           call wterrm(subname,version,subinfo)
C           errflg = 1
C           return
C      endif
 
c ---------------- outfile name ----------------------------------------

      call uclgst('out_regfile', out_regfile, status)
      if (status .ne. 0) then
          subinfo = 'getting outfile parameter'
          call wterrm(subname,version,fileinfo)
      endif
      call crmvlbk(out_regfile)

c ------------------ check validity of outfil --------------------------

      if(out_regfile(1:2)  .eq.  '  ') then
         subinfo= 'outfile name needs to be entered'
         call wterrm(subname,version,subinfo)
         errflg = 1
         return
      endif

      errflg = 0
      return
      end
c ------------------------------------------------------------------
c                      end of get_region_par
c -------------------------------------------------------------------

*+GET_SYS1_COORDS


      subroutine get_sys1_coords(imgfile,img_deltx,img_delty,img_pixx,
     >                    img_pixy,img_valx,img_valy,img_crot,
     >                    img_ctyp,chatter,errflg)

c ------------------ open input image file ------------------------
c this opens input image file and reads the required keyword
c CDELT,CRPIX,CRVAL
c -----------------------------------------------------------------

      character(180) imgfile
      character(100) subinfo,comm
      character(20) instr(50),outhdu(9,50),outver(9,50),extname
      character(20) extnames(9,50)
      integer iunit,nsearch, status,next(50),ninstr,chatter,errflg
      real img_deltx,img_delty,img_pixx,img_pixy,img_valx,img_valy
      real img_crot
      character(10) img_ctyp

c --------- author/modifications --------------------------------
c
c Banashree Mitra Seifert (Feb 1996) 1:0:0
c
c ---------------------------------------------------------------
       character(5) version
       parameter (version='1.0.0')
       character(15) subname

       subname = 'get_sys1_coords'
       subinfo  = 'using '//subname//' Ver :'//version
       call wtinfo(chatter,10,2,subinfo)

c -------------------------------------------------------------


         status = 0
         ninstr = 1
         instr(1) = 'IMAGE'
         nsearch = 50

         call mvext (0, imgfile, iunit, ninstr, instr, nsearch, next,
     >             outhdu, extnames, outver, extname, status, chatter)
         if (status .ne. 0) then
             subinfo='opening the input file'
             call wtferr(subname,version,status,subinfo)
             call ftclos(iunit,status)
             subinfo = 'closing input EVT file'
             call wtferr(subname,version,status,subinfo)
             errflg=1

         else
             subinfo='moved to IMAGE  extension'
             call wtinfo(chatter,10,1,subinfo)
         endif

c moved to desired extension
c Now read the reqd keywords

         call ftgkye(iunit,'CDELT1',img_deltx,comm,status)

            if (status .ne. 0) then
                subinfo='getting CDELT1 from IMAGE extension'
                call wterrm(subname,version,subinfo)
                errflg=1
                return
            endif

         call ftgkye(iunit,'CDELT2',img_delty,comm,status)

            if (status .ne. 0) then
                subinfo='getting CDELT2 from IMAGE extension'
                call wterrm(subname,version,subinfo)
                errflg=1
                return
            endif

         call ftgkye(iunit,'CRPIX1',img_pixx,comm,status)

            if (status .ne. 0) then
                subinfo='getting CRPIX1 from IMAGE extension'
                call wterrm(subname,version,subinfo)
                errflg=1
                return
            endif

         call ftgkye(iunit,'CRPIX2',img_pixy,comm,status)

            if (status .ne. 0) then
                subinfo='getting CRPIX2 from IMAGE extension'
                call wterrm(subname,version,subinfo)
                errflg=1
                return
            endif

            call ftgkye(iunit,'CRVAL1',img_valx,comm,status)

            if (status .ne. 0) then
                subinfo='getting CRVAL1 from IMAGE extension'
                call wterrm(subname,version,subinfo)
                errflg=1
                return
            endif

         call ftgkye(iunit,'CRVAL2',img_valy,comm,status)

            if (status .ne. 0) then
                subinfo='getting CRVAL2 from IMAGE extension'
                call wterrm(subname,version,subinfo)
                errflg=1
                return
            endif

         call ftgkye(iunit,'CROTA2',img_crot,comm,status)

            if (status .ne. 0) then
                subinfo='getting CROTA2 from IMAGE extension'
                call wterrm(subname,version,subinfo)
                errflg=1
                return
            endif

         call ftgkys(iunit,'CTYPE1',img_ctyp,comm,status)

            if (status .ne. 0) then
                subinfo='getting CTYPE1 from IMAGE extension'
                call wterrm(subname,version,subinfo)
                errflg=1
                return
            endif
         img_ctyp = img_ctyp(5:9)


      call ftfiou(iunit,status)
      call ftclos(iunit,status)

      return
      end
c -------------------------------------------------------------------
c                     end of get_coords
c -------------------------------------------------------------------

*+REG_CONVERTER

      subroutine reg_converter(ounit,shape_no,shape,npoints,points,
     >                         char_no,sign,
     >                         img_deltx,img_delty,img_pixx,
     >                         img_pixy,img_valx,img_valy,img_crot,
     >                         img_ctyp,tcdltx,tcdlty,tcrpxx,tcrpxy,
     >                         tcrvlx,tcrvly,tcrot,tctyp,chatter,errflg)

c --------------------------------------------------------------------
c this routine converts region file from one system to another
c from system1 parameters ---> img_*
c to   system2 parameters --> tc*
c --------------------------------------------------------------------

      implicit none
      character*(*) img_ctyp,tctyp
      real img_deltx,img_delty,img_pixx,img_pixy,img_valx,img_valy
      real img_crot,tcrot,scale_factor
      real tcdltx,tcdlty,tcrpxx,tcrpxy,tcrvlx,tcrvly
      real xpix,ypix
      integer ounit,chatter,errflg
      character*(*) shape(*)
      character*(*) sign(*)
      character(8) cm1,cm2
      character(80) fmtstr
      real points(100,100)
      real new_pts(100,100)
      double precision xsky,ysky,xpix_out,ypix_out
      integer i,j,jj,status,shape_no,m1,m2
      integer npoints(*),char_no(*)

      character(100) subinfo

c ------------- author/modifications -----------------------------
c Banashree Mitra Seifert (1996 April) 1.0.0:
c
c  Banashree M Seifert (1997, Jan) 1.1.0:
c         . corrected the fmtstr variable which was set incorrectly by MJT 
c -----------------------------------------------------------------
c check if the shape is a polygon
c polygon is a special case since xpix,ypix needs to be evaluated
c for each corner of the polygon.
c all other shapes has only one value to be converted, ie, center
c and other parameters are radius, length of sides etc.
c call to ftwldp converts xpix and ypix to sky coordinates and
c call to ftxypx converts xsky and ysky to detector coordinate
c ---------------------------------------------------------------------+

      character(5) version
      parameter (version='1.1.0')
      character(14) subname
      parameter (subname='reg_converter')

*-
      subinfo='using '//subname//' Ver :'//version
      call wtinfo(chatter,10,2,subinfo)

c ------------------------------------------------------------------------
      scale_factor = (abs(img_deltx) + abs(img_delty)) /
     >                (abs(tcdltx) + abs(tcdlty))

c write out the coordinates of system2

          write(ounit,601) '## 2nd Region parameters: '
          write(ounit,602) '## CRPIX1 = ',tcrpxx
          write(ounit,602) '## CRPIX2 = ',tcrpxy
          write(ounit,602) '## CDELT1 = ',tcdltx
          write(ounit,602) '## CDELT2 = ',tcdlty
          write(ounit,602) '## CRVAL1 = ',tcrvlx
          write(ounit,602) '## CRVAL2 = ',tcrvly
          write(ounit,602) '## CROAT2 = ',tcrot
          write(ounit,603) '## CTYPE1 = ',tctyp
          write(ounit,601) '##             '

 601      format(a13)
 602      format(a12,e16.10)
 603      format(a12,a4)

c coordinates writing is done
c ---------------------------------------------------------------------+

      do i=1,shape_no
         if(shape(i) .eq. 'POLYGON') then
            do j=1,npoints(i),2
               xpix=points(i,j)
               ypix=points(i,j+1)
               call ftwldp(dble(xpix),dble(ypix),dble(img_valx),
     >                     dble(img_valy),dble(img_pixx),dble(img_pixy),
     >                     dble(img_deltx),dble(img_delty),
     >                     dble(img_crot),img_ctyp, xsky,ysky, status)

               call ftxypx(dble(xsky),dble(ysky),dble(tcrvlx),
     >                     dble(tcrvly),dble(tcrpxx),dble(tcrpxy),
     >                     dble(tcdltx),dble(tcdlty),dble(tcrot),
     >                     tctyp,xpix_out,ypix_out,status)

               new_pts(i,j)   = sngl(xpix_out)
               new_pts(i,j+1) = sngl(ypix_out)
            enddo

            m1=char_no(i)
            m2= npoints(i)
c MJT 09July 1996 making linux happy...
c           write(ounit,901)  sign(i),shape(i),'(',new_pts(i,1),
c    >                      (',',new_pts(i,jj),jj=2,npoints(i)),')'
c901        format(a1,a<m1>,a1,f8.2,<m2>(a1,f8.2),a1)
            call ftkeyn('a',m1,cm1,status)
            call ftkeyn('a',m2,cm2,status)
            fmtstr='(a1,'//cm1//',a1,f8.2,'//cm2(2:)//'(a1,f8.2),a1)'
            write(ounit,fmtstr)  sign(i),shape(i),'(',new_pts(i,1),
     >           (',',new_pts(i,jj),jj=2,npoints(i)),')'
         else

c If the shape is not a polygon then calculate a single pair of
c xpix, ypix values for the center of the region.

            xpix=points(i,1)
            ypix=points(i,2)
            call ftwldp(dble(xpix),dble(ypix),dble(img_valx),
     >                  dble(img_valy),dble(img_pixx),dble(img_pixy),
     >                  dble(img_deltx),dble(img_delty),dble(img_crot),
     >                  img_ctyp,xsky,ysky, status)

            call ftxypx(dble(xsky),dble(ysky),dble(tcrvlx),dble(tcrvly),
     >                  dble(tcrpxx),dble(tcrpxy),dble(tcdltx),
     >                  dble(tcdlty),dble(tcrot),tctyp,xpix_out,
     >                  ypix_out,status)


c For CIRCLE,BOX, ELLIPSE and ANNULUS, we have to scale the dimensions
c i.e., radius, length of sides etc.

            if (shape(i) .eq. 'CIRCLE') then
C           Scale the radius
                points (i,3) = points (i,3) * scale_factor

            elseif (shape(i) .eq. 'BOX') then
C           Scale the sides
                points (i,3) = points (i,3) * scale_factor
                points (i,4) = points (i,4) * scale_factor

            elseif (shape(i) .eq. 'ELLIPSE') then
C           Scale the major, minor axes
                points (i,3) = points (i,3) * scale_factor
                points (i,4) = points (i,4) * scale_factor

            elseif (shape(i) .eq. 'ANNULUS') then
C           Scale the radii
                points (i,3) = points (i,3) * scale_factor
                points (i,4) = points (i,4) * scale_factor

            endif

            xpix = sngl(xpix_out)
            ypix=sngl(ypix_out)

            m1=char_no(i)
            m2= npoints(i)-2
c MJT 05July 1996 making linux happy...
            call ftkeyn('a',m1,cm1,status)
            call ftkeyn('a',m2,cm2,status)
            fmtstr='(a1,'//cm1//',a1,f10.2,a,f10.2,'
     $           //cm2(2:)//'(a,f10.2),a)'
            write(ounit,fmtstr)  sign(i),shape(i),'(',xpix,',',ypix,
     >           (',',points(i,j),j=3,npoints(i)),')'
c           write(ounit,902)  sign(i),shape(i),'(',xpix,',',ypix,
c    >                    (',',points(i,j),j=3,npoints(i)),')'
c902        format(a1,a<m1>,a1,f8.2,a,f8.2,<m2>(a,f8.2),a)
         endif
      enddo

      call ftfiou(ounit,status)
      return
      end

c ---------------------------------------------------------------------
c                   end of reg_converter
c ---------------------------------------------------------------------

*+GET_SYS2_COORDS

      subroutine get_sys2_coords(convfile,tcdltx,tcdlty,tcrpxx,
     >                           tcrpxy,tcrvlx,tcrvly,tcrot,tctyp,
     >                           chatter,errflg)

c ------------------------------------------------------------------
c this routine reads the reduired keyword needed for conversion
c routine and pass them on
c ------------------------------------------------------------------

      implicit none
      character*(*) convfile
      real tcdltx,tcdlty,tcrpxx,tcrpxy,tcrvlx,tcrvly,tcrot
      character(10) tctyp
      integer errflg,chatter

c ---------------- internal variables ------------------------------
      integer iunit,nsearch, status,next(50),ninstr
      character(100) subinfo,comm
      character(20) instr(50),outhdu(9,50),outver(9,50),extname
      character(20) extnames(9,50)

c ------------ author/modifications ---------------------------------
c Banashree Mitra Seifert (1996, April) 1.0.0:
c Banashree Mitra Seifert (1997, May)   1.1.0:
c                . replaced mver by mvext (as it was not going to
c                  extension
c       Peter D. Wilson (1998, June 10) 1.1.1:
c                . Fixed call to mvext to use full filename[ext]
c                . Comment out ftopen, etc... mvext opens file
c                . Fix call for reading CROTA1 from ftgkys to ftgkye
c ------------------------------------------------------------------
      character(16) subname
      parameter (subname='get_sys2_coords')
      character(5) version
      parameter (version='1.1.1')

c ---------------------------------------------------------------------
*_
      subinfo='using '//subname//' Ver :'//version
      call wtinfo(chatter,10,2,subinfo)

c ----------- opening evt file -----------------------------------------

      status = 0

C PDW: These calls are unnecessary... mvext parses and opens file
C      call fcpars(convfile,filename,extnum,status)
C      status=0
C      call ftgiou(iunit,status)
C      call ftopen(iunit,filename,1,block,status)

       extname='EVENTS'
       status = 0
       ninstr = 1
       instr(1) = 'EVENTS'
       nsearch = 50

C PDW: send convfile instead of filename to preserve file[ext] format
       call mvext (0, convfile, iunit, ninstr, instr, nsearch, next,
     >             outhdu, extnames, outver, extname, status, chatter)

      if (status .ne. 0) then
          subinfo='opening the input file:'//convfile
          call wtferr(subname,version,status,subinfo)
          call ftclos(iunit,status)
          subinfo = 'closing input file:'//convfile
          call wtferr(subname,version,status,subinfo)
          errflg=1
          return
      endif


       call ftgkye(iunit,'TCDLT1',tcdltx,comm,status)
       subinfo='reading pixel size TCDLT1'
       call wtfwrn(subname,version,chatter,20,status,subinfo)
       if (status .ne. 0) then
          subinfo='looking for CDELT1'
          call wtinfo(chatter,20,2,subinfo)
          status=0
          call ftgkye(iunit,'CDELT1',tcdltx,comm,status)
          if (status .ne. 0) then
              subinfo='reading pixel size CDELT1'
              call wterrm(subname,version,subinfo)
              errflg = 4
              return
          endif
          subinfo='Got it!'
          call wtinfo(chatter,20,2,subinfo)
       endif

       call ftgkye(iunit,'TCDLT2',tcdlty,comm,status)
       subinfo='reading pixel size TCDLT2'
       call wtfwrn(subname,version,chatter,20,status,subinfo)
       if (status .ne. 0) then
          subinfo='looking for CDELT1'
          call wtinfo(chatter,20,2,subinfo)
          status=0
          call ftgkye(iunit,'CDELT2',tcdlty,comm,status)
          if (status .ne. 0) then
              subinfo='reading pixel size CDELT2'
              call wterrm(subname,version,subinfo)
              errflg = 4
              return
          endif
          subinfo='Got it!'
          call wtinfo(chatter,20,2,subinfo)
       endif

       call ftgkye(iunit,'TCRVL1',tcrvlx,comm,status)
       subinfo='reading pixel size TCRVL1'
       call wtfwrn(subname,version,chatter,20,status,subinfo)
       if (status .ne. 0) then
          subinfo='looking for CRVAL1'
          call wtinfo(chatter,20,2,subinfo)
          status=0
          call ftgkye(iunit,'CRVAL1',tcrvlx,comm,status)
          if (status .ne. 0) then
              subinfo='reading pixel size CRVAL1'
              call wterrm(subname,version,subinfo)
              errflg = 4
              return
          endif
          subinfo='Got it!'
          call wtinfo(chatter,20,2,subinfo)
       endif

       call ftgkye(iunit,'TCRVL2',tcrvly,comm,status)
       subinfo='reading TCRVL2 keyword'
       call wtfwrn(subname,version,chatter,20,status,subinfo)
       if (status .ne. 0) then
          subinfo='looking for CRVAL2'
          call wtinfo(chatter,20,2,subinfo)
          status=0
          call ftgkye(iunit,'CRVAL2',tcrvly,comm,status)
          if (status .ne. 0) then
              subinfo='reading CRVAL2 keyword'
              call wterrm(subname,version,subinfo)
              errflg = 4
              return
          endif
          subinfo='Got it!'
          call wtinfo(chatter,20,2,subinfo)
       endif

       call ftgkye(iunit,'TCRPX1',tcrpxx,comm,status)
       subinfo='reading TCRPX1 keyword'
       call wtfwrn(subname,version,chatter,20,status,subinfo)
       if (status .ne. 0) then
          subinfo='looking for CRPIX1'
          call wtinfo(chatter,20,2,subinfo)
          status=0
          call ftgkye(iunit,'CRPIX1',tcrpxx,comm,status)
          if (status .ne. 0) then
              subinfo='reading CRPIX1 keyword'
              call wterrm(subname,version,subinfo)
              errflg = 4
              return
          endif
          subinfo='Got it!'
          call wtinfo(chatter,20,2,subinfo)
       endif

       call ftgkye(iunit,'TCRPX2',tcrpxy,comm,status)
       subinfo='reading TCRPX2 keyword'
       call wtfwrn(subname,version,chatter,20,status,subinfo)
       if (status .ne. 0) then
          subinfo='looking for CRPIX2'
          call wtinfo(chatter,20,2,subinfo)
          status=0
          call ftgkye(iunit,'CRPIX2',tcrpxy,comm,status)
          if (status .ne. 0) then
              subinfo='reading CRPIX2 keyword'
              call wterrm(subname,version,subinfo)
              errflg = 4
              return
          endif
          subinfo='Got it!'
          call wtinfo(chatter,20,2,subinfo)
       endif

       call ftgkys(iunit,'TCTYP1',tctyp,comm,status)
       subinfo = 'reading type of projection keyword'
       call wtfwrn(subname,version,chatter,20,status,subinfo)
       if (status .ne. 0) then
           subinfo = 'looking for keyword CTYPE1'
           call wtinfo(chatter,20,1,subinfo)
           status=0
           call ftgkys(iunit,'CTYPE1',tctyp,comm,status)
           if (status .ne. 0) then
               subinfo='Neither TCTYP1/CTYPE1 keywords found'
               call wterrm(subname,version,subinfo)
               errflg = 4
               return
           endif
           subinfo='Got it!'
           call wtinfo(chatter,20,1,subinfo)
       endif

       call ftupch(tctyp)
       tctyp = tctyp(5:8)

       call ftgkye(iunit,'TCROT1',tcrot,comm,status)
       if (status .ne. 0) then
           status=0
C PDW was: call ftgkys... Oops!
           call ftgkye(iunit,'CROTA1',tcrot,comm,status)
           if (status .ne. 0) then
               status=0
               call ftgkye(iunit,'TCROT2',tcrot,comm,status)
               if (status .ne. 0) then
                   status=0
                   call ftgkye(iunit,'CROTA2',tcrot,comm,status)
                   if(status .ne. 0) then
                      subinfo='Neither TCROT1/TCROT2/CROTA1/CROTA2'
     >                 //' is found'
                      call wtinfo(chatter,9,1,subinfo)
                      subinfo='So rotation is assumed to be 0'
                      call wtinfo(chatter,9,1,subinfo)
                      tcrot=0.
                   endif
               endif
           endif
        endif

        call ftclos(iunit,status)
        call ftfiou(iunit,status)
        return
        end

c ------------------------------------------------------------------
c                     end of get_sys2_coords
c ------------------------------------------------------------------


