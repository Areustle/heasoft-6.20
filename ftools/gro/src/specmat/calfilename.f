*  CALFILENAME
*
*  Produces the full file name of a desired calibration file.
*
*  Inputs:
*   tascin (logical)   TASC in the coincidence mode?
*   iclass (integer)   1 for Class C, 3 for Class A or A+B+C
*   calset (char*2)    Predefined values '00', '05', or '10'
*   ftype  (char*3)    Which type of file 'epd', 'psf', or 'sar'
*
*  Output:
*   calfile (character *(*))  Full filename.
*
*  Uses cvm's routines ctalog and cmpare to read the cal-file catalog.
*
*  Written Dec 1994 for Spectral version 2.7
*  PLN, Stanford.

*  Added support for fan modes (calset=15,20) for Spectral version 2.11
*  PLN, August 1996

      subroutine calfilename(data_dir,evclass,tascin,iclass,calset,
     *     ftype,fileid,calfile)
      implicit none
      integer iclass, evclass
      logical tascin
      character(3) ftype
      character*(*) data_dir, calfile, calset

      integer*2 param(29)
      integer ctype,iret,ift,len_trim
      character*(*) fileid

      save

      data param/1,0,0,0,2,7,74,6,120,1,19*0/


      if (iclass.ne.1.and.iclass.ne.3) go to 666

      if (ftype.eq.'edp') then
         ift = 0
      else if (ftype.eq.'psd') then
         ift = 1
      else if (ftype.eq.'sar') then
         ift = 2
      else
         go to 666
      end if

csb-02/07      call getenv('EVCLASS',evclass)

      if (evclass.eq.2) then
         param(5) = 1
      else if (evclass.eq.1) then
         param(5) = 5-iclass
      else
         go to 666
      end if

      param(10) = 0
      if (tascin) param(10) = 1

      read(calset,'(i2)') ctype
      param(28) = ctype
      if (calset.eq.'05') then
	 param(7) = 75          ! Only mode 75 in calset 5
      else if (calset.eq.'15') then
	 param(7) = 81          ! Mode 81 is sure to be there.
      else if (calset.eq.'20') then
	 param(7) = 87          ! Mode 87 is sure to be there.
      else
	 param(7) = 74          ! Mode 74 is sure to be there.
      end if
      
      call ctalog(data_dir,ift,param,29,fileid,iret)
      

      if (iret.ne.0) go to 667
c     call getenv('CALIB_DIR',caldir)
c     calfile = caldir(1:len_trim(caldir))//'/'//ftype//'fil'//fileid
      calfile = ftype // 'fil' // fileid(1:len_trim(fileid))
      return

 666  write (*,*) 'CALFILENAME: Illegal set of attributes specified',
     >     ' for calibration file.'
      return
      
 667  write (*,*) 'CALFILENAME: ctalog couldn''t find requested',
     >     ' calibration file in catalog.'
      write(*,*) 'calfilename::ftype: ', ftype, ' calset: ', calset, 
     *     ' tascin: ', tascin,
     *     ' iclass: ', iclass, ' ctype: ', ctype
      write (*,*) param

      return
      end
