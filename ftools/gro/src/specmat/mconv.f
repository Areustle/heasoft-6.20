*  @(#) mconv.f 1.4@(#)
******************************************************************
* Convert the 16-bit type/direction mode code to a number in the
* range 1-74 corresponding to the indexing scheme used in the
* calibration files.
*#*#*# There is a possible ambiguity about the order of the
* entries in the table below.  This should be checked.
* Also, this uses only direction modes; types are ignored.

* August 1996: Added new fan modes (76-87) and clarified mode
* 75 (vertical) for Spectral version 2.11
* PLN

      subroutine mconv(hexmode,imode)

      integer hexmode,imode
      integer telmod(74),typs,mode,tmask,mmask,suppl(13)

      save

      data telmod/
     >  Z'7F83', Z'5F83', Z'7783', Z'7D83', Z'7F03', Z'3F83',
     >  Z'6F83', Z'7B83', Z'7E83', Z'3F03', Z'1F83', Z'4F83',
     >  Z'6783', Z'7383', Z'7983', Z'7C83', Z'7E03', Z'1E03',
     >  Z'1F03', Z'0F03', Z'0F83', Z'0783', Z'4783', Z'4383',
     >  Z'6383', Z'6183', Z'7183', Z'7083', Z'7883', Z'7803',
     >  Z'7C03', Z'3C03', Z'3E03', Z'0F80', Z'6380', Z'7880',
     >  Z'3E00', Z'1F00', Z'4780', Z'7180', Z'7C00', Z'1E00',
     >  Z'0F00', Z'0780', Z'4380', Z'6180', Z'7080', Z'7800',
     >  Z'3C00', Z'0E00', Z'0380', Z'6080', Z'3800', Z'0C00',
     >  Z'0600', Z'0700', Z'0300', Z'0180', Z'4180', Z'4080',
     >  Z'6000', Z'7000', Z'3000', Z'1800', Z'1C00', Z'0500',
     >  Z'4100', Z'5000', Z'1400', Z'0400', Z'0100', Z'4000',
     >  Z'1000', Z'0000'/
      data tmask/Z'007C'/,mmask/Z'FF83'/
      data suppl/
     >  Z'0003', Z'0080', Z'0083', Z'0800', Z'0803', Z'0880',
     >  Z'0883', Z'0200', Z'0203', Z'2000', Z'2003', Z'2200',
     >  Z'2203'/

C *#*#*# If typs .ne. tmask, then we need a new data file. 
C  Not implemented yet.
      typs = iand(hexmode, tmask)
      mode = iand(hexmode, mmask)
      imode = 0
      do i = 1,74
	if (mode.eq.telmod(i)) then
	  imode = 75 -i	! Kluge!!  Table above is backwards!
	  go to 100
        end if
      end do
      do i = 75,87
	if (mode.eq.suppl(i-74)) then
	  imode = i
	  go to 100
        end if
      end do
100   return
      end

