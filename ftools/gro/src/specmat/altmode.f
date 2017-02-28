C     ALTMODE
*
*     Given an EGRET viewing mode number (1-74) and the azimuth of
*     a point of interest, determine the number of the mode in which
*     the corresponding point is in the first octant of azimuth
*     (0-45 degrees).  This is necessary because the calibration
*     files contain data only for the first octant.  For each mode
*     there is another one which is related by rotation and reflection
*     so that it contains data for each octant.
C
*     Calling sequence: call altmode(mode,newmode,phi)
C
*     Arguments:
*
*         mode     integer  input    Actual spacecraft mode.
*         newmode  integer  output   Corresponding mode.
*         phi      real     input    Azimuth angle of source.
C
*     Patrick Nolan
*     Stanford University
*     September 1993
C
C @(#) altmode.f 1.3@(#)

      subroutine altmode(mode,newmode,phi)
      implicit none
      integer mode,newmode
      real phi

      save

* Local constants
      integer modes(74)
      data modes/x'1FF',x'1DF',x'17F',x'1FD',x'1F7',x'1D7',x'15F',
     &     x'17D',x'1F5',x'1C7',x'1CF',
     &     x'19F',x'11F',x'13F',x'17E',x'17C',x'1FC',x'1F9',x'1F1',
     &     x'1F3',x'1E7',x'18F',x'13E',x'1F8',
     &     x'1E3',x'187',x'10F',x'11E',x'13C',x'178',x'1F0',x'1E1',
     &     x'1C3',x'107',x'11C',x'170',
     &     x'1C1',x'183',x'10E',x'138',x'1E0',x'83',x'87',x'7',x'F',
     &     x'E',x'1E',x'1C',x'3C',x'38',x'78',
     &     x'70',x'F0',x'E0',x'E1',x'C1',x'C3',x'3',x'6',x'C',x'18',
     &     x'30',x'60',x'C0',x'81',x'2',x'8',x'20',
     &     x'80',x'1',x'4',x'10',x'40',x'0'/
* Each entry in "modes" should be interpreted as a 9-bit bitmap.
* The bits correspond to the 9 EGRET direction modes, as follows:
*     0 E     1 NE    (0=LSB)
*     2 N     3 NW
*     4 W     5 SW
*     6 S     7 SE
*     8 vertical
* If a bit has the value 1, that means the corresponding direction
* mode is DISabled.  This is the opposite of the telemetry convention.
* This classification scheme is due to CVM.  The symmetry properties
* of this layout can be exploited to make rotations and reflections
* simple.

* Local variables
      integer dirnew,dirmod,ki,kf,m(0:8),i
      real phi2

* Functions called
      logical btest
      integer ibclr

* Calculate the octant in which the source position falls.  0 to 7.
* Even octants can be rotated to octant 0 by rotations of a multiple
* of 90 degrees.  Odd octants can be rotated and the reflected across
* the 45 degree line.  The effect is a permutation: each bit in the
* original mode corresponds to a (possibly) different bit in the new mode.
      ki = phi/45.

* Create the vector m, which describes the permutation of the bits.
      do i = 0,8
         m(i) = i
      end do
      phi2 = phi

* If ki is odd, reflect across the SW-NE diagonal
      if (mod(ki,2).eq.1) then
         do i = 0,7
            m(i) = mod(10-i,8)
         end do
         phi2 = mod(360.+90.-phi,360.)
      end if
      kf = phi2/45.

* Rotate by ki/2 steps of 90 degrees.  Use modular arithmetic.
      do i=0,7
         m(i) = m(i)-2*(kf/2)
         if (m(i).lt.0) m(i) = m(i) + 8
      end do

* Build new bitmap from old by turning off corresponding bits
      dirmod = modes(mode)   ! Bitmap for true viewing mode
      dirnew = 511           ! Initial state of new bitmap.  All on.
      do i = 0,8
         if (.not.btest(dirmod,i)) dirnew = ibclr(dirnew,m(i))
      end do

* Find mode number that corresponds to new bitmap
      newmode = -1
      do i=1,74
         if (dirnew.eq.modes(i)) then
            newmode = i
            go to 9999
         end if
      end do

 9999 continue
      return
      end
