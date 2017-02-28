c*******************************************************************************
c    Filename: rosconst.inc
c Description: standard constants required by *some* rosat tasks
c Author/Date: James Peachey, HEASARC/GSFC/NASA, Hughes STX / September, 1997
c       Notes: this file is based on the defunct files CCONST.CMN and
c              CGENL.CMN. the common blocks in these files were
c              conflicting with each other when building a single
c              executable. the solution is to use parameter constants,
c              not common blocks, and remove conflicting initializations
c              from the code which requires these standard variables
c
c*******************************************************************************
C Comments below were taken verbatim from CCONST.CMN
C
CC  general important constants, variables and arrays
C
C************************ CFORM VERSION 0.2 ****************************
C
CA  author : ZIM               date: 14-DEC-1984 09:00
CU  update : PAUL              date: 4-NOV-1985 13:19
C
C   general description:
CG  general important constants, variables and arrays
C
C   common_var.                description
CP  OUTPT                      internal file for output handling
CP  PI                         constant
CP  RAD                        for conversion of degrees into radians
CP  DEG                        for conversion of radians to degree
CP  VALMAX                     maximum value for VAX FORTRAN
CP  VALMIN                     minimum value for VAX FORTRAN
CP  VALEPS                     precision for REAL*4 operations on VAX
C
C***********************************************************************
C
C
      real*4 deg
      real*4 pi
      real*4 pi2
      real*4 pi3
      real*4 pi3h
      real*4 pi4
      real*4 pi4t
      real*4 pif
      real*4 pih
      real*4 pit
      real*4 rad
      real*4 valeps
      real*4 valmax
      real*4 valmin
      parameter (deg = 57.29578)
      parameter (pi = 3.141592654)
      parameter (pi2 = 6.283185308)
      parameter (pi3 = 9.424777961)
      parameter (pi3h = 4.712388981)
      parameter (pi4 = 12.56637061)
      parameter (pi4t = 4.188790203)
      parameter (pif = 0.785398163)
      parameter (pih = 1.570796327)
      parameter (pit = 1.047197551)
      parameter (rad = 0.0174532925)
      parameter (valeps = 1.E-7)
      parameter (valmax = 1.7E+38)
      parameter (valmin = 1.2E-38)
