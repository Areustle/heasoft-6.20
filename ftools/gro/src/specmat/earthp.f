C *mds* always celestial coords - dropped argument
c earthp
c         *************************************************************
c         earthp subroutine calculates the earth position as a function
c         of time for intervals less than one orbit.  it essentially
c         interpolates between a starting and ending location assuming
c         that the earth moves uniformly along a great circle.
c
c         this routine is used in the egret exposure map generation
c         process to report the earth position in the calculation of the
c         earth shadow effects.
c
c         written:  5/29/91.  d.l.bertsch.
c
c         calling sequence:
c            call earthp(nstep,ntotl,epos,qnew,slat,clat,rlon )
c
c                variable     i/o    description
c                --------     ---    -----------------
c                 nstep   i*2  i     step number.  range 1 to ntotl
c                 ntotl   i*2  i     total number of steps
c                 epos(4) r*4  i     earth starting and ending positions
c                                    in radians.
c                                      (1)  start right ascension
c                                      (2)  start declination
c                                      (3)  end right ascension
c                                      (4)  end declination
c                 qnew    l*4  i     when true, process new earth
c                                    starting and ending coordinates.
c                 slat    r*4  o     sine of the latitude (or decl) at
c                                    nstep
c                 clat    r*4  o     cosine of the latitude (or decl)
c                                    at nstep
c                 rlon    r*4  o     earth longitude (or ra) in radians
c                                    at nstep.
c
c         called by:  shadow
c        **************************************************************
c   @(#) earthp.f 1.2@(#)

      subroutine earthp( nstep, ntotl, epos, qnew, slat, clat,
     &                   rlon )

      implicit none
      integer	nstep, ntotl
      real	slat, clat, rlon, delra, sds, cds, sde, cde, gamma
      real	cgam, sbeta, beta, cbeta, delgam, czeta, szeta

      real epos(4)
      logical qnew

      save

c          initialization
      if( qnew ) then
         qnew = .false.
         delra = epos(3) - epos(1)

         sds = sin( epos(2) )
         cds = cos( epos(2) )
         sde = sin( epos(4) )
         cde = cos( epos(4) )

         cgam = sds*sde + cds*cde*cos(delra)
         gamma = acos( cgam )
         delgam = gamma/ntotl

         czeta = ( sde-sds*cgam )/( cds*sin(gamma) )
         szeta =  cde*sin(delra)/sin(gamma)
      endif
c         end of initialization.

      beta = (nstep-0.5)*delgam
      sbeta = sin(beta)
      cbeta = cos(beta)
      slat = sds*cbeta + cds*sbeta*czeta
      clat = sqrt( 1.0-slat*slat )

      rlon = epos(1) + atan2( sbeta*szeta*cds, cbeta-sds*slat )

      return
      end
