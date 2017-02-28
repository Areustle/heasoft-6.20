* Last processed by NICE on 10-SEP-1991 16:34:28
* Customized for :  VAX         VMS          
*
*----------------------------------------------------------------------
* Projection definitions
*
      REAL*8   A0                  ! X Coordinate of projection point
     $,D0                          ! Y Coordinate of projection point
     $,PANGLE                      ! Angle of projection line
     $,SIND0                       ! Sinus and
     $,COSD0                       ! Cosinus of D0
     $,NPOLE                       ! Position of North pole
     $,SPOLE                       ! Position of South pole
      INTEGER  P_TYPE              ! Projection type
     $,IPOLE                       ! Which pole visible ? 1 none, 2 North, 3 South 4 Both
     $,I_SYSTEM                    ! Coordinate system
      COMMON /MAP001/ A0,D0,PANGLE,
     $SIND0,COSD0,NPOLE,SPOLE,
     $P_TYPE,IPOLE,I_SYSTEM
*
* These values must remain consistent with [UTIL.SAS]CONSTANT.INC
      INTEGER P_NONE,P_GNOMONIC,P_ORTHO,P_AZIMUTHAL,P_STEREO
     $,P_LAMBERT,P_AITOFF,P_RADIO
      PARAMETER  (
     $P_NONE  = 0                  ! Unprojected data
     $,P_GNOMONIC = 1              ! Radial Tangent plane
     $,P_ORTHO = 2                 ! Dixon Tangent plane
     $,P_AZIMUTHAL = 3             ! Schmidt Tangent plane
     $,P_STEREO = 4                ! Stereographic
     $,P_LAMBERT = 5               ! Lambert equal area
     $,P_AITOFF = 6                ! Aitoff equal area
     $,P_RADIO   = 7               ! Classic Single dish radio mapping
     $)
*
*	Remarkable points
*
      REAL*8
     $AXREM1,AXREM2,AXREM3,AXREM4,AXREM5,AXREM6,
     $AYREM1,AYREM2,AYREM3,AYREM4,AYREM5,AYREM6,
     $DECMIN,DECMAX,RAMIN,RAMAX    ! Minima and maxima
      COMMON /MAP002/
     $AXREM1,AXREM2,AXREM3,AXREM4,AXREM5,AXREM6,
     $AYREM1,AYREM2,AYREM3,AYREM4,AYREM5,AYREM6,
     $DECMIN,DECMAX,RAMIN,RAMAX
*
      SAVE /MAP001/, /MAP002/
*----------------------------------------------------------------------
