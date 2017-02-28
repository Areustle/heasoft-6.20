NH (Nov96)                   ftools.heasarc                   NH (Nov96)



NAME
    nh - Return hydrogen column density for a given Ra and Dec
    
    
USAGE
    nh equinox ra dec disio
    
    
DESCRIPTION
    This   program   returns,   for  a  specified  right  ascension  and 
    declination, a value for the galactic hydrogen column  density,  Nh.
    This  value  is  derived  from  the  HI map by Kalberla et al. 2005,
    Astronomy   &    Astrophysics,    440,    775,    known    as    the  
    Leiden/Argentine/Bonn  (LAB)  survey.   Alternatively,  the Nh value
    can be derived from the HI map by Dickey & Lockman (DL), 1990,  Ann.
    Rev.  Ast.  Astr.  28, 215.  The LAB map was obtained by merging two
    surveys covering HI radial velocities from -400km/s to +400  km/s  -
    the  Leiden/Dwingeloo  Survey  (Hartmann  &  Burton  1997),  and the
    Instituto Argentino de Radioastronomía Survey (Arnal  et  al.  2000
    and   Bajaja   et  al.  2005).  The  resulting  velocity  integrated 
    combination had a resolution of approximately 0.5 degrees,  and  was
    resampled  onto  0.675  by  0.675 degree bins in L and B. The DL map
    was obtained by merging several surveys (see Dickey & Lockman  1990)
    and averaged into 1 by 1 degree bins in L and B.
    
    The  Nh  values  are in units of hydrogen atoms cm**-2. The software
    calculates an average Nh using  values  within  N  degree  from  the
    request  position (N is an input parameter, 'disio'). Two Nh average
    values  are  output  for  the  requested  map  (see   the   'usemap' 
    parameter):  a simple average and an average weighted by the inverse
    of the distance from the request position.
    
    'nh' starts by reading a submap M degrees by M degrees, where  M  is
    an  input  parameter, 'size' (default is 3 x 3), from which only the
    subset of values within N  degrees  are  used  for  the  average  Nh
    value.   The  calculated  simple and weighted average are written in
    the paramater file.  There  are  two  parameters  for  the  LAB  map
    (parameter  names  avnh  and  avwnh)  and  two for the DL (parameter
    names alnh and alwnh). These parameters are populated  depending  on
    whether the nh values are calculated for both maps or a single map.
    
    NOTE:  The  maximun  Nh  value  from  the  DL  map  is 2.58e22 at RA
    (2000)=15h 59m 29.383s Dec (2000)=-53d 04m 40.04s  corresponding  to
    (l,  b)  = (329.0, 0.0). In Dickey & Lockman this is instead printed
    as (l, b) = (339.0, 0.0), which is assumed to be a typo.
    
    
PARAMETERS
    
    equinox [equinox]
        Equinox for the input equatorial coordinates.
    
    ra [string]
        Right ascension value. It can be input as hh mm ss.s or degrees.
        
        NOTE: if input as degrees it is necessary to write the value  as
        decimal  (eg.  "270.") otherwise for values bigger than 24 it is
        interpreted incorrectly.
    
    dec [string]
        Declination value. It can be input as dd  mm  ss.s  or  degrees.
        See NOTE in parameter ra.
    
    (disio) [real]
        Search  radius in degrees. Only Nh values within 'disio' degrees
        of (ra, dec)  are  used  in  the  Nh  average  calculation.  The
        default  is 1 degree by 1 degree.  'disio' should be always less
        than or equal to 'size' parameter.
    
    (size) [real]
        Size in degrees of the  submap.  Defaults  to  3  degrees  by  3
        degrees.   By  increasing  the chattiness level (>10) all the Nh
        values from the submap are printed on the screen.
    
    (usemap) [integer]
        Parameter to specify which map to  use.  If  usemap=0  (default)
        then  the  LAB  map  will  be  used,  and the 'avnh' and 'avwnh'
        parameters will be populated  with  the  calculated  values.  If
        usemap=1  then  the  DL  map  will  be  used, and the 'alnh' and
        'alwnh'  parameters  will  be  populated  with  the   calculated 
        values.  If  usemap=2 then the Nh values will be calculated with
        both maps, and the outputs printed  separately.  In  this  case,
        all   of   the  'avnh',  'avwnh',  'alnh',  and  'alwnh'  output 
        parameters  will  be   set   (see   the   individual   parameter 
        descriptions).
    
    (map) [string]
        Name  and  location of the LAB HI map. This parameter should not
        be changed. This is the default map used by this program and  is
        a  FITS file produced by K. Kuntz with a pixel size of 0.675 deg
        (see DESCRIPTION section).
    
    (altmap) [string]
        Name and location of the DL HI map. This  parameter  should  not
        be  changed.  This  map is a FITS file produced by Steve Snowden
        with a pixel size of 0.675 deg (see DESCRIPTION section).
    
    (avnh) [real]
        Output average Nh from the  LAB  map,  calculated  as  described
        above.  This  will be set to zero if either usemap=1 or there is
        an error. Otherwise, it will be set to the calculated Nh value.
    
    (avwnh) [real]
        Output weighted average Nh  from  the  LAB  map,  calculated  as
        described  above.   This  will be set to zero if either usemap=1
        or there  is  an  error.  Otherwise,  it  will  be  set  to  the
        calculated weighted Nh value.
    
    (alnh) [real]
        Output  average  Nh from the DL map. This will be set to zero if
        usemap=0.  Otherwise, it will be set to the calculated Nh value.
    
    (alwnh) [real]
        Output weighted average Nh from the DL map. This will be set  to
        zero  if  usemap=0.  Otherwise, it will be set to the calculated
        Nh value.
    
    (tchat = 10) [integer]
        Terminal chattiness level. If tchat <  lchat  then  a  log  file
        named  ./nh.log  will  be produced containing the output (at the
        chattiness level of lchat). If
    
    (lchat = 0) [integer]
        Logfile chattiness level.
    
    
EXAMPLES
    
        1. Calculate the nh at ra="21 44 41.20" and dec="38 19 18"
        
            > nh equinox=2000 ra="21 44 41.20" dec="38 19 18"
            
    
        2. As above but with the coordinates given as decimal degrees
        
            > nh equinox=2000 ra=326.17166 dec=38.32166
            
    
        3. As above but use both the LAB and DL maps
        
            > nh equinox=2000 ra=326.17166 dec=38.32166 usemap=2
            
    
    
BUGS
    
          No known bugs.
