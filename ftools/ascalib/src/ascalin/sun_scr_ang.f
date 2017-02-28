C******************************************************************************
C SUBROUTINE:
C      sun_src_ang
C
C DESCRIPTION:
C      compute the sun-source angle for a given ascatime and ra, dec
C
C AUTHOR/DATE:
C       Eric Gotthelf    December 1993
C	NASA/GSFC
C
C MODIFICATION HISTORY:
C
C USAGE:
C      call sun_src_ang(ascatime, ra, dec, sun_source)
C
C ARGUMENTS:
C   input:
C	ascatime   - time in ascatime
C	ra, dec    - equatorial coordinates of source (radians)
C
C   output:
C	sun_source - angle between sun and source at ascatime (radians)
C       long       - longitude of sun (radians)
C
C PRIMARY LOCAL VARIABLES:
C	lb2ad              - l,b to RA, DEC transformation matrix
C	source, sun, sunlb - source, sun cartisian coordinates
C
C CALLED ROUTINES:
C     aberration   - to return logitude of sun at time ascatime
C     matrix_proj3 - matrix multiplication
C     dot_prod     - compute dot product
C     
C******************************************************************************
        
        subroutine sun_src_ang(ascatime, ra, dec, long, sun_source)
        double precision ascatime
        real ra, dec, sun_source
        
        real dum, long
        double precision dot_prod, source(3), sun(3), sunlb(3)
        double precision dot, invcos, lb2ad(3,3)

        data lb2ad /1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.917434d0, 0.397888d0,
     &       0.0d0, -0.397888d0, 0.917434d0/
        
        call aberration(ascatime, dum, dum, dum, dum, long)
        
c     convert source to cartesian coords:
        
        source(1) = cos(ra) * cos(dec) 
        source(2) = sin(ra) * cos(dec) 
        source(3) = sin(dec)
        
c     convert to sun to cartesian coords:
        
        sunlb(1) = cos(long) 
        sunlb(2) = sin(long)
        sunlb(3) = 0.0
        
c     convert to ra dec system:
        
        call matrix_proj3(lb2ad, sunlb, sun)
        
c     take dot product to get angle:
        
        sun_source = real(acos(dot_prod(source, sun, 3)))
        
        end




