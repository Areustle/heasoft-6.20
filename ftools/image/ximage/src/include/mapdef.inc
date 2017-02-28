c NOTE: Always preceded mapdef with maxvals.inc
c
c  Global variables for defining and tracking image maps
c
      integer p_map(MAX_MAPS)
      integer mszx(MAX_MAPS), mszy(MAX_MAPS)
      character*(MAX_IDSTR) mapids(MAX_MAPS)
      logical maplock(MAX_MAPS)
      integer icurmap, idismap

      common /mapdef/ p_map, mszx, mszy, maplock, icurmap, idismap
      common /mapstr/ mapids
