#ifndef METHODS_INCLUDED

#include "imagetranslib/image.h"
#include "imagetranslib/param.h"

void transform_by_center(     IMAGE* original, IMAGE* transformed, IMAGETRANS* param);
void transform_by_interpolate(IMAGE* original, IMAGE* transformed, IMAGETRANS* param);
void transform_by_events(     IMAGE* original, IMAGE* transformed, IMAGETRANS* param);
void transform_by_area(       IMAGE* original, IMAGE* transformed, IMAGETRANS* param);
void transform_flattened_area(IMAGE* original, IMAGE* transformed, IMAGETRANS* param);
void transform_flag_area(     IMAGE* original, IMAGE* transformed, IMAGETRANS* param);
void transform_by_interalpha( IMAGE* original, IMAGE* transformed, IMAGETRANS* param);

/*******************************************************************************
*
*******************************************************************************/
void distribute_value(IMAGE* image, COMBOXFORM* combo, int i, int j,
                             double value);
void distribute_flags(IMAGE* image, COMBOXFORM* combo, int i, int j,
                             long flags);
                    
#define METHODS_INCLUDED
#endif /* METHODS_INCLUDED */

/*
  Revision Log:
  $Log: methods.h,v $
  Revision 1.1  2016/01/16 00:36:57  rshill
  Copied from attitude/tasks/imagetrans.  The only change
  is renaming the PARAM structure to IMAGETRANS.


*/
