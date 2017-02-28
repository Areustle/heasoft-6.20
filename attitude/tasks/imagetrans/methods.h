#ifndef METHODS_INCLUDED

#include "image.h"
#include "param.h"

void transform_by_center(     IMAGE* original, IMAGE* transformed, PARAM* param);
void transform_by_interpolate(IMAGE* original, IMAGE* transformed, PARAM* param);
void transform_by_events(     IMAGE* original, IMAGE* transformed, PARAM* param);
void transform_by_area(       IMAGE* original, IMAGE* transformed, PARAM* param);
void transform_flattened_area(IMAGE* original, IMAGE* transformed, PARAM* param);
void transform_flag_area(     IMAGE* original, IMAGE* transformed, PARAM* param);
void transform_by_interalpha( IMAGE* original, IMAGE* transformed, PARAM* param);

/*******************************************************************************
*
*******************************************************************************/
void distribute_value(IMAGE* image, COMBOXFORM* combo, int i, int j,
                             double value);
void distribute_flags(IMAGE* image, COMBOXFORM* combo, int i, int j,
                             long flags);
                    
#define METHODS_INCLUDED
#endif /* METHODS_INCLUDED */
