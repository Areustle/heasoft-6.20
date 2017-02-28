//#     Filename:       RangeConvex.hxx
//#
//#     H definitions for  RangeConvex
//#
//#
//#     Author:         Peter Z. Kunszt, based on A. Szalay's code
//#     
//#     Date:           October 16, 1998
//#
//#		Copyright (C) 2000  Peter Z. Kunszt, Alex S. Szalay, Aniruddha R. Thakar
//#                     The Johns Hopkins University
//#

extern istream& operator >>( istream&, RangeConvex &);
extern ostream& operator <<( ostream&, const RangeConvex &);


inline
SpatialConstraint &
RangeConvex::operator [](size_t i) {
  return constraints_[i];
}

inline
size_t
RangeConvex::numConstraints() {
  return constraints_.size();
}
