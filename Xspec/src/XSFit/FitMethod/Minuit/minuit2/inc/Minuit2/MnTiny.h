// @(#)root/minuit2:$Id: MnTiny.h,v 1.1 2013/06/14 15:34:06 kaa Exp $
// Authors: M. Winkler, F. James, L. Moneta, A. Zsenei   2003-2005  

/**********************************************************************
 *                                                                    *
 * Copyright (c) 2005 LCG ROOT Math team,  CERN/PH-SFT                *
 *                                                                    *
 **********************************************************************/

#ifndef ROOT_Minuit2_MnTiny
#define ROOT_Minuit2_MnTiny

namespace ROOT {

   namespace Minuit2 {


class MnTiny {
  
public:
  
  MnTiny() : fOne(1.) {}
  
  ~MnTiny() {}
  
  double One() const;
  
  double operator()(double epsp1) const;
  
private:
  
  double fOne;
};

  }  // namespace Minuit2

}  // namespace ROOT

#endif  // ROOT_Minuit2_MnTiny
