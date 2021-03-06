// @(#)root/minuit2:$Id: MnEigen.h,v 1.1 2013/06/14 15:34:04 kaa Exp $
// Authors: M. Winkler, F. James, L. Moneta, A. Zsenei   2003-2005  

/**********************************************************************
 *                                                                    *
 * Copyright (c) 2005 LCG ROOT Math team,  CERN/PH-SFT                *
 *                                                                    *
 **********************************************************************/

#ifndef ROOT_Minuit2_MnEigen
#define ROOT_Minuit2_MnEigen

#include "Minuit2/MnConfig.h"
#include <vector>

namespace ROOT {

   namespace Minuit2 {


class MnUserCovariance;

/**
   API class for calculating the eigenvalues of symmetric matrix
 */

class MnEigen {

public:

  MnEigen() {}

  ~MnEigen() {}

  /// calculate the eigenvalues
  std::vector<double> operator()(const MnUserCovariance&) const;

private:

};

  }  // namespace Minuit2

}  // namespace ROOT

#endif  // ROOT_Minuit2_MnEigen
