
#include <LinearInterp.h>
#include <XSUtil/Error/Error.h>

namespace Numerics {

  namespace Rebin {


    void initializeBins(const RealArray& inputArray, const RealArray& interpolant,
			const Real FUZZY, size_t& inputBin, size_t& outputBin,
			IntegerArray& startBin, IntegerArray& endBin, 
			RealArray& startWeight, RealArray& endWeight)
    {
      // The assumption for original starting values is that 
      // inputBin and outputBin are the lowest values which
      // satisfy:
      //    interpolant[outputBin]*(1.0+FUZZY) >= inputArray[inputBin]
      // Therefore if there's any overlap between the two arrays,
      // at least one of inputBin,outputBin must be 0.
      size_t Nout (interpolant.size() - 1);
      size_t Nin  (inputArray.size() - 1);
      startWeight.resize(Nout,0);
      endWeight.resize(Nout,0);
      startBin.clear();
      endBin.clear();
      startBin.resize(Nout,0);
      endBin.resize(Nout,0);

      while ( outputBin < Nout ) {

	if ( interpolant[outputBin+1] <= inputArray[inputBin+1] ) {

	  endBin[outputBin] = 0;
	  startBin[outputBin] = inputBin;
	  startWeight[outputBin] 
	    = (interpolant[outputBin+1]- interpolant[outputBin])
	    / ( inputArray[inputBin+1] - inputArray[inputBin]);
	  if (interpolant[outputBin+1]*(1.0+FUZZY) >= inputArray[inputBin+1]) {
	    ++inputBin;
	  }
	  ++outputBin;

	} else {

	  startBin[outputBin] = inputBin;
	  startWeight[outputBin] 
	    = ( inputArray[inputBin+1] - interpolant[outputBin] )
	    / (inputArray[inputBin+1] - inputArray[inputBin]);
	  while ( inputBin < Nin 
		  && interpolant[outputBin+1] > inputArray[inputBin+1] ) {
	    ++inputBin;
	  }

	  if ( inputBin >= Nin ) {
	    endBin[outputBin] = Nin - 1;
	    endBin[outputBin] != startBin[outputBin] 
	      ?  endWeight[outputBin] = 1. 
	      : endBin[outputBin] = 0;
	    ++outputBin;       

	  } else {

	    endBin[outputBin] = inputBin;
	    endWeight[outputBin] 
	      = (interpolant[outputBin+1] - inputArray[inputBin])
	      / ( inputArray[inputBin+1] - inputArray[inputBin]);
	    if (interpolant[outputBin+1]*(1.0+FUZZY) >= inputArray[inputBin+1]) {
	      ++inputBin;
	    }
	    ++outputBin;
	  }

	}

	if ( inputBin >= Nin ) outputBin = Nout + 1;
      }
    }

    void rebin(const RealArray& inputArray, const IntegerArray& startBin, 
	       const IntegerArray& endBin, const RealArray& startWeight, 
	       const RealArray& endWeight, RealArray& outputArray)
    {
      rebin(inputArray, startBin, endBin, startWeight, endWeight, outputArray,
	    (Real)0.0, (Real)0.0);
    }

    void rebin(const RealArray& inputArray, const IntegerArray& startBin, 
	       const IntegerArray& endBin, const RealArray& startWeight, 
	       const RealArray& endWeight, RealArray& outputArray, 
	       const Real lowValue, const Real highValue)
    {
      size_t Nout(outputArray.size());
      bool isLow(true);
      for (size_t j = 0; j < Nout; ++j) {

	if ( startBin[j] >= 0 ) {
	  isLow = false;
	  outputArray[j] = startWeight[j]*inputArray[startBin[j]];
	  if ( endBin[j] > startBin[j] ) {
	    for ( int k = startBin[j] + 1; k < endBin[j]; ++k) {
	      outputArray[j] += inputArray[k];
	    } 
	    outputArray[j] += endWeight[j]*inputArray[endBin[j]];
	  }      

	} else {

	  if ( isLow ) {
	    outputArray[j] = lowValue;
	  } else {
	    outputArray[j] = highValue;
	  }

	}

      }

    }

   void interpolate(const RealArray& inputArray,const IntegerArray& startBin, 
		    const IntegerArray& endBin, const RealArray& startWeight, 
		    const RealArray& endWeight, RealArray& outputArray, 
		    bool exponential)
   {
     Real lowValue(0.0);
     Real highValue(0.0);
     interpolate(inputArray, startBin, endBin, startWeight, endWeight, 
		 outputArray, exponential, lowValue, highValue);
   }

   void interpolate(const RealArray& inputArray,const IntegerArray& startBin, 
		    const IntegerArray& endBin, const RealArray& startWeight, 
		    const RealArray& endWeight, RealArray& outputArray, 
		    bool exponential, const Real lowValue, const Real highValue)
   {
      size_t Nout (outputArray.size());
      bool isLow = true;

      for ( size_t j = 0; j < Nout; ++j) {

	Real denominator (0.);
	outputArray[j] = startWeight[j]*inputArray[startBin[j]];
	denominator += startWeight[j];
	if ( endBin[j] > startBin[j] ) {
	  for ( int k = startBin[j] + 1; k < endBin[j]; ++k) {
	    outputArray[j] += inputArray[k];
	    denominator += 1;
	  }
	  outputArray[j] += endWeight[j]*inputArray[endBin[j]];
	  denominator += endWeight[j];
	}

	if ( denominator > 0 ) {
	  isLow = false;
	  outputArray[j] /= denominator;
	} else {
	  if ( isLow ) {
	    outputArray[j] = ( exponential ? -log(lowValue) :  lowValue) ;
	  } else {
	    outputArray[j] = ( exponential ? -log(highValue) :  highValue) ;
	  }
	}

      }

      if ( exponential ) {
	for (size_t j = 0; j < Nout; ++j) { 
	  outputArray[j] > 100. 
	    ?  outputArray[j] = 0.
	    : outputArray[j] = exp(-outputArray[j]);
	}           
      }

   }

    bool findFirstBins(const RealArray& inputArray, const RealArray& outputArray,
		       const Real FUZZY, size_t& inputStart, size_t& outputStart)
    {
      // Find first bin in outputArray which which is contained
      // in range of inputArray, and the bin inputArray to
      // which it corresponds.  To conform with inibin.f function,
      // an output bin only partially contained on LOW end of 
      // inputArray is NOT accepted.  It IS accepted if it is
      // partially overlapping the HIGH end.  If no output bin
      // is found, function returns false.
    
      bool isOverlap = false;
      inputStart = outputStart = 0;

      if (inputArray.size() && outputArray.size()) {

	size_t nInBins = inputArray.size() - 1;
	size_t nOutBins = outputArray.size() - 1;
	while (outputStart < nOutBins && outputArray[outputStart]*(1.+FUZZY)
	       < inputArray[0]) ++outputStart;

	if (outputStart < nOutBins) {
	  while (inputStart < nInBins && inputArray[inputStart+1] 
		 < outputArray[outputStart]*(1.+FUZZY)) ++inputStart;
	  if (inputStart < nInBins) isOverlap = true;
	}

      }

      return isOverlap;
    }

    void gainRebin(const RealArray& inputArray, const IntegerArray &startBin,
		   const IntegerArray& endBin, const RealArray& startWeight,
		   const RealArray& endWeight, RealArray& outputArray)
    {
      // ASSUMPTION: All arrays passed here are the SAME SIZE
      size_t sz = outputArray.size();

      // Partial check.
      if (sz != inputArray.size() || sz != startBin.size()) {
	throw RedAlert("Array size mismatch occured in gainRebin function.");
      }

      for (size_t i=0; i<sz; ++i) {
	Real total = .0;
	Real& output_i = outputArray[i];
	const int startBin_i = startBin[i];
	const int endBin_i = endBin[i];
	output_i = startWeight[i]*inputArray[startBin_i];
	total += startWeight[i];
	if (endBin_i > startBin_i) {
	  for (int j=startBin_i+1; j<endBin_i; ++j) {
	    // If we're here, output bin spreads over more than
	    // just 2 input bins.
	    output_i += inputArray[j];
	    total += 1.0;
	  }
	  output_i += endWeight[i] *inputArray[endBin_i];
	  total += endWeight[i];
	}
	if (total > .0) {
	  output_i /= total;
	} else {
	  output_i = 1.0;
	}
      }
    }

    void linInterpInteg(const RealArray& inputEnergy, const RealArray& inputdFlux, 
			const RealArray& outputEnergy, RealArray& outputFlux, 
			size_t& inputBin, size_t& outputBin)
    {
      // This calculates the integrated outputFlux[i] between outputEnergy[i] and
      // outputEnergy[i+1] by linearly interpolating on the input flux/energy in
      // inputdFlux which has values at inputEnergy.

      outputFlux.resize(outputEnergy.size()-1);
      outputFlux = 0.0;

      Real Ein = inputEnergy[inputBin];
      Real Ein1 = inputEnergy[inputBin+1];
      Real dFlin = inputdFlux[inputBin];
      Real dFlin1 = inputdFlux[inputBin+1];

      Real EoutL = outputEnergy[outputBin];
      Real EoutH = outputEnergy[outputBin+1];

      // if outputBin > 0 we need to start by calculating outputFlux[outputBin-1]
      // note that if outputBin > 0 then inputBin must be 0.

      if ( outputBin > 0 ) {

	// first add a contribution from outputEnergy[outputBin-1] to inputEnergy[0]
	outputFlux[outputBin-1] += dFlin*(Ein-outputEnergy[outputBin-1]);

	// then a contribution from inputEnergy[0] to outputEnergy[outputBin]
	outputFlux[outputBin-1] += 0.5 * (EoutL-Ein)
	  * ( dFlin*(1.0+Ein1-EoutL) + dFlin1*(EoutL-Ein) );

      }

      // loop till we run out of outputEnergy bins or inputEnergy points

      while ( outputBin < outputEnergy.size()-1 && inputBin < inputEnergy.size()-2 ) {

	// if the next input point lies in the next output bin

	if ( Ein1 >= EoutH ) {

	  if ( Ein <= EoutL ) {

	    Real EoutM = 0.5*(EoutL+EoutH);
	    outputFlux[outputBin] += (EoutH-EoutL)
	      * ( dFlin*(Ein1-EoutM) + dFlin1*(EoutM-Ein) ) / (Ein1-Ein);

	  } else {

	    outputFlux[outputBin] += 0.5 * (EoutH-Ein)
	      * ( dFlin*(Ein1-Ein+Ein1-EoutH) + dFlin1*(EoutH-Ein) ) / (Ein1-Ein);

	  }

	  outputBin++;
	  EoutL = EoutH;
	  EoutH = outputEnergy[outputBin+1];

	  // now do the case of the next input point lies in the current output bin

	} else if ( Ein < EoutH ) {

	  if ( Ein <= EoutL ) {

	    outputFlux[outputBin] += 0.5 * (Ein1-EoutL)
	      * ( dFlin*(Ein1-EoutL) + dFlin1*(Ein1-Ein+EoutL-Ein) ) / (Ein1-Ein);

	  } else {

	    outputFlux[outputBin] += 0.5 * (Ein1-Ein) * (dFlin + dFlin1);

	  }

	  inputBin++;
	  Ein = Ein1;
	  Ein1 = inputEnergy[inputBin+1];
	  dFlin = dFlin1;
	  dFlin1 = inputdFlux[inputBin+1];

	}

      }

      // Check whether we have got to the end of the inputEnergy array before we
      // have done all the output bins

      if ( inputBin == inputEnergy.size()-1 && outputBin < outputEnergy.size()-1 ) {
	outputFlux[outputBin] += dFlin*(EoutH-Ein);
      }

      return;
    }

  }  // namespace Rebin

}  // namespace Numerics
