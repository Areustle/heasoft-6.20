#include <Integrate.h>
#include <XSUtil/Utils/XSutility.h>

namespace Numerics {

   std::pair<Real,Real> integrationKernel (const RealArray& energy, 
        const RealArray& fluxArray, const Real& eMin, const Real& eMax)
   {
        std::pair<Real,Real> result(0.,0.);
        //specIdx / nrgIdx ::= store position for specific energy found. 
        //Unlike Fortran, we have a 2-D array, needing a pair of 
        //subscripts.
        static const Real KEVTOERGS (0.801096E-9);
        // already know that the find will succeed... 

        if (eMax <= energy[0] || eMin >= energy[energy.size()-1])
        {
           return result;
        }
        int nEmin(0);
        // find will return nEmin = -1 if eMin < energy[0]
        XSutility::find(energy,eMin,nEmin);
        Real lowE (eMin);
        Real lowESq (eMin*eMin);
        if (nEmin < 0)
        {
           nEmin = 0;
           lowE = energy[0];
           lowESq = lowE*lowE;
        }
        // the +1 is necessary because the energy array has one more
        // point than the modelArray.
        nEmin += 1;
        // high E is the upper bound to the energy bin containing eMin.
        Real highE = energy[nEmin];
        Real highESq = highE * highE;

        Real& flux = result.first;
        Real& eFlux = result.second;


        // recall that the modelFlux array contains the integrated fluxes in each bin,
        // so integration is just a matter of adding them up (and for eFlux, scaling).
        Real mFlux(fluxArray[nEmin - 1]);    

        Real prevNrg = energy[nEmin - 1];

        // flux all in one bin
        if (highE > eMax) 
        {
            flux = mFlux * ((eMax - lowE) / (highE - prevNrg));
            eFlux = mFlux * ((eMax * eMax - lowESq) / (highE - prevNrg));
            eFlux *= KEVTOERGS;
            return result;
        }
        // start point for integration: contribution from fraction of
        // the lowest bin.
        else  
        {
            flux = mFlux * ((highE - lowE) / (highE - prevNrg));
            eFlux = mFlux * ((highESq - lowESq) / (highE - prevNrg));
        }

        int N (energy.size());

        while ( ++nEmin < N && energy[nEmin] <= eMax) 
        {
            mFlux = fluxArray[nEmin - 1];

            flux += mFlux;

            lowE = highE;
            lowESq = highESq;
            highE = energy[nEmin];
            highESq = highE * highE;

            if (highE != lowE)
            {
                eFlux += mFlux * ((highESq - lowESq) / (highE - lowE));
            }

        }
        // fractional remainder of upper bin.
        if (nEmin < N) 
        {
            lowE = highE;
            lowESq = highESq;
            highE = eMax;
            highESq = highE * highE;

            Real currNrg = energy[nEmin];

            if (currNrg != lowE) 
            {
                mFlux = fluxArray[nEmin-1];
                Real quot = mFlux / (currNrg - lowE);
                flux += quot * (highE - lowE);
                eFlux += quot * (highESq - lowESq);
            }

        }
        //need to check up on scientific notation in C++
        eFlux *= KEVTOERGS;



      return result;
   }


}
