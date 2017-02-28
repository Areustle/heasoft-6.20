//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <cmath>

// SumComponent
#include <XSModel/Model/Component/SumComponent.h>
// Model
#include <XSModel/Model/Model.h>
// RealResponse
#include <XSModel/Data/Detector/RealResponse.h>
// MultiResponse
#include <XSModel/Data/Detector/MultiResponse.h>
// UserDummyResponse
#include <XSModel/Data/Detector/UserDummyResponse.h>
#include "XSstreams.h"
#include "XSsymbol.h"
// This is the name to use when it is necessary to distinguish by
// response name a  UserDummyResponse from an actual Response or the 
// singleton dummy response.
const std::string USR_DUMMY_RSP("$j27w3*awXS_USR_DUMMY");


// Class UserDummyResponse::NoRealResponse 

UserDummyResponse::NoRealResponse::NoRealResponse (size_t spectrumNumber, size_t detNumber)
     :YellowAlert(" no real response exists for spectrum ")
{
  tcerr << spectrumNumber <<", detector " << detNumber <<'\n';
}


// Class UserDummyResponse 

UserDummyResponse::UserDummyResponse()
  : Response(),
    m_eLow(.01),
    m_eHigh(100.0),
    m_isLog(true),
    m_channelOffset(0),
    m_channelWidth(0),
    m_usingChannels(true),
    m_matrix(),
    m_channels(),
    m_diagRspMode(false),
    m_detNum(0),
    m_eboundsMin(),
    m_eboundsMax(),
    m_dmyStartEngs(),
    m_source(0), // non-owning
    m_arfNames()
{
}

UserDummyResponse::UserDummyResponse(const UserDummyResponse &right)
    : Response(right),
      m_eLow(right.m_eLow),
      m_eHigh(right.m_eHigh),
      m_isLog(right.m_isLog),
      m_channelOffset(right.m_channelOffset),
      m_channelWidth(right.m_channelWidth),
      m_usingChannels(right.m_usingChannels),
      m_matrix(right.m_matrix),
      m_channels(right.m_channels),
      m_diagRspMode(right.m_diagRspMode),
      m_detNum(right.m_detNum),
      m_eboundsMin(right.m_eboundsMin),
      m_eboundsMax(right.m_eboundsMax),
      m_dmyStartEngs(right.m_dmyStartEngs),
      m_source(right.m_source), // shallow copy: non-owning
      m_arfNames(right.m_arfNames)
{
   numEnergies(right.numEnergies());
   // In case right is an inherited type that's inactive, ie.
   // the singleton DummyResponse....
   setActive(true);
}

UserDummyResponse::UserDummyResponse (Real eLow, Real eHigh, int numEnergies, bool isLog, SpectralData* spectrum, Real channelOffset, Real channelWidth, size_t detNum)
    : Response(),
      m_eLow(eLow),
      m_eHigh(eHigh),
      m_isLog(isLog),
      m_channelOffset(channelOffset),
      m_channelWidth(channelWidth),
      m_matrix(),
      m_channels(),
      m_diagRspMode(false),
      // m_detNum is not used here.  It is only used for diagrsp case.
      m_detNum(detNum),
      m_eboundsMin(),
      m_eboundsMax(),
      m_dmyStartEngs(),
      m_source(spectrum) // non-owning
{

  // Note:  This is the constructor that should be called from the dummyrsp command.
  sourceNumber(detNum+1);
  Response::numEnergies(numEnergies);
  setEnergies();
  numChannels(m_source->channels());
  spectrumNumber(m_source->spectrumNumber());
  m_usingChannels = (m_channelWidth != .0);
  setChannels();
  generateResponse();
}

UserDummyResponse::UserDummyResponse (SpectralData* spectrum, size_t detNum)
    : Response(),
      m_eLow(.0),
      m_eHigh(.0),
      m_isLog(false),
      m_channelOffset(.0),
      m_channelWidth(.0),
      m_usingChannels(true),
      m_matrix(),
      m_channels(),
      m_diagRspMode(true),
      m_detNum(detNum),
      m_dmyStartEngs(),
      m_source(spectrum)
{

  // Note:  This is the constructor that should be called from the diagrsp command.
  //        Unlike the case of the dummyrsp command, the owning specturm MUST have
  //        an actual Real or Multi Response object to correspond with this one. 
  //        This condition should already be tested before it gets here, but just
  //        in case, check again.
  Response *genuine = m_source->detector(m_detNum);
  if (!genuine || genuine->toUserDummyResponse())
  {
     throw NoRealResponse(m_source->spectrumNumber(),m_detNum);
  }
  spectrumNumber(m_source->spectrumNumber()); 
  sourceNumber(detNum+1); 
  numChannels(m_source->channels());
  setChannels();
  setEnergies();
  generateResponse();
  if (const MultiResponse* mr = genuine->toMultiResponse())
  {
     m_arfNames = mr->arfNames();
  }
  else if (const RealResponse* rr = genuine->toRealResponse())
  {
     m_arfNames.push_back(rr->arfName());
  }
}


UserDummyResponse::~UserDummyResponse()
{
}


UserDummyResponse* UserDummyResponse::clone () const
{
   return new UserDummyResponse(*this);
}

Model& UserDummyResponse::operator * (Model& model) const
{
  if (!m_usingChannels)
  {
     return model;
  }
  else
  {
     // This is identical to RealResponse::operator *
     int iData(spectrumNumber());

     const RealArray& modFlux = model.modelFlux(iData);
     RealArray foldedMod(.0,numChannels());
     if ( modFlux.size())
     {
             const RealArray& modFluxErr = model.modelFluxError(iData);
             RealArray foldedModErr;
             if (modFluxErr.size()) foldedModErr.resize(numChannels(),.0);
             convolve(modFlux,modFluxErr,foldedMod,foldedModErr);

             model.foldedModel(iData,foldedMod);
             if (modFluxErr.size()) model.foldedModelError(iData,foldedModErr);
     }    
     else
     {
             model.foldedModel(iData,foldedMod);
             model.foldedModelError(iData,RealArray());           
     }           
     return model;

  }
}

void UserDummyResponse::setEnergies ()
{
  if (m_diagRspMode)
  {
     const Response* genuine = m_source->detector(m_detNum);
     numEnergies(genuine->numEnergies());
     energies().resize(genuine->energies().size());
     energies() = genuine->energies();
  }
  else
  {
     energies().resize(numEnergies()+1);
     if (m_isLog)
     {
	Real ldelta = (log(m_eHigh)-log(m_eLow))/numEnergies();
	for (size_t i=0; i<=numEnergies(); ++i)
	{
           energies(i, m_eLow*exp(i*ldelta));
	}
     }
     else
     {
	Real delta = (m_eHigh-m_eLow)/numEnergies();
	energies(0,m_eLow);
	for (size_t i=1; i<=numEnergies(); ++i)
	{
           energies(i, m_eLow + i*delta);
	}
     }
  }
}

SpectralData* UserDummyResponse::source () const
{
  return m_source;
}

void UserDummyResponse::source (SpectralData* spectrum)
{
  m_source = spectrum;
}

void UserDummyResponse::operator * (SumComponent& source) const
{
  int iData(spectrumNumber());

  if (!m_usingChannels)
  {
     // This code is identical to DummyResponse::operator*(SumComponent&)
     const SumComponent& csource = const_cast<SumComponent&>(source);
     source.foldedPhotonFlux(iData).resize(csource.photonArray(iData).size());
     source.foldedPhotonFluxErr(iData).resize(csource.photonErrArray(iData).size());
     source.foldedPhotonFlux(iData) = csource.photonArray(iData);
     source.foldedPhotonFluxErr(iData) = csource.photonErrArray(iData);
  }
  else
  {
     // This code is identical to RealResponse::operator*(SumComponent&)
     const SumComponent& csource = const_cast<SumComponent&>(source);      
     const RealArray& photonFlux = csource.photonArray(iData);
     source.foldedPhotonFlux(iData).resize(numChannels(),0.);
     if ( csource.photonErrArray(iData).size())  
     {
             source.foldedPhotonFluxErr(iData).resize(numChannels(),0.);
     }
     convolve(photonFlux,csource.photonErrArray(iData),source.foldedPhotonFlux(iData),
                               source.foldedPhotonFluxErr(iData));
  }
}

void UserDummyResponse::generateResponse ()
{
   if (m_usingChannels)
   {
      const int nC = static_cast<int>(numChannels());
      const int nE = static_cast<int>(numEnergies());
      const RealArray& phEnergies = energies();
      const BoolArray& noticedChannels = source()->noticedChannels();
      const size_t offset = source()->startChan() - source()->firstChan();
      m_matrix.resize(nC);
      m_dmyStartEngs.resize(nC,0);

      // Just to be safe...
      if (!nC || !nE)
          return;

      const bool isDescending = (m_channels[0] > m_channels[nC]);
      // startChan, endChan, and iterChan refer specifically
      // to the indexing performed in the m_channels array.
      const int startChan = isDescending ? nC : 0;
      const int endChan = isDescending ? 0 : nC;
      const int iterChan = isDescending ? -1 : 1;
      // When iterating in descending order, the m_channels index number
      // will be one higher than its corresponding bin number.  Therefore
      // we need this additional correction when grabbing the corresponding
      // bin from other arrays:
      const int binOffset = isDescending ? 1 : 0;
      int iCurrE = 0;
      int iCurrC = startChan;

      // Find photon energy bin values which bracket each channel bin.
      // First, proceed until first channel bin is found which exceeds the
      // lowest bound of the photon energy bins.
      while (iCurrC != endChan)
      {
         if ((m_channels[iCurrC+iterChan] > phEnergies[0]) &&
	 		noticedChannels[iCurrC+offset-binOffset])
	 {
	    break;
	 }
         iCurrC += iterChan;
      }
      if (iCurrC == endChan)
      {
         // All noticed channel bins fall below the lowest energy bin.  No need
	 // to proceed.
	 return;
      }
      // The channel bin with low bound iCurrC now either overlaps the photon
      // energy bins, or it is entirely beyond the photon energy bins' 
      // upper bound.

      while ((iCurrE <= nE) && (m_channels[iCurrC] >= phEnergies[iCurrE]))
      {
	 ++iCurrE;
      }
      if (iCurrE > nE)
      {
	 // This channel bin and all the ones after it are beyond the range
	 // of the highest photon ebins.  There is no need to continue.
	 return;
      }
      // iCurrE is now the first index such that phEnergies[iCurrE] > 
      // low E of current channel bin. 
      int iPrevHigh = iCurrE;
      // Now, for each channel bin which is in range of the photon ebins,
      // find the photon ebounds which bracket the channel, and assign
      // the response values.
      while (iCurrC != endChan)
      {
         if (noticedChannels[iCurrC+offset-binOffset])
	 {
            int iL = iPrevHigh - 1, iH = -1;
	    while ((iCurrE <= nE) && (m_channels[iCurrC+iterChan] >= phEnergies[iCurrE]))
	    {
	       ++iCurrE;
	    }
	    if (iCurrE <= nE)
	    {
	       // if iCurrE > nE, iH will retain value of -1.
	       iH = iCurrE;
	    }

	    // Assign values to the dummy response matrix.
	    RealArray& chanResp = m_matrix[iCurrC-binOffset];
	    if (iL < 0 || iH < 0)
	    {
	       // This channel bin extends beyond one or both ends of photon bins.
	       if (iL < 0 && iH < 0)
	       {
		  // Both ends of channel bin extend beyond ph ebins.
		  chanResp.resize(nE);
		  m_dmyStartEngs[iCurrC-binOffset] = 0;
		  for (int i=0; i<nE; ++i)
		  {
	             chanResp[i] = 1.0;
		  }
		  break;
	       }
	       else if (iL < 0)
	       {
		  // Low channel bound is below all ph ebins.
		  chanResp.resize(iH);
		  m_dmyStartEngs[iCurrC-binOffset] = 0;
		  for (int i=0; i<iH-1; ++i)
		  {
	             chanResp[i] = 1.0;
		  }
		  chanResp[iH-1] = (m_channels[iCurrC+iterChan]-phEnergies[iH-1])
	       				   /(phEnergies[iH]-phEnergies[iH-1]);
	       }
	       else
	       {
		  // High channel bound is above all ph ebins.
		  chanResp.resize(nE-iL);
		  m_dmyStartEngs[iCurrC-binOffset] = static_cast<size_t>(iL);
		  chanResp[0] = (phEnergies[iL+1]-m_channels[iCurrC])
	       			   /(phEnergies[iL+1]-phEnergies[iL]);
		  for (int i=1; i<(nE-iL); ++i)
		  {
	             chanResp[i] = 1.0;
		  }
		  break;
	       } 
	    } // end if one or both chan boundaries outside of bins.
	    else
	    {
	       // Channel bin falls entirely within ph ebins.
	       if (iH-iL == 1)
	       {
		  // Channel bin falls inside of 1 ph ebin.
		  chanResp.resize(1);
		  chanResp[0] = (m_channels[iCurrC+iterChan]-m_channels[iCurrC])/(phEnergies[iH]-phEnergies[iL]);
		  m_dmyStartEngs[iCurrC-binOffset] = static_cast<size_t>(iL);
	       }
	       else
	       {
		  // Channel may be mapped to several ph bins.
		  size_t nEngs = static_cast<size_t>(iH-iL);
		  chanResp.resize(nEngs);
		  m_dmyStartEngs[iCurrC-binOffset] = static_cast<size_t>(iL);
		  chanResp[0] = (phEnergies[iL+1]-m_channels[iCurrC])
	       				   /(phEnergies[iL+1]-phEnergies[iL]);
		  for (size_t i=1; i<nEngs-1; ++i)
		  {
	             chanResp[i] = 1.0;
		  }
		  chanResp[nEngs-1] = (m_channels[iCurrC+iterChan]-phEnergies[iH-1])
	       				   /(phEnergies[iH]-phEnergies[iH-1]);
	       }
	    }
	    iPrevHigh = iH;
         } // end if channel noticed
	 iCurrC += iterChan;
      } // end loop through channel bins
      // for debugging
/*      for (size_t i=0; i<nC; ++i)
      {
         tcout << i <<":  ";
	 for (size_t j=0; j<m_dmyStartEngs[i]; ++j)
	 {
	    tcout << "0" << "  ";
	 }
	 for (size_t j=0; j<m_matrix[i].size(); ++j)
	 {
	    tcout << m_matrix[i][j] << "  ";
	 }
	 tcout <<std::endl;
      }*/
   } // end if using channels
}

void UserDummyResponse::convolve (const RealArray& flux, const RealArray& fluxErr, RealArray& foldFlux, RealArray& foldFluxErr) const
{
  size_t nC = numChannels();
  RealArray effectiveArea(.0, flux.size());
  getEffectiveArea(effectiveArea);
  RealArray aflux(effectiveArea*flux);
  if (fluxErr.size())
  {
     RealArray fluxVariance(effectiveArea*fluxErr*fluxErr);
     for (size_t i=0; i<nC; ++i)
     {
        size_t sz = m_matrix[i].size();	
	if (sz)
	{
	   const RealArray& matrix_i = m_matrix[i];
	   size_t iEstart = m_dmyStartEngs[i];
           Real foldFlux_i =.0;
	   Real foldFluxErr_i = .0;
	   for (size_t j=0; j<sz; ++j)
	   {
	      foldFlux_i += aflux[iEstart+j]*matrix_i[j];
	      foldFluxErr_i += fluxVariance[iEstart+j]*matrix_i[j];
	   }
	   foldFlux[i] = foldFlux_i;
	   foldFluxErr[i] = foldFluxErr_i;
	}
     }
  }
  else
  {
     for (size_t i=0; i<nC; ++i)
     {
        size_t sz = m_matrix[i].size();	
	if (sz)
	{
	   const RealArray& matrix_i = m_matrix[i];
	   size_t iEstart = m_dmyStartEngs[i];
           Real foldFlux_i(.0);
	   for (size_t j=0; j<sz; ++j)
	   {
	      foldFlux_i += aflux[iEstart+j]*matrix_i[j];
	   }
	   foldFlux[i] = foldFlux_i;
	}
     }
  }
}

void UserDummyResponse::setChannels ()
{
  size_t nC = numChannels();
  bool isDescending = false;
  if (m_diagRspMode)
  {
     Response *genuine = m_source->detector(m_detNum);
     m_channels.resize(nC+1,.0);
     if (nC)
     {
        // m_channels is 1 elem longer than eboundsMin valarray, so
        // the assignment operator can't be used.
        const RealArray& chanMin = genuine->eboundsMin();
        const RealArray& chanMax = genuine->eboundsMax();
        isDescending = (chanMin[0] > chanMin[nC-1]);

        if (isDescending)
           for (size_t i=0; i<nC; ++i)
              m_channels[i] = chanMax[i];
        else
           for (size_t i=0; i<nC; ++i)
              m_channels[i] = chanMin[i];

        if (isDescending)
           m_channels[nC] = chanMin[nC-1];
        else
           m_channels[nC] = chanMax[nC-1];
     }
  }
  else if (m_usingChannels)
  {
     m_channels.resize(nC+1,.0);
     m_channels[0] = m_channelOffset;
     for (size_t i=1; i<=nC; ++i)
     {
	m_channels[i] = m_channelOffset + i*m_channelWidth;
     }
  }


  // The following is a hack.  eboundsMin and Max arrays were
  // created later than the rest of this class, in order to more
  // closely conform with the base Response class interface.  
  // Otherwise, something could erroneously use the Response's
  // default eboundMin/Max function for UserDummyResponse.  
  // They essentially repeat the information that was already
  // stored in m_channels, and must be maintained along with it.
  if (m_diagRspMode || m_usingChannels)
  {
     size_t sz = m_channels.size();
     if (sz)
     {
        m_eboundsMin.resize(sz-1);
        m_eboundsMax.resize(sz-1);
        if (isDescending)
        {
           for (size_t i=0; i<sz-1; ++i)
           {
              m_eboundsMax[i] = m_channels[i];
              m_eboundsMin[i] = m_channels[i+1];
           }
        }
        else
        {
           for (size_t i=0; i<sz-1; ++i)
           {
              m_eboundsMin[i] = m_channels[i];
              m_eboundsMax[i] = m_channels[i+1];
           }
        }
     }
     else
     {
        m_eboundsMin.resize(0);
        m_eboundsMax.resize(0);
     }
  }
}

void UserDummyResponse::getEffectiveArea (RealArray& effectiveArea) const
{
  size_t sz = effectiveArea.size();
  if (m_diagRspMode)
  {
     const Response* genuine = m_source->responseHooks(m_detNum);
     if (const MultiResponse* mr = genuine->toMultiResponse())
     {
        for (size_t i=0; i<mr->effectiveArea().size(); ++i)
	{
	   if (sz != mr->effectiveArea()[i].size())
	   {
	      throw RedAlert("Size mismatch: UserDummyResponse effective area array.\n");
	   }
	   effectiveArea += mr->effectiveArea()[i];
	}
     }
     else
     {
        const RealResponse* rr = genuine->toRealResponse();
	if (!rr)
	{
	   throw RedAlert("Unexpected response type in UserDummyResponse\n");
	}
	if (sz != rr->effectiveArea().size())
	{
	   throw RedAlert("Size mismatch: UserDummyResponse effective area array.\n");
	}
	effectiveArea = rr->effectiveArea();
     }
  }
  else
  {
     // Perfect efficiency.
     for (size_t i=0; i<sz; ++i)
     {
        effectiveArea[i] = 1.0;
     }
  }
}

const RealArray& UserDummyResponse::eboundsMin () const
{
  return m_eboundsMin;
}

const RealArray& UserDummyResponse::eboundsMax () const
{
  return m_eboundsMax;
}

void UserDummyResponse::channels (const RealArray& value)
{
  size_t sz = value.size();
  m_channels.resize(sz);
  m_channels = value;
  if (sz)
  {
     m_eboundsMin.resize(sz-1);
     m_eboundsMax.resize(sz-1);
     for (size_t i=0; i<sz-1; ++i)
     {
        m_eboundsMin[i] = m_channels[i];
        m_eboundsMax[i] = m_channels[i+1];
     }
  }
  else
  {
     m_eboundsMin.resize(0);
     m_eboundsMax.resize(0);
  }
}

// Additional Declarations
