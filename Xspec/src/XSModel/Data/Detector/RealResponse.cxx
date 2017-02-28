//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// Model
#include <XSModel/Model/Model.h>
// SumComponent
#include <XSModel/Model/Component/SumComponent.h>
// XSutility
#include <XSUtil/Utils/XSutility.h>
// RealResponse
#include <XSModel/Data/Detector/RealResponse.h>
#include <XSContainer.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/Parameter/ResponseParam.h>
#include <GlobalContainer/ResponseContainer.h>
#include <Data/BackCorr/Background.h>
#include <XSUtil/Numerics/LinearInterp.h>
#include <XSstreams.h>
#include <iostream>
#include <cstring>


// Class RealResponse 

RealResponse::RealResponse()
      : Response(),
        m_rmfName(""), 
	m_arfName(""),
	m_arfRow(0),
        m_savedEffectiveArea(),  
        m_effectiveArea(),
        m_rmfData(0),
        m_source(0),
	m_noticedElements(),
	m_noticedElemPos()
{
}

RealResponse::RealResponse(const RealResponse &right)
      : Response(right), 
        m_rmfName(right.m_rmfName),
	m_arfName(right.m_arfName), 
	m_arfRow(right.m_arfRow),
        m_savedEffectiveArea(right.m_savedEffectiveArea),
        m_effectiveArea(right.m_effectiveArea),
        m_rmfData(right.m_rmfData), // refcounted: let copy constructor RMF worry about memory
        m_source(right.m_source),
	m_noticedElements(right.m_noticedElements),
	m_noticedElemPos(right.m_noticedElemPos)
{
}


RealResponse::~RealResponse()
{
  checkRMFdata();      
}


Model& RealResponse::operator * (Model& model) const
{
   // multiply the model photon array with the sparse representation
   // of the response matrix, taking into account its representation
   // as a set of row-wise run starting at point {binStartChannels[i]}
   // and continuing for length {binRunLengths[i]}

   // get reference to appropriate arrays.   

//        RealArray S(numChannels(),0);
   int iData(spectrumNumber());

   const RealArray& modFlux = model.modelFlux(iData);
   RealArray foldedMod(numChannels());
   if ( modFlux.size())
   {
           const RealArray& modFluxErr = model.modelFluxError(iData);
           RealArray foldedModErr;
           if (modFluxErr.size()) foldedModErr.resize(numChannels());
           convolve(modFlux,modFluxErr,foldedMod,foldedModErr);

// Can't have multiple threads trying to insert arrays into the
// same Model object.  That would cause a race condition.
#pragma omp critical
          {
           model.foldedModel(iData,foldedMod);
           if (modFluxErr.size()) model.foldedModelError(iData,foldedModErr);
          }
   }    
   else
   {
#pragma omp critical
          {
           model.foldedModel(iData,foldedMod);
           model.foldedModelError(iData,RealArray());
          }           
   }           
   return model;
}

void RealResponse::setData (size_t specNum, size_t groupNumber, size_t nRmf)
{

  using namespace XSContainer;

  // Since an extension specifier is optional for the first response extension
  // (extVer = 1), must treat cases such as myResp.rsp and myResp.rsp{1} as the
  // same RMF.  
  string secondTest;
  string::size_type extLoc = string::npos;
  if (m_rmfName.find_first_of('{') == string::npos)
     secondTest = m_rmfName + "{1}";
  else if ((extLoc = m_rmfName.find("{1}")) != string::npos)
     secondTest = m_rmfName.substr(0,extLoc);


  bool readRMF(true);
  std::pair<RMFMapConstIter,RMFMapConstIter> range(responses->RMFmap().equal_range(m_rmfName));
  if (range.first == range.second && secondTest.length())
     range = responses->RMFmap().equal_range(secondTest);
  const SpectralData* spec = source();
  int firstChan = static_cast<int>(spec->firstChan());
  int startChan = static_cast<int>(spec->startChan());
  int endChan = static_cast<int>(spec->endChan());
  RMFMapConstIter p(range.first);
  while (readRMF && p != range.second) 
  {
	const CodeContainer& w = m_source->gqString();
  	if (p->second->compareChanRange(firstChan, startChan, endChan) 
                && p->second->gqMatch(w))
	{
		readRMF = false;
	}

	else
	{
		++p;
	}
  }

  if (!readRMF)
  {
  	rmfData(p->second);
	setSharedDescription(specNum, groupNumber);	
  }

  else
  {
  	read(m_rmfName);
  	setArrays();          
  	setDescription(specNum,groupNumber);
	responses->addToList(RefCountPtr<ResponseMatrix>(m_rmfData)); 
	closeSourceFiles();
  }
}

bool RealResponse::readAuxResponse (int rowNum)
{
  return false;
}

void RealResponse::setEnergies ()
{
  energies().resize(numEnergies()+1);

  energies(0,m_rmfData->energyLow(0));
  for (size_t j = 0; j < numEnergies(); ++j)
  {
        energies(j+1,m_rmfData->energyHigh(j));
  }
}

void RealResponse::setSharedDescription (size_t specNum, size_t groupNum)
{
  numChannels(m_rmfData->eboundsMin().size());
  numEnergies(m_rmfData->energyLow().size());

  dataGroup(groupNum);
  spectrumNumber(specNum);
  setEffectiveArea(m_rmfData->normFactor());
  if (m_arfName.length())
  {     
     readAuxResponse();
  }
  const string fakeitPlaceholder("USE_FAKEIT_RMF");
  const string& rmfTelescope = m_rmfData->telescope();
  const string& rmfInstrument = m_rmfData->instrument();
  const string& rmfChanType = m_rmfData->channelType();

  if (m_source->telescope() == fakeitPlaceholder)
  {
     if (rmfTelescope.size())
        m_source->telescope(rmfTelescope);
     else
        m_source->telescope("UNKNOWN");
  }
  else if (rmfTelescope != m_source->telescope())
     tcout << "Warning: RMF TELESCOPE keyword is not consistent with spectrum" <<std::endl;

  if (m_source->instrument() == fakeitPlaceholder)
  {
     if (rmfInstrument.size())
        m_source->instrument(rmfInstrument);
     else
        m_source->instrument("UNKNOWN");
  }
  else if (rmfInstrument != m_source->instrument())
     tcout << "Warning: RMF INSTRUMENT keyword is not consistent with spectrum" <<std::endl;

  if (m_source->channelType() == fakeitPlaceholder)
  {
     m_source->channelType(rmfChanType);
  }
  else
  {
     const string& specChanStr = m_source->channelType(); 
     if (rmfChanType.length() && rmfChanType != specChanStr)
     {
       tcout << "Warning: RMF CHANTYPE keyword (" << rmfChanType
	     << ") is not consistent with that from spectrum (" 
	     << specChanStr << ")" <<std::endl;        
     }
  }
}

void RealResponse::RMFremove ()
{
  if (m_rmfData.operator->() && m_rmfData->refCount() == 2)      
  {      
        XSContainer::responses->removeByToken(m_rmfData);      
  }
}

bool RealResponse::order (const Response& right) const
{
  const RealResponse& that = static_cast<const RealResponse&>(right);
  bool result (false);
  if (m_rmfName < that.m_rmfName)
  {
        result = true;
  }
  else 
  {
        if (m_rmfName == that.m_rmfName && index() < that.index()) result = true;       
  }
  return result;
}

const RealArray& RealResponse::eboundsMin () const
{
  return m_rmfData->eboundsMin();
}

const RealArray& RealResponse::eboundsMax () const
{
  return m_rmfData->eboundsMax();
}

void RealResponse::convolve (const RealArray& flux, const RealArray& fluxErr, RealArray& foldFlux, RealArray& foldFluxErr) const
{
   const IntegerArray& firstEngs = m_rmfData->energyStart();
   const IntegerArray& numbInGroup = m_rmfData->energyRunLengths();
   const IntegerArray& chanForRun = m_rmfData->channelForRun();
   Real* convArray(m_rmfData->convArray());

   RealArray aflux (m_effectiveArea*flux);
   XSutility::auto_array_ptr<Real> pAflux (new Real[aflux.size()]);
   Real* Aflux (pAflux.get());
   std::copy(&aflux[0],&aflux[0]+aflux.size(),&Aflux[0]);

   const size_t N (numChannels());

   XSutility::auto_array_ptr<Real> pFoldFlux(new Real[numChannels()]);
   Real* FoldFlux (pFoldFlux.get());
   memset(FoldFlux,0,N*sizeof(Real));

   size_t firstEng_i (0);
   size_t chanForRun_i (0);
   Real foldFlux_i (0);
   Real foldFluxErr_i (0);
   Real cnv_j (0);

   size_t noticedElements_i(0);
   size_t noticedSz = m_noticedElements.size();

   if (fluxErr.size())
   {	
   	RealArray fluxVariance(m_effectiveArea*fluxErr*fluxErr);
        XSutility::auto_array_ptr<Real> pFluxVariance (new Real[fluxVariance.size()]);
        Real* FluxVariance (pFluxVariance.get());
        std::copy(&fluxVariance[0],&fluxVariance[0]+fluxVariance.size(),&FluxVariance[0]);

        XSutility::auto_array_ptr<Real> pFoldFluxErr(new Real[numChannels()]);
        Real* FoldFluxErr (pFoldFluxErr.get());
        memset(FoldFluxErr,0,N*sizeof(Real));

	for (size_t i=0; i<noticedSz; ++i)
	{
	   noticedElements_i = m_noticedElements[i];
	   chanForRun_i = chanForRun[noticedElements_i]; 
	   firstEng_i = firstEngs[noticedElements_i]; 
	   foldFlux_i = FoldFlux[chanForRun_i];
	   foldFluxErr_i = FoldFluxErr[chanForRun_i];

           size_t fm (firstEng_i +numbInGroup[noticedElements_i]);
           size_t jj (m_noticedElemPos[i]);

	   for (size_t j = firstEng_i; j < fm; ++j)
	   {
	      cnv_j = convArray[jj];
	      foldFlux_i += Aflux[j]*cnv_j;
	      foldFluxErr_i += FluxVariance[j]*cnv_j;
              ++jj;
	   }
	   FoldFlux[chanForRun_i] = foldFlux_i;
	   FoldFluxErr[chanForRun_i] = foldFluxErr_i;
	}  
        foldFlux = RealArray(FoldFlux,N);
        foldFluxErr = RealArray(FoldFluxErr,N);

   }
   else
   {
        Real ac(0);
	for (size_t i=0; i<noticedSz; ++i)
	{
	   noticedElements_i = m_noticedElements[i];
	   chanForRun_i = chanForRun[noticedElements_i];
	   firstEng_i = firstEngs[noticedElements_i]; 
	   foldFlux_i = FoldFlux[chanForRun_i];
           size_t fm (firstEng_i +numbInGroup[noticedElements_i]);
           size_t jj (m_noticedElemPos[i]);
	   for (size_t j = firstEng_i; j < fm; ++j)
           {
              ac = Aflux[j];
              ac *= convArray[jj];
	      foldFlux_i += ac;
//              ++jj,++x;
              ++jj;

           }
	   FoldFlux[chanForRun_i] = foldFlux_i;
	}
        foldFlux = RealArray(FoldFlux,N);
   } 
//   tcout << " lp= " << x << std::endl;
}

void RealResponse::prepareForFit ()
{
   size_t k=0;
   // ChanForRun, numbInGroup, and firstEngs are all the same length,
   // = # of channel row groups.  
   const IntegerArray& chanForRun = m_rmfData->channelForRun();
   const IntegerArray& numbInGroup = m_rmfData->energyRunLengths();
   const size_t N (chanForRun.size());
   size_t offset = source()->startChan() - source()->firstChan();
   m_noticedElements.clear();
   m_noticedElemPos.clear();
   m_noticedElements.reserve(N);
   m_noticedElemPos.reserve(N);
   for (size_t i=0; i < N ; i++)
   {
      if (m_source->noticedChannels()[chanForRun[i]+offset])
      {
         m_noticedElements.push_back(i);
	 m_noticedElemPos.push_back(k);
      }
      k += numbInGroup[i];
   }
}

const RealArray& RealResponse::efficiency () const
{
  return m_effectiveArea;
}

RealArray RealResponse::sensitivity (const SpectralData* data)
{

  // computes the vector which is the sum of Response / Data variance at
  // each channel.      

  size_t nz = m_noticedElements.size();
  const IntegerArray& chanForRun    = m_rmfData->channelForRun();
  const IntegerArray& firstEnergy   = m_rmfData->energyStart();
  const IntegerArray& numberInGroup = m_rmfData->energyRunLengths();
  Real* convArray(m_rmfData->convArray());
  RealArray sens(0.,numEnergies());
  RealArray effAreaSq (m_effectiveArea*m_effectiveArea);
  Real ac(0),respElt(0);
  size_t ne(0), ch(0), first(0), last(0), jpos(0);
  for (size_t j = 0; j < nz; ++j)
  {
        ne = m_noticedElements[j];
        ch = chanForRun[ne];
        first =  firstEnergy[ne];
        last  = first + numberInGroup[ne];
        jpos  = m_noticedElemPos[j];
        Real var = data->variance(ch);
        if (data->background())
           var += data->background()->variance()[ch];
        for (size_t k = first; k < last; ++k )
        {
                respElt = convArray[jpos];
                ac =  effAreaSq[k]*respElt*respElt;
                sens[k] += ac/var;
                ++jpos;
        } 
  }


  return sens;
}

void RealResponse::operator * (SumComponent& source) const
{
   int iData(spectrumNumber());
  // one of those rare uses for const_cast which seem to be acceptable.
  // source is cast to const so it can call the const versions of
  // photonArray(), etc ... the non-const versions are protected.
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

void RealResponse::shiftEffectiveArea (const RealArray& newEnergies)
{
  const Real FUZZY = 1.0e-6;
  // Lazy initialization of m_savedEffectiveArea array. If gain
  // command is never called, this need not be done.
  if (!m_savedEffectiveArea.size())
  {
     m_savedEffectiveArea.resize(m_effectiveArea.size());
     m_savedEffectiveArea = m_effectiveArea;
  }
  size_t inStart, outStart;
  if (!Numerics::Rebin::findFirstBins(energies(), newEnergies, FUZZY,
                inStart, outStart))
  {
     string msg("***No new energy bins overlap the old, cannot apply gain.");
     throw GainError(msg);
  }
  IntegerArray startBin, endBin;
  RealArray startWeight, endWeight;
  Numerics::Rebin::initializeBins(energies(), newEnergies, FUZZY, inStart,
                outStart, startBin, endBin, startWeight, endWeight);
  // this function modifies m_effectiveArea
  Numerics::Rebin::gainRebin(m_savedEffectiveArea, startBin, endBin, 
                startWeight, endWeight, m_effectiveArea);
}

void RealResponse::restoreEffectiveArea ()
{
  size_t sz = m_savedEffectiveArea.size();
  if (sz)
  {
     if (m_effectiveArea.size() == sz)
     {
        m_effectiveArea = m_savedEffectiveArea;
     }
     else
     {
        throw RedAlert("Size mismatch between old and new effectiveArea array.");
     }
  }
}

void RealResponse::calcEffAreaPerChan (RealArray& effArea)
{
   // Can't assume prepareForFit was called.  There may be no model
   // associated with this response.
   prepareForFit();
   const RealArray& ebins = energies();
   const size_t nBins = ebins.size()-1;
   RealArray dummyError;
   effArea.resize(numChannels(), .0);
   // flatFlux corresponds to a constant rate/keV.  Therefore the flux
   // in each bin is proportional to the bin width.
   RealArray flatFlux(.0, nBins);
   Real lowerE = ebins[0];
   for (size_t i=0; i<nBins; ++i)
   {
      Real higherE = ebins[i+1];
      flatFlux[i] = higherE - lowerE;
      lowerE = higherE;
   }
   convolve(flatFlux, dummyError, effArea, dummyError);

   const RealArray& chanEmin = eboundsMin();
   const RealArray& chanEmax = eboundsMax();
   const size_t nChans = chanEmin.size();
   RealArray chanWidths(.0, nChans);
   if (nChans != effArea.size())
   {
      throw RedAlert("Channel size mismatch in RealResponse::calcEffAreaPerChan");
   }
   chanWidths = chanEmax - chanEmin;
   effArea /= chanWidths;
}

void RealResponse::setZeroGainEffectiveArea (const RealArray& newEffArea)
{
   if (m_savedEffectiveArea.size())
   {
      if (m_savedEffectiveArea.size() != newEffArea.size())
      {
         throw RedAlert("Size mismatch between old and new effective area array.");
      }
      m_savedEffectiveArea = newEffArea;
   }
}

// Additional Declarations
