//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// Model
#include <XSModel/Model/Model.h>
// SumComponent
#include <XSModel/Model/Component/SumComponent.h>
// XSutility
#include <XSUtil/Utils/XSutility.h>
// MultiResponse
#include <XSModel/Data/Detector/MultiResponse.h>
#include <XSContainer.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/Parameter/ResponseParam.h>
#include <GlobalContainer/ResponseContainer.h>
#include <Data/BackCorr/Background.h>
#include <XSUtil/Numerics/LinearInterp.h>
#include <XSstreams.h>
#include <cstring>


// Class MultiResponse::positionInfo 

// Class MultiResponse 

MultiResponse::MultiResponse()
      : Response(),
        m_currentRMF(0),
        m_unionSize(0),
        m_rmfData(),
        m_effectiveArea(),
        m_source(0),
	m_rmfNames(),
	m_arfNames(),
        m_noticedElements(),
        m_noticedElemPos(),
        m_energyStart(),
        m_energyRunLengths(),
        m_channelForRun(),
        m_savedEffectiveArea()
{
}

MultiResponse::MultiResponse(const MultiResponse &right)
      : Response(right),
        m_currentRMF(right.m_currentRMF),
        m_unionSize(right.m_unionSize),
        m_rmfData(right.m_rmfData), // RMFs are reference counted. Check explicitly 
                                    // that this works as expected (increases number of
                                    // references for multi response).
        m_effectiveArea(right.m_effectiveArea), // stack-allocated.
        m_source(right.m_source),
	m_rmfNames(right.m_rmfNames),
	m_arfNames(right.m_arfNames),
        m_noticedElements(right.m_noticedElements),
        m_noticedElemPos(right.m_noticedElemPos),
        m_energyStart(right.m_energyStart),
        m_energyRunLengths(right.m_energyRunLengths),
        m_channelForRun(right.m_channelForRun),
        m_savedEffectiveArea(right.m_savedEffectiveArea)
{
}


MultiResponse::~MultiResponse()
{
  checkRMFdata();      
}


Model& MultiResponse::operator * (Model& model) const
{
   int iData(spectrumNumber());

   const RealArray& modFlux = model.modelFlux(iData);
   RealArray foldedMod(0.,numChannels());
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

void MultiResponse::setData (size_t specNum, size_t groupNumber, size_t nRmf)
{
  using namespace XSContainer;

  m_rmfData.push_back(0);
  m_currentRMF = nRmf;
  size_t nr(responses->RMFmap().count(m_rmfNames[nRmf]));
  bool readRMF(true);
  const SpectralData* spec = source();
  int firstChan = static_cast<int>(spec->firstChan());
  int startChan = static_cast<int>(spec->startChan());
  int endChan = static_cast<int>(spec->endChan());

  if ( nr > 0 ) 
  {

	  RMFMapConstIter p(responses->RMFmap().lower_bound(m_rmfNames[nRmf]));
	  RMFMapConstIter last(responses->RMFmap().upper_bound(m_rmfNames[nRmf]));

	  while (readRMF && p != last) 
	  {
		  const CodeContainer& w = m_source->gqString();
		  if (p->second->compareChanRange(firstChan, startChan, endChan) 
                        && p->second->gqMatch(w))
                  {
                          readRMF = false;
                  }

		  else
		     ++p;
	  }

          if (!readRMF)
          {
 		  rmfData(m_rmfData.size()-1, p->second);
		  if (!numEnergies() ) numEnergies(p->second->energyHigh().size());
		  if (!numChannels() ) numChannels(p->second->detectorChannels());
		  setSharedDescription(specNum,groupNumber);   
                  // grab a reference from the ResponseContainer::RMFmap 
                  // object if necessary.
                  // the setDescription info might  be different.                                      
          }                        
  }

  if (readRMF)
  {
	  read(m_rmfNames[nRmf]);
	  setArrays();
      	  setDescription(specNum,groupNumber);
          responses->addToList(RefCountPtr<ResponseMatrix>(m_rmfData.back()));
	  closeSourceFiles();
  }

}

bool MultiResponse::readAuxResponse ()
{
  return false;
}

void MultiResponse::setEnergies ()
{
  RealArray& responseEnergy = energies();
  responseEnergy.resize(numEnergies()+1);

  responseEnergy[0] = m_rmfData[0]->energyLow(0);
  for (size_t j = 0; j < numEnergies(); ++j)
  {
               responseEnergy[j+1] = m_rmfData[0]->energyHigh(j);
  }
}

void MultiResponse::setSharedDescription (size_t specNum, size_t groupNum)
{

  dataGroup(groupNum);
  spectrumNumber(specNum);
  effectiveArea(m_currentRMF, m_rmfData[m_currentRMF]->normFactor());
  readAuxResponse();

  const string fakeitPlaceholder("USE_FAKEIT_RMF");
  const string& rmfTelescope = m_rmfData[m_currentRMF]->telescope();
  const string& rmfInstrument = m_rmfData[m_currentRMF]->instrument();
  const string& rmfChanType = m_rmfData[m_currentRMF]->channelType();

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

void MultiResponse::RMFremove ()
{
  size_t n (m_rmfData.size());
  if (n) 
  {
        if (m_rmfData[0].operator->() && m_rmfData[0]->refCount() == 2)
        {
                for (size_t j = 0; j < n; ++j) XSContainer::responses->removeByToken(m_rmfData[j]);
        }
  }
}

bool MultiResponse::order (const Response& right) const
{
  const MultiResponse& that = static_cast<const MultiResponse&>(right);
  bool result (false);
  if (m_rmfNames[0] < that.m_rmfNames[0])
  {
       result = true;
  }
  else        
  {
       if (m_rmfNames[0] == that.m_rmfNames[0] && index() < that.index()) result = true;
  }
  return result;
}

void MultiResponse::combineRMF (Real* cvUnion) const
{
  size_t numRmfs(m_rmfData.size());

  // initialize
  size_t remainingRmfs = numRmfs;  // # of convArrays with uncounted elements
  std::vector<positionInfo> currPosInfo(numRmfs);
  std::vector<positionInfo>::iterator first, last;
//  cvUnion.resize(m_unionSize);

  remainingRmfs = numRmfs;  // # of convArrays with uncounted elements
  currPosInfo.resize(numRmfs);
  for (size_t i=0; i<numRmfs; i++)
  {
     // rmfNum and rmfSize will remain constant after initialization
     currPosInfo[i].rmfNum = i;
     currPosInfo[i].rmfSize = m_rmfData[i]->convArraySize();
     currPosInfo[i].pos = 0;
     currPosInfo[i].posValue = m_rmfData[i]->positions(0);
  }
  // initialize heap with the matrix position values of the first elements 
  // in each of the convArrays
  first = currPosInfo.begin();
  last = currPosInfo.end();
  make_heap(first,last,std::greater<positionInfo>());
  ResponseMatrix::RMFLONG prevElement = currPosInfo[0].posValue+1;

  size_t j = 0, k = 0;
  size_t nChanRowGroup = 0;
  size_t nInGroup = m_energyRunLengths[0];
  while (remainingRmfs)
  {
     pop_heap(first,last,std::greater<positionInfo>());
     positionInfo &back = currPosInfo.back();
     // In case of duplicate position values, count elements only once.
     if (back.posValue != prevElement)
     {
        if (j >= nInGroup)
	{
	   j=0;
	   nChanRowGroup++;
	   nInGroup = m_energyRunLengths[nChanRowGroup];
	}
        cvUnion[k] = m_rmfData[back.rmfNum]->convArray(back.pos) *
	       m_effectiveArea[back.rmfNum][m_energyStart[nChanRowGroup]+j];
	prevElement = back.posValue;
     }
     else
     {
        --j;
        cvUnion[--k] += m_rmfData[back.rmfNum]->convArray(back.pos) *
	       m_effectiveArea[back.rmfNum][m_energyStart[nChanRowGroup]+j];
     }
     k++;
     j++;

     // Now find the next smallest remaining position value from the 
     // original RMFs.
     if (++back.pos < back.rmfSize)
     {
        back.posValue = m_rmfData[back.rmfNum]->positions(back.pos);
	push_heap(first,last,std::greater<positionInfo>());
     }
     else // One of the original RMFs has reached the end of its non-zero
          // matrix elements.
     {
        remainingRmfs--;
	last--;
	currPosInfo.pop_back();
     }
  }
}

void MultiResponse::prepareForFit ()
{
  size_t numRmfs(m_rmfData.size());
  size_t remainingRmfs = numRmfs;  // # of convArrays with uncounted elements
  std::vector<positionInfo> currPosInfo(numRmfs);
  std::vector<positionInfo>::iterator first, last;
  std::vector<ResponseMatrix::RMFLONG> posUnion;

  // This section needs to be performed exactly once during lifetime of
  // the MultiResponse object.
  if (m_unionSize==0)
  {
     //initialize currPosInfo structure
     for (size_t i=0; i<numRmfs; i++)
     {
	// rmfNum and rmfSize will remain constant after initialization
	currPosInfo[i].rmfNum = i;
	currPosInfo[i].rmfSize = m_rmfData[i]->convArraySize();
	currPosInfo[i].pos = 0;
	currPosInfo[i].posValue = m_rmfData[i]->positions(0);
     }
     // initialize heap with the matrix position values of the first elements 
     // in each of the convArrays
     first = currPosInfo.begin();
     last = currPosInfo.end();
     make_heap(first,last,std::greater<positionInfo>());
     // make sure prevElement != currPosInfo[0].posValue initially
     ResponseMatrix::RMFLONG prevElement = currPosInfo[0].posValue+1;

     while (remainingRmfs)
     {
	pop_heap(first,last,std::greater<positionInfo>());
	positionInfo &back = currPosInfo.back();
	// In case of duplicate position values, count elements only once.
	if (back.posValue != prevElement)
	{
	   posUnion.push_back(back.posValue);
	   prevElement = back.posValue;
	}
	if (++back.pos < back.rmfSize)
	{
           back.posValue = m_rmfData[back.rmfNum]->positions(back.pos);
	   push_heap(first,last,std::greater<positionInfo>());
	}
	else
	{
           remainingRmfs--;
	   last--;
	   currPosInfo.pop_back();
	}
     }
     m_unionSize = posUnion.size();
     // This assumes that all of the RMFs that are combined have the
     // same number of energies in their expanded matrices.
     size_t nE = m_rmfData[0]->energyLow().size();

     // From filled posUnion array, store in condensed format, and fill
     // the m_noticedElements and m_noticedElemPos arrays.
     ResponseMatrix::RMFLONG lnE = static_cast<ResponseMatrix::RMFLONG>(nE);
      if (m_unionSize)
      {
	 m_channelForRun.push_back(static_cast<int>(posUnion[0]/lnE)); //determine row
	 m_energyStart.push_back(static_cast<int>(posUnion[0]%lnE));  //determine col
	 m_energyRunLengths.push_back(1);
      }
      for (size_t i=1; i<m_unionSize; i++)
      {
	 ResponseMatrix::RMFLONG pos_i = posUnion[i];
	 if (pos_i-posUnion[i-1]!=1 || pos_i%lnE==0) //start of new group
	 {
            m_channelForRun.push_back(static_cast<int>(pos_i/lnE)); 
            m_energyStart.push_back(static_cast<int>(pos_i%lnE));  
            m_energyRunLengths.push_back(1);
	 }
	 else
            ++m_energyRunLengths.back();
      }
   }


   size_t k=0;
   size_t nChanRowGroups = m_channelForRun.size();
   size_t offset = m_source->startChan() - m_source->firstChan();
   m_noticedElements.clear();
   m_noticedElemPos.clear();
   m_noticedElements.reserve(nChanRowGroups);
   m_noticedElemPos.reserve(nChanRowGroups);
   for (size_t i=0; i<nChanRowGroups; i++)
   {
      if (m_source->noticedChannels(m_channelForRun[i]+ offset))
      {
         m_noticedElements.push_back(i);
	 m_noticedElemPos.push_back(k);
      }
      k += m_energyRunLengths[i];
   }
}

const RealArray& MultiResponse::eboundsMin () const
{
  return m_rmfData[0]->eboundsMin();
}

const RealArray& MultiResponse::eboundsMax () const
{
  return m_rmfData[0]->eboundsMax();
}

void MultiResponse::convolve (const RealArray& flux, const RealArray& fluxErr, RealArray& foldFlux, RealArray& foldFluxErr) const
{
   XSutility::auto_array_ptr<Real> pCvUnion(new Real[m_unionSize]);
   Real* cvUnion = pCvUnion.get();
   combineRMF(cvUnion);	   
   size_t firstEng_i, numbInGroup_i, chanForRun_i, firstEng_ij;
   size_t noticedElements_i, noticedElementsPos_i;
   Real foldFlux_i, foldFluxErr_i, cnv_j;

   // aflux is created to be able to use copy algorithm on the valarray,
   // since it doesn't compile on a const valarray - this is probably
   // faster than a loop which calls valarray operator[]
   RealArray aflux(flux);
   XSutility::auto_array_ptr<Real> pFlux (new Real[aflux.size()]);
   Real* Flux (pFlux.get());
   std::copy(&aflux[0],&aflux[0]+aflux.size(),&Flux[0]);

   const size_t N (numChannels());   
   XSutility::auto_array_ptr<Real> pFoldFlux(new Real[numChannels()]);
   Real* FoldFlux (pFoldFlux.get());
   memset(FoldFlux,0,N*sizeof(Real));

   size_t noticedSz = m_noticedElements.size();

   if (fluxErr.size())
   {	
   	RealArray fluxVariance(fluxErr*fluxErr);
        XSutility::auto_array_ptr<Real> pFluxVariance (new Real[fluxVariance.size()]);
        Real* FluxVariance (pFluxVariance.get());
        std::copy(&fluxVariance[0],&fluxVariance[0]+fluxVariance.size(),&FluxVariance[0]);

        XSutility::auto_array_ptr<Real> pFoldFluxErr(new Real[numChannels()]);
        Real* FoldFluxErr (pFoldFluxErr.get());
        memset(FoldFluxErr,0,N*sizeof(Real));

	for (size_t i=0; i<noticedSz; ++i)
	{
	   noticedElements_i = m_noticedElements[i];
	   noticedElementsPos_i = m_noticedElemPos[i];
	   numbInGroup_i = m_energyRunLengths[noticedElements_i];
	   chanForRun_i = m_channelForRun[noticedElements_i]; 
	   firstEng_i = m_energyStart[noticedElements_i]; 
	   foldFlux_i = FoldFlux[chanForRun_i];
	   foldFluxErr_i = FoldFluxErr[chanForRun_i];
	   for (size_t j=0; j<numbInGroup_i; ++j)
	   {
	      cnv_j = cvUnion[noticedElementsPos_i+j];
	      firstEng_ij = firstEng_i+j;
	      foldFlux_i += Flux[firstEng_ij]*cnv_j;
	      foldFluxErr_i += FluxVariance[firstEng_ij]*cnv_j;
	   }
	   FoldFlux[chanForRun_i] = foldFlux_i;
	   FoldFluxErr[chanForRun_i] = foldFluxErr_i;
	} 
        foldFlux    = RealArray(FoldFlux,N);     
        foldFluxErr = RealArray(FoldFluxErr,N);     
   }
   else
   {
	for (size_t i=0; i<noticedSz; ++i)
	{
	   noticedElements_i = m_noticedElements[i];
	   noticedElementsPos_i = m_noticedElemPos[i];
	   numbInGroup_i = m_energyRunLengths[noticedElements_i];
	   chanForRun_i = m_channelForRun[noticedElements_i];
	   firstEng_i = m_energyStart[noticedElements_i]; 
	   foldFlux_i = FoldFlux[chanForRun_i];
	   for (size_t j=0; j<numbInGroup_i; ++j)
	      foldFlux_i +=
	      Flux[firstEng_i+j]*cvUnion[noticedElementsPos_i+j];
	   FoldFlux[chanForRun_i] = foldFlux_i;
	}
        foldFlux    = RealArray(FoldFlux,N);     
   }   
}

RealArray MultiResponse::sensitivity (const SpectralData* data)
{
  XSutility::auto_array_ptr<Real> pConvArray(new Real[m_unionSize]);
  Real* convArray = pConvArray.get();
  combineRMF(convArray);
  size_t nz = m_noticedElements.size();
  RealArray sens(0.,numEnergies());
  Real respElt(0);
  size_t ne(0), ch(0), first(0), last(0), jpos(0);

  for (size_t j = 0; j < nz; ++j)
  {
        ne = m_noticedElements[j];
        ch = m_channelForRun[ne];
        first =  m_energyStart[ne];
        last  = first + m_energyRunLengths[ne];
        jpos  = m_noticedElemPos[j];
        Real var = data->variance(ch);
        if (data->background())
           var += data->background()->variance()[ch];
        for (size_t k = first; k < last; ++k )
        {
                respElt = convArray[jpos];
                sens[k] += respElt*respElt/var;
                ++jpos;
        } 
  }



  return sens;
}

void MultiResponse::operator * (SumComponent& source) const
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

void MultiResponse::shiftEffectiveArea (const RealArray& newEnergies)
{
  const Real FUZZY = 1.0e-6;
  // Lazy initialization of m_savedEffectiveArea array. If gain
  // command is never called, this need not be done.
  if (!m_savedEffectiveArea.size())
  {
     size_t sz = m_effectiveArea.size();
     m_savedEffectiveArea.resize(sz);
     for (size_t i=0; i<sz; ++i)
     {
        m_savedEffectiveArea[i].resize(m_effectiveArea[i].size());
        m_savedEffectiveArea[i] = m_effectiveArea[i];
     }
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
  size_t nRmf = m_effectiveArea.size();
  for (size_t i=0; i<nRmf; ++i)
  {
     // this function modifies m_effectiveArea
     Numerics::Rebin::gainRebin(m_savedEffectiveArea[i], startBin, endBin, 
                   startWeight, endWeight, m_effectiveArea[i]);
  }
}

void MultiResponse::restoreEffectiveArea ()
{
  size_t nRmf = m_savedEffectiveArea.size();
  if (nRmf)
  {
     if (m_effectiveArea.size() == nRmf)
     {
        for (size_t i=0; i<nRmf; ++i)
        {
           if (m_effectiveArea[i].size() == m_savedEffectiveArea[i].size())
           {
              m_effectiveArea[i] = m_savedEffectiveArea[i];
           }
           else
           {
              throw RedAlert("Size mismatch between old and new effectiveArea[i] array.");
           }
        }
     }
     else
     {
        throw RedAlert("nRMF size mismatch between old and new effectiveArea container.");
     }
  }
}

void MultiResponse::calcEffAreaPerChan (RealArray& effArea)
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
      throw RedAlert("Channel size mismatch in MultiResponse::calcEffAreaPerChan");
   }
   chanWidths = chanEmax - chanEmin;
   effArea /= chanWidths;
}

// Additional Declarations
