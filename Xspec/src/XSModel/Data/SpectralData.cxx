//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// UserDummyResponse
#include <XSModel/Data/Detector/UserDummyResponse.h>
// Background
#include <XSModel/Data/BackCorr/Background.h>
// Response
#include <XSModel/Data/Detector/Response.h>
// XSparse
#include <XSUtil/Parse/XSparse.h>
// SpectralData
#include <XSModel/Data/SpectralData.h>
// XSutility
#include <XSUtil/Utils/XSutility.h>

#include <XSModel/Data/DataSet.h>
#include <XSModel/Data/DataUtility.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/GlobalContainer/ResponseContainer.h>
#include <XSModel/GlobalContainer/Weight.h>
#include <XSModel/Model/Model.h>
#include <XSUtil/Utils/XSstream.h>
#include <XSUtil/Numerics/Numerics.h>
#include <XSstreams.h>
#include <XSsymbol.h>
#include <XSContainer.h>
#include <algorithm>
#include <iomanip>


// Class SpectralData::NoEnergyRange 

SpectralData::NoEnergyRange::NoEnergyRange (int spectrumNumber)
  : YellowAlert(" no energy defined range for spectrum ")
{
  tcerr << spectrumNumber << '\n';
}


// Class SpectralData::FluxCalc 

// Additional Declarations

// Class SpectralData::SpectralMemento 

// Additional Declarations

// Class SpectralData 
int SpectralData::s_chanEngWave = 0;
Real SpectralData::s_engUnits = 1.0;
Real SpectralData::s_waveUnits = 1.0;

SpectralData::SpectralData (DataSet* parent, size_t channels, size_t rowNumber, size_t sourceNums)
      : m_exposureTime(0.),
        m_correctionScale(0.),
        m_channels(channels),
        m_startChan(0),
        m_endChan(0),
        m_telescope(""),
        m_instrument(""),
        m_channelType(string("PHA")),
        m_backgroundFile(""),
        m_correctionFile(""),
        m_spectrumNumber(1),
        m_plotGroup(1),
        m_netFlux(0.),
        m_totalFlux(0.),
        m_netVariance(0.),       
        m_gqString(),
        m_firstChan(0),
	m_rowNumber(rowNumber),
        m_spectrumIsZeroed(true),
        m_lastEqWidthCalc(.0),
	m_backgroundChanged(false),
	m_correctionChanged(false),
	m_parent(parent),
        m_memento(0),
        m_isPoisson(true),
        m_effectiveAreas(),
        m_statName("chi"),
        m_testStatName("chi"),
        m_background(0),
        m_correction(0),
        m_rawVariance(0.0,channels),
        m_noticedChannels(),
        m_variance(0.0,channels),
        m_spectrum(0.0,channels),
        m_detector(std::vector<Response*>(sourceNums,static_cast<Response*>(0))),
        m_indirectNotice(),
        m_areaScale(),
        m_backgroundScale(),
        m_quality(),
        m_qualityInfo(),
        m_groupingInfo(),
	m_responseHooks(std::vector<Response*>(sourceNums,
		static_cast<Response*>(0))),
	m_responseChanged(std::vector<bool>(sourceNums,false)),
        m_xflt(),
        m_arfChanged(std::vector<bool>(sourceNums,false)),
        m_lastModelFluxCalc(),
        m_lastModelLuminCalc()
{
}


SpectralData::~SpectralData()
{
  destroy();  // nothrow.
}


void SpectralData::destroy () throw ()
{
  // although this apparently implies a recursive call, it won't
  // happen because for Background/Correction objects, the Background/Correction
  // pointer is initialized to null and never allocated.
  // the map m_detector of responses is not an owning pointer.
  // all other arrays are standard library classes which will self-destruct. 
  delete m_background;
  m_background = 0;

  delete m_correction;
  m_correction = 0;

  delete m_memento;
  m_memento = 0;

  for (size_t i=0; i<m_detector.size(); ++i)
  {
     removeResponses(i+1);
  }
}

void SpectralData::closeAncillaryFiles ()
{
  if (m_background) m_background->closeSourceFiles();
  if (m_correction) m_correction->closeSourceFiles();  
}

void SpectralData::renumber (size_t newIndex)
{
  m_spectrumNumber = newIndex;
  if (m_background != 0)
  {
          m_background->renumber(newIndex);
  }
  if (m_correction != 0)
  {
          m_correction->renumber(newIndex);
  }

  for (std::vector<Response*>::iterator iResponse = m_detector.begin();
        iResponse != m_detector.end(); ++iResponse)
  {
                if ( *iResponse != 0 ) (*iResponse)->renumber(newIndex);         
  }

  for (size_t i=0; i<m_responseHooks.size(); ++i)
  {
     if (m_responseHooks[i])
     {
        m_responseHooks[i]->renumber(newIndex);
     }
  }
}

void SpectralData::removeResponses (size_t index) throw ()
{
  // This will dispose of any response in the specified detector slot,
  // genuine or dummy, as well as any response on the corresponding
  // response hook.

  // removeByToken mustn't throw since this is called by SpectralData's       
  // dtor.  It NO LONGER calls delete on Reponse obj, which now must
  // be done here.  By having SpectralData own its responses, it 
  // should make clean up easier for SpectralData objs in TrashCan,
  // which have responses NOT in global ResponseContainer.

  // index is 1-based
  if (index && index <= m_detector.size())
  {
     // If this is not a dummy response, the following call does nothing.
     // If it IS a dummy response AND there's no genuine response hanging
     // on a hook, the m_detector slot will be rendered null and there'll
     // be no reason to continue further.
     removeUserDummy(index-1);
     if (m_detector[index-1])
     {
        // removeByToken requires a 1-based index.
        XSContainer::responses->removeByToken(m_detector, index);
        delete m_detector[index-1];
        m_detector[index-1] = 0;
     }
  }
}

Response* SpectralData::detector (size_t index) const
{
  if (index < m_detector.size()) return m_detector[index];
  else return 0;  
}

void SpectralData::attachDetector (Response* value, size_t index)
{
  if (index < m_detector.size()) m_detector[index] = value;
}

void SpectralData::setChannels (bool value, IntegerArray& range)
{
    using namespace std;     
    size_t N(range.size()); 

    // N == 0 means set all channel notice values to "value". This is
    // expected to be used in implementing 
    //std::ostream_iterator<int> x(std::cerr,"\t");
    //std::cerr << " range " << range.size() << '\n' ;
    //std::copy(range.begin(),range.end(),x);
    //std::cerr  << '\n' ;

    // notice all
    // (ignore all doesn't make much sense, but would work).

    int number = 0;

    if (N > 0)
    {
	IntegerArray rrange;
	//this function should only accept a single range, or
	//one in which the range has already been expanded out.
	//This condition should make sure other code that 
	//calls this function doesn't break.
	if(N <= 2)
	{
	    //subtract 1 because the user is thinking '1-based' arrays
	    //also check for a wild card of '**' having been entered
	    //code was moved from here that handled the '*' case and
	    //put into the handler for xsIgnore/xsNotice. SpectralData
	    //didn't need to worry about that.
	    range[0] = (range[0] == -2 ? 1 : range[0]);
	    range[1] = (range[1] == -2 ? m_channels : range[1]);

	    rrange = XSparse::expandRange(range);
	}
	else
	    rrange = range;

	int __size = rrange.size();
	// the range might exceed the bounds of the channel array for
	// a particular dataset, so catch that condition here.
	if (__size > 0) 
	    while (rrange[__size - 1] > static_cast<int>(m_channels)) --__size;

	int offset = m_startChan - m_firstChan;

	for(int j = 0; j < __size; ++j, ++number)
	    m_noticedChannels[rrange[j] + offset - 1] = value;
    } 
    /*
    else
    {
        // 'all' mode
        fill(m_noticedChannels.begin() + m_startChan - m_firstChan - 1,
             m_noticedChannels.begin() + m_startChan - m_firstChan -1 +  m_channels,value);
    }
    */

    std::ios_base::fmtflags save(tcout.flags());
    if (number > 0)
    {
	tcout.setf(ios::right);

	string which(value ? "noticed" : "ignored");

	tcout << setw(6) << number
	      << " channels (" << XSparse::ArrayToRange(range) << ") "
	      << which << " in spectrum # " << setw(5) << m_spectrumNumber << std::endl;
    }
    else
	tcout << "      No channels ignored (no channels in specified range)"
              << std::endl;

    tcout.flags(save);

    computeTotals();
}

std::vector<RealArray> SpectralData::sensitivity () const
{
  std::vector<RealArray> sensitivities;
  const size_t nDets = m_detector.size();
  for (size_t iDet=0; iDet<nDets; ++iDet)
  {
     if ( m_detector[iDet])
     {
        sensitivities.push_back(RealArray());
        RealArray& sens = sensitivities.back();
           sens.resize(m_detector[iDet]->numEnergies());
           sens = m_detector[iDet]->sensitivity(this);
     }
  }  
  return sensitivities;
}

bool SpectralData::setBadChannels ()
{
  using namespace std;      
  // set the bad channels' notice flag  to false
  size_t numberBad(0);
  bool retVal(true);

  if (m_quality.size())
  {
          for (size_t j = 0; j <  m_channels; ++j)
          {
                if (m_quality[j])
                {
                        ++numberBad;
                        m_noticedChannels[j + m_startChan - m_firstChan] = false;
                }
          }      
          computeTotals();
          tcout << "\nignore:" << setw(6) << setiosflags(ios::right)  
                << numberBad   << " channels ignored from  source number " 
                << m_spectrumNumber;
  }
  else 
  {
          tcout << "\nignore: no quality array present: spectrum " << m_spectrumNumber;
          retVal = false;
  }

  return retVal;
}

void SpectralData::setChannels (bool value, std::pair<Real,Real>& range)
{
    const Response*  eb = 0;
    // Just find the first detector with a response:
    std::vector<Response*>::const_iterator itDet = m_detector.begin();
    while (!eb && itDet != m_detector.end())
    {
       if (*itDet)
          eb = *itDet;
       ++itDet;
    }
    static const Real FUZZ = 1.e-12;

    if (!eb) throw NoEnergyRange(m_spectrumNumber);

    const RealArray& lowerEbound = eb->lowerBoundNominalEnergy();
    const RealArray& upperEbound = eb->upperBoundNominalEnergy();

    //Invariant: whether user is dealing in wavelengths or energies,
    // and whether ebins are in ascending or descending order,
    // these begin and end variables are always in energy units
    // and begin should always be <= end.
    Real begin(0), end(0);
    bool isAscending = (lowerEbound[0] < lowerEbound[m_channels-1]);
    if (s_chanEngWave != 2)
    {  
       // process wild cards in first argument.
       if (range.first == -2)
          range.first = isAscending ? lowerEbound[0] - FUZZ : 
                                lowerEbound[m_channels-1] - FUZZ;
       else // user-entered val is not necessarily in keV
          range.first /= s_engUnits;


       if (range.second == -2)
          range.second = isAscending ? upperEbound[m_channels-1] + FUZZ :
                                upperEbound[0] + FUZZ;
       else
          range.second /= s_engUnits;

       begin = range.first;
       end   = range.second;
    }
    else
    {
       // process wild cards in second argument.
       if (range.first == -2)
          // give 'em the smallest wavelength, or largest energy.
          end = isAscending ? upperEbound[m_channels-1] + FUZZ : 
                        upperEbound[0] + FUZZ;
       else // User-entered wavelength is not necessarily in Angstroms
          end = Numerics::KEVTOA*s_waveUnits/std::max(range.first,FUZZ);
       if(range.second == -2)
          // give largest wavelength, or smallest energy.
	  begin = isAscending ? lowerEbound[0] - FUZZ : 
                        lowerEbound[m_channels-1] - FUZZ; 
       else  
          begin = Numerics::KEVTOA*s_waveUnits/std::max(range.second,FUZZ);
    }        
    if (begin > end)   
    {
       // As a result of FUZZ above, if one of the ranges (doesn't
       // matter which) was a -2, then this swap will produce a range
       // entirely out of bounds below, which is what we want.
       Real tmp = begin;
       begin = end;
       end = tmp;
    }

    int firstChan=0, lastChan=0;
    IntegerArray channelSet;          
    if (isAscending)
    {
       // If begin,end are entirely out of range, do nothing and pass
       // an empty channelSet to second setChannels function.
       if (end > lowerEbound[0] && begin <= upperEbound[m_channels-1])
       {
          // ebounds array increases in energy w/channel number.
          // Find bin that encloses 'begin'.  That will be first
          // ignored 0-based channel.
          size_t j=0;
          while (j < m_channels && upperEbound[j] < begin )  ++j;
          firstChan = j;

          // Find bin that encloses 'end'.  That will be last 
          // ignored channel, which may be the same as firstChan.
          while ( j < m_channels && upperEbound[j] < end )  ++j;
          if (j == m_channels) --j;
          lastChan = j;
          size_t M (lastChan - firstChan + 1);
          channelSet.resize(M);
          // the "+1" allows for the fact that the user sees channel numbers as
          // 1-based. This +1 is subtracted before the noticed channels array is set.
          for (size_t k = 0; k < M; ++k) channelSet[k] = firstChan + k + 1;
       }
    }
    else
    {
       if (begin <= upperEbound[0] && end > lowerEbound[m_channels-1])
       {
          // ebounds array decreases in energy w/channel number.
          int j = static_cast<int>(m_channels-1);
          while ( j >=0 && upperEbound[j] < begin ) --j;
          firstChan = j;

          while ( j >=0 && upperEbound[j] < end) --j;
          if (j == -1) ++j;
          lastChan = j;
          size_t M(firstChan -  lastChan + 1);
          channelSet.resize(M);
          // it probably doesn't matter which order these numbers are stored in the 
          // channelSet array, but it's probably more efficient to do it in increasing
          // order.
          // the "+1" allows for the fact that the user sees channel numbers as
          // 1-based. This +1 is subtracted before the noticed channels array is set.
          for (size_t k = 0; k < M; ++k) channelSet[k] = lastChan + k + 1;
       }
    }


    setChannels(value,channelSet);  
}

bool SpectralData::responseLoaded (size_t index)
{
  return (m_detector[index] != 0);
}

void SpectralData::setBackgroundSpectrum (const RealArray& value, bool scaleCounts)
{
  if (m_background) 
  {
     RealArray array(value);
     if (scaleCounts)
     {
        const SpectralData* bckSpec = background()->data();       
        RealArray factor (bckSpec->exposureTime()*bckSpec->areaScale());
        factor *= bckSpec->backgroundScale();
        factor /= m_backgroundScale;
        array  /= factor; 
     }
     m_background->setSpectrum(array);
  }
}

void SpectralData::debugPrint (XSstream& s, const string& type) const
{
        using namespace std;
        ios_base::fmtflags save(s.flags());
        s << xsverbose(25) << type << ": Channel  \tVariance ";
        bool qual = m_quality.size();
        if (qual) s << "\tQuality\n" ;
        else s << '\n';
        size_t N = m_spectrum.size();
        int offset = m_startChan - m_firstChan;
        Real sum (0);
        if (qual)
        {
                for (size_t l = 0; l < N; ++l) 
                {        
                      if (m_noticedChannels[l + offset])
                      {
                                s << setw(4) << l + 1 << setw(12) << m_spectrum[l] 
                                        << setw(12)  << m_rawVariance[l] << setw(12) 
                                        << m_quality[l] << '\n'; 
                                sum += m_spectrum[l];

                      }   
                }
        }
        else
        {
                for (size_t l = 0; l < N; ++l) 
                {        
                      if (m_noticedChannels[l + offset])
                      {
                          s << setw(4) << l + 1 << setw(12) << m_spectrum[l] 
                                        << setw(12)  << m_rawVariance[l]  << '\n';
                      }            
                }
        }
        s << " Sum: " << type << " " << sum << '\n';
        s << flush;
        s.flags(save);
        s << xsverbose();  
}

void SpectralData::prepareForFit ()
{

  // This rather awkward way of setting m_indirectNotice is a result
  // of compromise between attempting to keep buildIndirectNotice
  // a const function (for report functions).
  std::valarray<size_t> tmpIndirect;
  buildIndirectNotice(tmpIndirect);
  m_indirectNotice.resize(tmpIndirect.size());
  m_indirectNotice = tmpIndirect;

  for (size_t i = 0; i < m_detector.size(); i++)  
  {
     if (m_detector[i] != NULL)
     {
        m_detector[i]->prepareForFit();
     }
  }
}

int SpectralData::energyChannel (const Real& energy, int& channel) const
{
  const Response* rsp = 0;
  // Just find the first detector with a response:
  std::vector<Response*>::const_iterator itDet = m_detector.begin();
  while (!rsp && itDet != m_detector.end())
  {
     if (*itDet)
        rsp = *itDet;
     ++itDet;
  }

  if (rsp)
  {
        bool increasing = (rsp->lowerBoundNominalEnergy()[0] < rsp->lowerBoundNominalEnergy()[1]);
        const std::valarray<size_t>& IN = indirectNotice();
        // find returns the first energy less than the input energy.
        RealArray energyArray(0.,IN.size());
        if (increasing)
        {
                energyArray = rsp->lowerBoundNominalEnergy()[IN];
        }
        else
        {
                energyArray = rsp->upperBoundNominalEnergy()[IN];      
        }         
        XSutility::find( energyArray,energy,channel) ;
        if (channel < 0)
        {
           channel = 0;
        }
        else if (channel >= (int)energyArray.size())
        {
           channel = energyArray.size()-1;
        }
        return channel;
  }     
  else 
  {
        throw NoEnergyRange(m_spectrumNumber);       
  }      
}

Real SpectralData::channelEnergy (size_t channel)
{
  const Response* rsp = 0;
  // Just find the first detector with a response:
  std::vector<Response*>::const_iterator itDet = m_detector.begin();
  while (!rsp && itDet != m_detector.end())
  {
     if (*itDet)
        rsp = *itDet;
     ++itDet;
  }
  if (rsp)
  {
        const std::valarray<size_t>& IN = indirectNotice();
        RealArray eMin(0.,IN.size());
        RealArray eMax(0.,IN.size());
        eMin = rsp->eboundsMin()[IN];
        eMax = rsp->eboundsMax()[IN];
        return (eMin[channel]+eMax[channel])/2.0;
  }     
  else 
  {
        throw NoEnergyRange(m_spectrumNumber);       
  }      
}

void SpectralData::simulateSpectrum(bool useCountingStat)
{
    // Assume spectrum already contains a simulated rate/channel/unit area
    // from an M*R calculation, and scale it to a count spectrum with 
    // background re-added and corrections removed.  

    const RealArray atime = m_exposureTime*m_areaScale;
    RealArray var = m_rawVariance;
    const size_t nChans = m_spectrum.size();
    if (m_correction)
    {
       m_spectrum += m_correctionScale*m_correction->spectrum();
    }
    
    // Add in the background if it exists.
    if (m_background)
    {
       m_spectrum += m_background->data()->spectrum();
    }
    
    // SECTION 1:
    // First do the source - if randomize option is set then use either
    // Poisson or Normal statistics depending on what it set in the current
    // data file. If randomize option is not set then ensure m_isPoisson is
    // false so that the output is written as RATE in order to avoid truncation
    // errors   
    if (useCountingStat)
    {
       if (m_isPoisson)  
       {
          m_spectrum *= atime;
          Numerics::PoissonRand(m_spectrum);
          m_spectrum /= atime;
       }
       else
       {
          RealArray gaussNum(0.0,nChans);
          Numerics::GaussRand(gaussNum);
          m_spectrum += gaussNum*sqrt(var);
       }
    }
    else
    {
      m_isPoisson = false;
    }

    if (m_isPoisson)
    {
       // var remains the same for non-Poisson.
       var = m_spectrum/atime;
    }
    m_rawVariance = var;
    
               
     // SECTION 2:
     // Do the same to background, if it exists.
    if ( m_background )
    {
        // NOTE: The simualted/fake background spectrum at this stage includes 
        // the ratio multiplier SpecBSCALE/BckBSCALE, and bkVar
        // contains the ratio squared.  This must be accounted for
        // when converting to counts for Poisson randomization,
        // and will be removed during the write to file (for fakeit).
       SpectralData* bkg = m_background->data();
       RealArray bkgSpectrum(m_background->spectrum());
       RealArray bkgVar(bkg->rawVariance());
       RealArray btimeRatio = bkg->exposureTime()*bkg->areaScale();
       btimeRatio *= bkg->backgroundScale();
       btimeRatio /= m_backgroundScale;
       
       if (useCountingStat)
       {
          if (bkg->isPoisson())
          {
             bkgSpectrum *= btimeRatio;
             Numerics::PoissonRand(bkgSpectrum);
             bkgSpectrum /= btimeRatio;
          }
          else
          {
             RealArray gaussNum(0.0,bkgSpectrum.size());
             Numerics::GaussRand(gaussNum);
             bkgSpectrum += gaussNum*sqrt(bkg->rawVariance());
          }
       }
       else
       {
	 bkg->isPoisson(false);
       }

       if (bkg->isPoisson())
       {
          bkgVar = bkgSpectrum/btimeRatio;
       }
       m_background->setRawVariance(bkgVar);
       m_background->setSpectrum(bkgSpectrum);
       DataUtility::statWeight()(*bkg,btimeRatio);
    }    
    DataUtility::statWeight()(*this,atime); 
     
}

void SpectralData::setBackgroundVariance (const RealArray& value)
{
  if ( m_background ) m_background->setRawVariance(value);
}

void SpectralData::setSpectrum (const RealArray& value, bool scaleCounts)
{
  m_spectrum.resize(value.size());
  RealArray array(value);
  if ( scaleCounts)
  {
         array /= m_exposureTime;
         array /= m_areaScale; 
  }
  m_spectrum = array;  
  m_spectrumIsZeroed = false;  
}

void SpectralData::computeTotals ()
{
  bool backgroundPresent = (m_background != 0);
  bool correctionPresent = (m_correction != 0);

  Real totalFlux = 0.;
  Real netVariance = 0.;
  Real netFlux = 0.;

  int offset = m_startChan - m_firstChan;
  for (size_t j = 0; j < m_channels; ++j)
  {
	if (m_noticedChannels[j + offset]) 
	{
                Real area (m_areaScale[j]);
		Real area2 = area*area;
		totalFlux   += area*m_spectrum[j];
                netFlux     += area*m_spectrum[j];
		netVariance += area2*m_variance[j];
                if (backgroundPresent)
		{
		   netFlux -= area*m_background->spectrum()[j];
		   netVariance += area2*m_background->variance()[j];
		}
                if (correctionPresent)
                {
                   Real corTerm = area*m_correctionScale*m_correction->spectrum()[j];
                   netFlux -= corTerm;
                   totalFlux -= corTerm;
                }
	}
  }

  netVariance = sqrt(netVariance);

  m_totalFlux = totalFlux;
  m_netFlux = netFlux;
  m_netVariance = netVariance;  
}

bool SpectralData::energiesEqual (SpectralData* right) const
{
  bool result(false);

  const Response* firstResp = 0;
  std::vector<Response*>::const_iterator itResp = detector().begin();
  while (itResp != detector().end() && !firstResp)
  {
     if (*itResp)
        firstResp = *itResp;
     ++itResp;
  }
  const Response* rFirstResp = 0;
  itResp = right->detector().begin();
  while (itResp != right->detector().end() && !rFirstResp)
  {
     if (*itResp)
        rFirstResp = *itResp;
     ++itResp;
  }

  if ( firstResp && rFirstResp )
  {

          const RealArray& l = firstResp->eboundsMin();      
          const RealArray& r = rFirstResp->eboundsMin();

	  if (l.size() != r.size())  return false;
          std::valarray<bool> c ( l == r );

#ifndef STD_COUNT_DEFECT
          int nFalse(std::count(&c[0], &c[0]+c.size(), false));  
#else
          int nFalse = 0;
          std::count(&c[0], &c[0]+c.size(), false, nFalse);  
#endif    
          result = !nFalse;
  }
  else if ( !firstResp && !rFirstResp)
  {
        result =  (m_channels == right->channels());
  }
  return result;
}

void SpectralData::attachUserDummy (UserDummyResponse* dummyResponse, size_t index)
{

  // If the dummyResponse was created by something other than its
  // copy constructor, source will already be properly set.
  dummyResponse->source(this);
  dummyResponse->spectrumNumber(m_spectrumNumber);

  if (dynamic_cast<UserDummyResponse*>(m_detector[index]))
  {
     // A dummyResponse already exists for this source number.  The global
     // ResponseContainer will swap it with the new one and then we'll
     // delete it here.
     Response* doomed = m_detector[index];
     XSContainer::responses->swapResponses(dummyResponse, doomed);
     m_detector[index] = dummyResponse;
     delete doomed;
  }
  else
  {
     // No dummyResponse is associated with this source.  Is there a 
     // genuine response associated with it?
     if (m_detector[index])
     {
	// Remove (but do not delete) the genuine response from the container
	// and place it in the local storage container.  Place the dummy response
	// in the global container.  
	Response* genuine = m_detector[index];
	if (!XSContainer::responses->swapResponses(dummyResponse, genuine))
	{
	   // The genuine response was not found in the container.  Something
	   // is SERIOUSLY wrong.
	   throw RedAlert("Memory address for response is corrupted.");
	}
	m_detector[index] = dummyResponse;
	m_responseHooks[index] = genuine;	
     }
     else
     {
        // No response of any kind associated with this source.  The
	// dummy response will then be inserted into the response container.
	m_detector[index] = dummyResponse;
	XSContainer::responses->addToList(USR_DUMMY_RSP, dummyResponse);          
     }
  }
}

bool SpectralData::removeUserDummy (size_t index)
{
  bool isChanged = false;
  if (m_detector[index] && m_detector[index]->toUserDummyResponse())
  {
     Response* doomed = m_detector[index];
     Response* genuine = m_responseHooks[index];
     if (genuine)
     {
        XSContainer::responses->swapResponses(genuine, doomed);
	m_detector[index] = genuine;
	m_responseHooks[index] = static_cast<Response*>(0);
	delete doomed;
     }
     else
     {
        XSContainer::responses->removeByToken(m_detector, index+1);
        delete doomed;
        m_detector[index] = 0;
     }
     isChanged = true;
  }
  return isChanged;
}

bool SpectralData::removeAllDummies ()
{
  bool isChanged = false;
  for (size_t i=0; i<m_detector.size(); ++i)
  {
     if (removeUserDummy(i))
     {
	isChanged = true;
     }
  }

  return isChanged;
}

void SpectralData::setNumberOfChannels (size_t nChans)
{
  // This function is to be used only when the number of channels isn't known
  // at the time of construction.  
  m_channels = nChans;
  m_rawVariance.resize(nChans, 0.0);
  m_variance.resize(nChans, 0.0);
  m_spectrum.resize(nChans, 0.0);
}

void SpectralData::initializeFake (const SpectralData* origSpectrum)
{
   m_groupingInfo = origSpectrum->groupingInfo();
   m_qualityInfo = origSpectrum->qualityInfo();
   DataUtility::encodeGQ(m_groupingInfo, m_qualityInfo, m_gqString);     
   m_telescope = origSpectrum->telescope();
   m_instrument = origSpectrum->instrument();
   m_firstChan = origSpectrum->firstChan();
   m_startChan = origSpectrum->startChan();
   m_endChan = origSpectrum->endChan();
   m_areaScale.resize(m_channels);     
   m_areaScale = origSpectrum->areaScale();
   m_backgroundScale.resize(m_channels);
   m_backgroundScale = origSpectrum->backgroundScale();
   m_isPoisson = origSpectrum->isPoisson();
   if (!m_isPoisson)
   {
      // Variance arrays already sized to m_channels in ctor.
      m_rawVariance = origSpectrum->rawVariance();
      // We want this to be independent of weighting mode of
      // original spectrum.
      m_variance = origSpectrum->rawVariance();
   }

   m_channelType = origSpectrum->channelType();
   m_noticedChannels.resize(origSpectrum->noticedChannels().size(), false);
   for (size_t i = m_startChan-m_firstChan; i <= m_endChan-m_firstChan; ++i)
   {
      m_noticedChannels[i] = true;
   }
   m_statName = origSpectrum->statName();
   m_testStatName = origSpectrum->testStatName();
   m_xflt = origSpectrum->xflt();
}

bool SpectralData::isDummyrspMode2 (size_t iDet) const
{
  bool status = false;
  if (iDet < m_detector.size())
  {
     if (const UserDummyResponse *udr = 
                dynamic_cast<UserDummyResponse*>(m_detector[iDet]))
     {
        status = !udr->usingChannels();
     }
  }  
  return status;
}

void SpectralData::reportNoticed () const
{
  std::valarray<size_t> indirectNotice;
  buildIndirectNotice(indirectNotice);
  // buildIndirectNotice builds an array with 0-based channel numbers.
  // Report should produce 1-based channels.
  size_t n = indirectNotice.size();
  // Using a for loop rather than just valarray += 1 due to some
  // unexplained compiler issue on Solaris.
  for (size_t i=0; i<n; ++i)
  {
     indirectNotice[i] += 1;
  }
  string range(XSparse::ArrayToRange(indirectNotice));
  tcout << "  Noticed Channels:  " << range << std::endl;
}

void SpectralData::buildIndirectNotice (std::valarray<size_t>& indirectNotice) const
{
  int offset = m_startChan - m_firstChan;
#ifndef STD_COUNT_DEFECT
  size_t noted(std::count(m_noticedChannels.begin() + offset,
             m_noticedChannels.begin() + offset + m_channels, true));
#else
  size_t noted(0); 
  std::count(m_noticedChannels.begin() + offset,
             m_noticedChannels.begin() + offset + m_channels, true, noted);
#endif
  indirectNotice.resize(noted);

  size_t ii = 0;
  for (size_t j = 0; j < m_channels; ++j)
  {
          if (m_noticedChannels[j + offset]) 
          {       indirectNotice[ii]  = j;
                  ++ii;
          }
  }
}

void SpectralData::reportKeywords () const
{
  tcout << "  Telescope: " << m_telescope << " Instrument: " 
        << m_instrument << "  Channel Type: " << m_channelType << '\n'
        << "  Exposure Time: " << m_exposureTime << " sec\n"; 
  if (m_xflt.size())
  {
        tcout << "  Filtering Keys: \n";
	std::map<string, Real>::const_iterator 
	    i_xfltBeg = m_xflt.begin(),
	    i_xfltEnd = m_xflt.end();

	while(i_xfltBeg != i_xfltEnd)
	{
	    tcout << "    " <<i_xfltBeg->first << ": " << i_xfltBeg->second 
                  << std::endl;
	    ++i_xfltBeg;
	}

  }

  tcout << std::flush;
}

void SpectralData::reportRates () const
{
  using namespace std;
  ios_base::fmtflags save(tcout.flags());
  streamsize p(tcout.precision(3));

  tcout << "Net count rate (cts/s) for Spectrum:" << left << m_spectrumNumber << "  " 
        << setw(9) << scientific << m_netFlux << " +/- " << setw(9) << m_netVariance;
  if (m_background && m_totalFlux != 0)
  {
        tcout << " (" << setprecision(1) << fixed << 100.*m_netFlux/m_totalFlux << " % total)"
             << endl;

  }         
  else tcout << endl;

  // This was initially added as a patch enhancement to display rate change
  // due to a Corfile.  It would be better at some point to simply save
  // the original correction calculation performed in computeTotals rather
  // than recalculate here.  That of course will require an interface change.
  if (m_correction)
  {
     int offset = m_startChan - m_firstChan;
     Real totalCorrection = 0.0;
     for (size_t j = 0; j < m_channels; ++j)
     {
        if (m_noticedChannels[j + offset]) 
        {
            totalCorrection += m_areaScale[j]*m_correction->spectrum()[j];
        }
     }
     totalCorrection *= m_correctionScale;
     tcout << " After correction of " << setprecision(3) << setw(9) << scientific 
        << totalCorrection << " (using cornorm ";
     tcout.precision(4);
     tcout.flags(save);
     tcout << showpoint << m_correctionScale << ")" << endl;
  }

  tcout.precision(p);
  tcout.flags(save);
}

void SpectralData::resetDetsDataGroupNums (size_t dataGroup)
{
  for (size_t i=0; i<m_detector.size(); ++i)
  {
     Response* rsp = m_detector[i];
     if (rsp)
     {
        rsp->dataGroup(dataGroup);
     }
  }
}

void SpectralData::initializeFake (const SpectralData::ChannelInfo& chanInfo)
{
  // Creating a fake spectrum that is NOT based on an existing spectrum.
  // It's configuration will have to be determined from a response file
  // and possibly an existing background file.
   if (chanInfo.m_detChans < m_channels)
   {
     // This situation should have been checked for before this point.
      string msg("Attempting to create a faked spectrum with DETCHANS less than nChans");
      throw RedAlert(msg);
   }
   m_groupingInfo.resize(m_channels, 1);
   m_qualityInfo.resize(m_channels, 0);
   DataUtility::encodeGQ(m_groupingInfo, m_qualityInfo, m_gqString); 
   // Setting m_backgroundScale to -1.0 is a bit of a hack.  When it
   // comes time to create the background (if any), the -1.0 is a flag
   // telling the background to place its own backgroundScale value
   // into this slot.  NOTE: If there is no background, the -1.0 should
   // still manually be replaced with 1.0 at some later point.
   m_backgroundScale.resize(m_channels);
   m_backgroundScale = -1.0;
   m_startChan = chanInfo.m_startChan;
   m_endChan = chanInfo.m_endChan;
   m_firstChan = chanInfo.m_firstChan;
   // The rest of these data members may be altered by the presence
   // of an existing background file (see reinitFromBackground).
   m_areaScale.resize(m_channels);
   m_areaScale = 1.0;
   m_noticedChannels = BoolArray(chanInfo.m_detChans, false);
   for (size_t i = m_startChan-m_firstChan; i <= m_endChan-m_firstChan; ++i)
   {
      m_noticedChannels[i] = true;
   }
   // These three keywords will be filled in later by the associated RMF.
   m_telescope = "USE_FAKEIT_RMF";
   m_instrument = "USE_FAKEIT_RMF";
   m_channelType = "USE_FAKEIT_RMF";     	
}

void SpectralData::reinitFromBackground ()
{
  // This function deals with the chicken-and-egg problem when trying
  // to create a fake spectrum based on information in an existing
  // background file.  A BackCorr object requires an existing
  // SpectralData object, but we need the BackCorr information to
  // (fully) create the SD object in this case.  Therefore, we
  // initialize the SD first with default info, create the BackCorr
  // object, which is then used here to complete initialization.
   if (m_background)
   {
      SpectralData* const bckSpectrum = m_background->data();
      if (m_channels != bckSpectrum->channels())
      {
         string msg("Size mismatch between fake spectrum and background spectrum.\n");
         msg += "    while attempting to create data set: ";
         msg += parent()->dataName();
         msg += "\n";
         throw RedAlert(msg);            
      }
      m_areaScale.resize(m_channels);
      m_areaScale = bckSpectrum->areaScale();
      m_channelType = bckSpectrum->channelType(); 
      m_telescope = bckSpectrum->telescope();
      m_instrument = bckSpectrum->instrument();
      // m_noticedChannels size should already be in-synch with the
      // background file.  This is a result of the getNChansForFake
      // function digging out the background file's DETCHANS prior
      // to creating this SD object.
      size_t offset = m_startChan - m_firstChan; 
      size_t end = m_endChan - m_firstChan;
      for (size_t i=0; i<offset; ++i)
      {
         m_noticedChannels[i] = false;
      } 
      for (size_t i=offset; i<=end; ++i)
      {
         m_noticedChannels[i] = true;
      } 
      for (size_t i=end+1; i<m_noticedChannels.size(); ++i)
      {
         m_noticedChannels[i] = false;
      } 	
   }
   else
   {
      string msg("Attempting to access non-existing background obj\n");
      msg += "    in SpectralData::reinitFromBackground()\n";
      throw RedAlert(msg);
   }
}

void SpectralData::CreateMemento ()
{
    //needed for potential memory leak? appropriate here?
    if(m_memento)
	delete m_memento;

    m_memento = new SpectralMemento();

}

bool SpectralData::checkForPoisson (const SpectralData* parentSpec)
{
   // By this point, areaScale and backgroundScale arrays should
   // already have been verified to be size == m_channels.
   // "Poisson-ness" applies only to the state of the RAW VARIANCE,
   // not the variance that may have been manipulated by special weighting.

   // Convert from cts/cm^2-sec to cts.
   RealArray dataNorm(m_areaScale*m_exposureTime);
   if (parentSpec)
   {
      // We are dealing with a background spectrum.
      dataNorm *= m_backgroundScale;
      dataNorm /= parentSpec->backgroundScale();
   }
   if (m_spectrum.size() != dataNorm.size())
   {
      throw RedAlert("Array size mismatch in SpectralData::checkForPoisson.");
   }
   for (size_t i=0; i<m_channels; ++i)
   {
      if (dataNorm[i] == 0.0)  dataNorm[i] = 1.0;
   }
   RealArray counts(m_spectrum*dataNorm);
   RealArray error(m_rawVariance*dataNorm*dataNorm);
   const Real LIMIT = 1.0e-5;
   m_isPoisson = true;
   // If any channel is not poisson, then isPoisson = false.
   for (size_t i=0; i<m_channels; ++i)
   {
      if (std::fabs(counts[i] - error[i]) > LIMIT)
      {
         m_isPoisson = false;
         break;
      } 
   }
   return m_isPoisson;
}

void SpectralData::SetMemento ()
{
    using namespace XSContainer;

    if(m_memento)
    {
	size_t dataGroup = m_memento->m_dataGroup;
	string dataName = m_memento->m_dataName;

	DataArrayConstIt
	    __beg_data = datasets->dataArray().begin(),
	    __end_data = datasets->dataArray().end();

	bool found = false;

	while(__beg_data != __end_data && !found)
	{
	    if(__beg_data->first == dataName &&
	       __beg_data->second->dataGroup() == dataGroup)
	    {
		//reinsert into existing DataSet
		DataSet* data_set = __beg_data->second;

		if(data_set->isMultiple())
		{

		}
	    }

	    ++__beg_data;
	}

	if(!found)
	{
	    //must create new DataSet
	}
    }
}

void SpectralData::calcEffectiveAreas ()
{
  // This can be a very time-consuming operation, therefore do not
  // call until it is absolutely necessary (ie. when plotting
  // data with setplot area = true).  If multiple detectors,
  // simply sum and take average.
   m_effectiveAreas.resize(0);
   int nResps = 0;
   for (size_t i=0; i<m_detector.size(); ++i)
   {
      if (m_detector[i])
      {
         RealArray detEffectiveAreas;
         m_detector[i]->calcEffAreaPerChan(detEffectiveAreas);
         if (!m_effectiveAreas.size())
         {
            m_effectiveAreas.resize(detEffectiveAreas.size(), .0);
         }
         else if (m_effectiveAreas.size() != detEffectiveAreas.size())
         {
            throw RedAlert("Effective areas array size mismatch in SpectralData::calcEffectiveAreas");
         }
         m_effectiveAreas += detEffectiveAreas;
         ++nResps;
      }
   }
   if (nResps)
      m_effectiveAreas /= static_cast<Real>(nResps);
   else
   {
      std::ostringstream msg;
      msg << "No responses loaded for spectrum " << m_spectrumNumber
         << ".  Cannot calculate effective areas.";
      throw YellowAlert(msg.str()); 
   }
}

void SpectralData::clearEffectiveAreas ()
{
   m_effectiveAreas.resize(0);
}

void SpectralData::addToSpectrum (const RealArray& addValue, bool scaleCounts)
{
   // Initially implemented for use by ModelContainer::generateFakeData.
   if (!m_spectrum.size())
      m_spectrum.resize(addValue.size());
   if (m_spectrum.size() != addValue.size())
   {
      throw RedAlert("Spectrum size mismatch in SpectralData::addToSpectrum");
   } 
   RealArray array(addValue);
   if ( scaleCounts)
   {
          array /= m_exposureTime;
          array /= m_areaScale; 
   }
   m_spectrum += array;  
   m_spectrumIsZeroed = false;  
}

void SpectralData::reduceNDets (size_t newNDets)
{
   // This function should not have been called unless ALL spectra
   // have already been checked to have no responses in the detector
   // slots about to be deleted.  Since this could cause a 
   // nasty memory leak though, let's check again at the cost
   // of a small speed hit.
   if (!newNDets)
      throw RedAlert("Cannot resize detector array to size 0");
   size_t nCurrDets = m_detector.size();
   if (nCurrDets)
   {
      for (size_t i=nCurrDets-1; i>=newNDets; --i)
      {
         if (m_detector[i])
         {
            throw RedAlert("Attempting to remove detector slots that are still in use.");
         }
      }
   }
   m_detector.resize(newNDets);  
   m_responseHooks.resize(newNDets);
   m_responseChanged.resize(newNDets);
   m_arfChanged.resize(newNDets);  
}

void SpectralData::increaseNDets (size_t newNDets)
{
   if (newNDets < m_detector.size())
      throw RedAlert("New detector size is smaller than current size in increaseNDets.");
   m_detector.resize(newNDets, static_cast<Response*>(0)); 
   m_responseHooks.resize(newNDets,static_cast<Response*>(0));   
   m_responseChanged.resize(newNDets,false);
   m_arfChanged.resize(newNDets,false);  
}

void SpectralData::reportPha () const
{
   using namespace std;
   ios_base::fmtflags saveFlags(tcout.flags());
   streamsize savePrec(tcout.precision());
   tcout << xsverbose(10);
   tcout << setw(6) << "Bin " << setw(12) << "Source   " << setw(12) << "Error   ";
   if (m_background)
      tcout << setw(12) << "Back    " << setw(12) << "Error   ";
   if (m_correction)
      tcout << setw(12) << "Corr    ";
   tcout << setw(12) << "Net    " << setw(12) << "Error   ";
   bool isMod = false;
   const size_t szSpec = m_indirectNotice.size();
   RealArray foldedMod(.0,szSpec);
   if (XSContainer::models->modelSet().size())
   {
      for (size_t i=0; i<m_detector.size(); ++i)
      {
         const Response* resp = m_detector[i];
         if (resp && resp->active())
         {
            const Model* mod = XSContainer::models->lookup(resp);
            if (mod)
            {
               isMod = true;
               foldedMod += mod->foldedModel(m_spectrumNumber)[m_indirectNotice];
            }
         }
      }
   }
   if (isMod)
   {
      tcout << setw(12) << "Model   ";
   }
   tcout << endl;

   tcout << scientific << showpoint << setprecision(4);
   for (size_t i=0; i<szSpec; ++i)
   {
      const size_t iCh = m_indirectNotice[i];
      Real phaObs = m_spectrum[iCh];
      Real phaErr = sqrt(m_variance[iCh]);
      Real net = phaObs;
      Real netErr = phaErr;
      tcout << setw(6) << iCh+1 << setw(12) << phaObs << setw(12) << phaErr;

      if (m_background)
      {
         Real phaBack = m_background->data()->spectrum(iCh);
         Real phaBackErr =sqrt(m_background->data()->variance(iCh));
         net -= phaBack;
         netErr = sqrt(m_variance[iCh] + m_background->data()->variance(iCh));
         tcout << setw(12) << phaBack << setw(12) << phaBackErr;
      }
      if (m_correction)
      {
         Real phaCorr = m_correctionScale*m_correction->data()->spectrum(iCh);
         net -= phaCorr;
         tcout << setw(12) << phaCorr;
      }
      tcout << setw(12) << net << setw(12) << netErr;
      if (isMod)
      {
         // This array only contains the noticed channels, so use 
         // index i and not iCh.
         tcout << setw(12) << foldedMod[i];
      }
      tcout <<endl;
   }
   tcout << xsverbose();
   tcout.precision(savePrec);
   tcout.flags(saveFlags);
}

void SpectralData::reportGrouping () const
{
  // Grouping info for higher chatter.
  using namespace std;
  if (tpout.maxChatter() > 10)
  {
     tpout<<xsverbose(11);
     const size_t nAllPossibleChans = m_noticedChannels.size();
     const size_t offset = m_startChan - m_firstChan;
     const size_t lastRelevantChan = offset + m_groupingInfo.size();
     const size_t chansPerLine = 50;
     const size_t nLines = (nAllPossibleChans-1)/chansPerLine + 1;
     tcout << " Grouping data :\n   " << nAllPossibleChans 
         << " channels grouped to " << m_channels << endl; 
     size_t iChan=1;
     for (size_t iLine=1; iLine<=nLines; ++iLine)
     {
        tcout << right << setw(9) << iChan << ' ' << flush;
        for (size_t j=1; j<=chansPerLine && iChan<=nAllPossibleChans; ++j,++iChan)
        {
           if (iChan > offset && iChan <= lastRelevantChan)
           {
              size_t iGrp = iChan - offset - 1;
              const int groupVal = m_groupingInfo[iGrp];
              const int qualVal = m_qualityInfo[iGrp];
              if (!qualVal)
              {
                 if (groupVal == 1)
                    tcout << '+';
                 else 
                    tcout << '-';
              }
              else if (qualVal > 1)
              {
                 if (groupVal == 1)
                    tcout << '*';
                 else
                    tcout << '=';
              }
              else  
                 tcout << ' ';
           }
           else
              tcout << ' ';
        }
        tcout << endl;
     }
     tpout <<xsverbose();
  }
}

void SpectralData::report (bool orderByFile) const
{
  using namespace std;
  ios_base::fmtflags save (tcout.flags());  
  streamsize p(tcout.precision(4));
  if (orderByFile)
  {
     tcout << "Spectral Data File: " << m_parent->dataName(); 
     if (m_rowNumber > 0 ) tcout << "{"  << m_rowNumber  <<"}";
     tcout << "  Spectrum " << m_spectrumNumber;
  }
  else
  {
     tcout << "Spectrum " << m_spectrumNumber 
           << "  Spectral Data File: " << m_parent->dataName(); 
     if (m_rowNumber > 0 ) tcout << "{"  << m_rowNumber  <<"}";
  }
  tcout << std::endl;
  //tcout << " at address " << reinterpret_cast<int>(data);
  reportRates();

  tcout << " Assigned to Data Group " << m_parent->dataGroup()
        << " and Plot Group " << m_plotGroup << '\n';

  reportNoticed();
  reportKeywords();
  tcout << " Using fit statistic: " <<m_statName << "\n";
  tcout << " Using test statistic: " <<m_testStatName << "\n";
  if ( m_backgroundFile != "" )
  {
     const SpectralData* bckSpec = m_background->data();
     size_t bckRow = bckSpec->rowNumber();
     tcout << left << setw(38) << " Using Background File " <<  m_backgroundFile;
     if (bckRow)
     {
        tcout << '{' << bckRow << '}';
     }
     tcout << std::endl << "  Background Exposure Time: " 
           << bckSpec->exposureTime() << " sec" << std::endl;

  }
  if ( m_correctionFile != "")
  {
     size_t corRow = m_correction->data()->rowNumber();
     tcout << left << setw(38) << " Using Correction File " <<  m_correctionFile;
     if (corRow)
     {
        tcout << '{' << corRow << '}';
     }
     tcout << std::endl;
  }

  m_parent->reportResponse(m_rowNumber); 

  reportGrouping();

  tcout.precision(p); 
  tcout.flags(save);                    
  tcout << endl;
}

void SpectralData::background (Background* value)
{
  if (m_background)
  {
     delete m_background;
  }
  m_background = value;
}

void SpectralData::correction (Correction* value)
{
  if (m_correction)
  {
     delete m_correction;
  }
  m_correction = value;
}

// Additional Declarations
