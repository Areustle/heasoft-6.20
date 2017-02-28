//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSContainer.h>
#include <XSstreams.h>
#include <XSFit/Fit/Fit.h>
#include <XSFit/StatMethod/ChiSquare/ChiSquare.h>
#include <XSModel/Data/SpectralData.h>
#include <XSModel/Data/BackCorr/Background.h>
#include <XSModel/Data/Detector/Response.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/Model/Model.h>
#include <XSModel/Model/Component/SumComponent.h>
#include <XSPlot/Plot/PlotGroup.h>
#include <XSPlot/Plot/PlotSettings.h>
#include <XSPlot/Plot/PlotVector.h>
#include <XSUtil/Error/Error.h>
#include <XSUtil/Numerics/Numerics.h>
#include <algorithm>
#include <sstream>
#include <utility>

// CreateBinnedPlotGroups
#include <XSPlot/Plot/CreateBinnedPlotGroups.h>


// Class CreateBinnedPlotGroups 

CreateBinnedPlotGroups::CreateBinnedPlotGroups (bool isUnfolded, bool isCounts)
  : PlotGroupCreator(),
   m_isUnfolded(isUnfolded),
   m_modelPositionDirectory(),
   m_modelsForSpectra(),
   m_settings(0), // Non-owning
   m_counts(isCounts)
{
}


CreateBinnedPlotGroups::~CreateBinnedPlotGroups()
{
}


std::vector<PlotGroup*> CreateBinnedPlotGroups::createPlotGroups (const PlotSettings& settings)
{
  using namespace XSContainer;   
  m_settings = &settings;
  std::vector<PlotGroup*> plotGroups;

  size_t Ngroups (datasets->numberOfPlotGroups()); 
  itemizePlotGroupModels();

  std::vector<std::list<SpectralData*> > groupedSpectra(Ngroups,std::list<SpectralData*>());

  // This function owns the new PlotGroups until it returns.
  // Therefore it's responsible for cleanup if anything throws.
  try
  {
     for (size_t j = 0; j < Ngroups; ++j )
     {
        int chanXGroupOffset (0); // icbeg
        // can throw an "EnergiesNotDefined" exception...
        // Effective areas will be initialized in here (if needed).
        PlotGroup* gr = initializePlotGroup(j+1,groupedSpectra[j]);
        plotGroups.push_back(gr);

        // gr->n can be 0 for the case of ALL channels ignored.  Otherwise
        // n does NOT take ignored channels into account.
        if (gr->n > 0)
        {
           int npts (0);
           int latestChannel(0);
           bool moreBins (true);
           do // execute at least once...
           {
	     if ( (moreBins = getNextPlotBin(gr, npts, groupedSpectra[j], 
					     latestChannel)) )
              {
                 if (m_settings->xOption() == CHANNELS) 
                     gr->xAxis.data[npts] += chanXGroupOffset;
                 ++npts;
              }
	      else gr->n = npts;

           }  while ( moreBins );
           if (m_settings->divideByArea()) 
                 clearEffectiveAreas(groupedSpectra[j]);
           if (gr->model.size() > 1)
           {
	      std::vector<Real>& modelSum = gr->model[0].data;
              for (size_t l = 1; l < gr->model.size(); ++l)
              {
	         std::vector<Real>& model = gr->model[l].data;
                 for (int k = 0; k < npts; ++k) 
                 {
                    modelSum[k] += model[k];       
                 }
              }
           }
           chanXGroupOffset += gr->channelPlotSpacing;
        } // end if gr->n > 0 
     } // end plot groups loop
  }
  catch (YellowAlert&)
  {
     for (size_t i=0; i<plotGroups.size(); ++i)
     {
        // Whether or not initEffectiveAreas was called, or
        // effectiveAreas have already been cleared for the
        // particular group, this is safe to do:
        clearEffectiveAreas(groupedSpectra[i]);
        delete plotGroups[i];
     }
     throw;
  }
  return plotGroups;
}

void CreateBinnedPlotGroups::itemizePlotGroupModels ()
{
   using namespace XSContainer;
   // Only count active/on models.
   m_modelPositionDirectory.clear();
   m_modelsForSpectra.clear();
   // This is the highest possible number of models used.
   const size_t allSources = datasets->numSourcesForSpectra();
   // To speed things along so we don't always have to check every detector
   // slot for every spectrum, first see which slots actually have active 
   // models associated with them, and store the number of sum comps for 
   // a given model so its lookup only needs to be performed once.
   std::vector<size_t> sourcesWithActiveMods;
   std::vector<size_t> numCompsForMods(allSources,0);
   for (size_t i=0; i<allSources; ++i)
   {
      string modName(models->lookupModelForSource(i+1));
      // Model with modName must be active/on or active/off.
      if (modName.length())
      {
         const Model* mod = models->lookup(modName);
         if (mod && mod->isActive())
         {
            // Let's keep source numbers 1-based throughout.
            sourcesWithActiveMods.push_back(i+1);
            const size_t nComps = mod->sources().size();
            numCompsForMods[i] = nComps;
         }
      }
   }
   size_t prevGroup = 1;
   const SpecGroup& spectra = m_settings->spectra();
   m_modelsForSpectra.resize(spectra.size());
   std::set<size_t> sourcesForGroup;
   SpecGroup::const_iterator itSpec = spectra.begin();
   SpecGroup::const_iterator itSpecEnd = spectra.end();
   while (itSpec != itSpecEnd)
   {
      // Don't assume spectra are in order by spectrum number,
      // only by plot group number.
      const std::vector<const Model*> 
                modsForSpec(models->getModsForSpec(itSpec->second));
      std::vector<const Model*>& spectraMods = 
                m_modelsForSpectra[itSpec->second->spectrumNumber()-1];
      // modsForSpec has by definition size = allSources, and
      // may have many (or all) elements = 0.  We don't need to
      // check each one, thanks to sourcesWithActiveMods subset.
      std::vector<size_t>::const_iterator itActSource =
                sourcesWithActiveMods.begin();
      std::vector<size_t>::const_iterator itActSourceEnd =
                sourcesWithActiveMods.end();
      while (itActSource != itActSourceEnd)
      {
         const Model* mod = modsForSpec[*itActSource-1];
         if (mod)
         {
            spectraMods.push_back(mod);
            // Don't want to repeat source nums, that's
            // why this is a set and not a multiset.
            sourcesForGroup.insert(mod->sourceNumber());
         }
         ++itActSource;
      }
      ++itSpec;
      if (itSpec == itSpecEnd || itSpec->first != prevGroup)
      {
         // End of current plot group.
         if (itSpec != itSpecEnd)
            prevGroup = itSpec->first;
         // If more than one model, reserve 0 element for sum.
         size_t offset = sourcesForGroup.size() > 1 ? 1 : 0;
         std::set<size_t>::const_iterator itNum = sourcesForGroup.begin();
         std::map<size_t,size_t> assignments;
         size_t count = 0;
         while (itNum != sourcesForGroup.end())
         {
            size_t sourceForGroup = *itNum;
            assignments[sourceForGroup] = count + offset;
            ++itNum;
            ++count;
         }
         m_modelPositionDirectory.push_back(assignments);
         sourcesForGroup.clear();
      }
   }
}

PlotGroup* CreateBinnedPlotGroups::initializePlotGroup (size_t groupIndex, std::list<SpectralData*>& groupSpectra)
{

   using namespace XSContainer;

   // This is guaranteed to return a non-null PlotGroup pointer, or
   // else it throws.

    // first: get the spectra which belong to this plot group.
    // all of the spectra in the group have been checked to have the same number of
    // channels.      

   std::pair<SpecGroup::const_iterator,SpecGroup::const_iterator> itRange = 
        m_settings->spectra().equal_range(groupIndex);
   SpecGroup::const_iterator itSpec = itRange.first;
   bool notChannels = (m_settings->xOption() != CHANNELS);

   while (itSpec != itRange.second)
   {
      SpectralData* spec = itSpec->second;
      // test for "ignored" data and do not add to group if no channels are there.
      const BoolArray& noticed = spec->noticedChannels();
      BoolArray::const_iterator itNotice (std::find(noticed.begin(),noticed.end(),true));

      if (itNotice != noticed.end())
      {
         groupSpectra.push_back(spec); 
         if (notChannels)
         {
            std::vector<Response*>::const_iterator itDet = spec->detector().begin();         
            std::vector<Response*>::const_iterator itDetEnd = spec->detector().end();
            while (itDet != itDetEnd)
            {
               if (*itDet && (*itDet)->eboundsMin().size())
                  break;
               ++itDet;
            }
            if (itDet == itDetEnd)         
            {
	        std::ostringstream diag;
	        diag << "No energies defined for spectrum " 
                     << spec->spectrumNumber() << ", but plot requested on "
		     << "energy/wavelength axis.\n Plot Group " << groupIndex 
		     << " will not be constructed \n\n";
	        throw YellowAlert(diag.str());
            }
         }
      }
      ++itSpec;
   }

   // groupSpectra now contains all spectra that will make up plot group.

   PlotGroup* gr = 0;
   int Nchannels (0);
   size_t numModVects = m_modelPositionDirectory[groupIndex-1].size();
   if (groupSpectra.empty()) 
   {
       gr = new PlotGroup(0, numModVects);
   }
   else
   {

       Nchannels = (*groupSpectra.begin())->channels();      
       // create a PlotGroup & allocate its arrays.
       gr = new PlotGroup(Nchannels,numModVects);
       // saveData will be needed as an internal work array during binning.
       gr->saveData.resize(Nchannels, 0.0);
       // Even if background is not to be displayed, background spectra
       // will still contribute to error calculation.  So always allocate
       // these vectors.
       gr->background.data.resize(Nchannels,0);
       gr->background.errors.push_back(std::vector<Real>(Nchannels,0));
   }

   gr->single = (Nchannels == 1);

   std::map<int,PlotSettings::RebinInfo>::const_iterator rbEntry = 
        m_settings->groupsRebinInfo().find(groupIndex); 
   if (rbEntry == m_settings->groupsRebinInfo().end())
   {
       // Use all-group default values, key = -1, always in map.
       rbEntry = m_settings->groupsRebinInfo().find(-1);
   }
   gr->critSignificance = rbEntry->second.sigma;
   gr->critNumChans = rbEntry->second.maxBins;
   gr->errorType    = rbEntry->second.mode; 

   std::list<SpectralData*>::const_iterator itGs = groupSpectra.begin();
   std::list<SpectralData*>::const_iterator itGsEnd = groupSpectra.end();

   int maxChan (0);
   int minChan (999999);

   if (m_settings->divideByArea())
      initEffectiveAreas(groupSpectra);

   while (itGs != itGsEnd)
   {
       // Find the first and last contributing channels.
       // For channel to contribute to a bin, it must 1) be noticed, 
       // and 2) if using setplot area, then area != 0.0.

       const SpectralData* spec = *itGs;

       // We really shouldn't have to do this here.  SpectralData
       // shouldn't be waiting till prepareForFit to update its
       // m_indirectNotice member.
       std::valarray<size_t> indirectNotice;
       spec->buildIndirectNotice(indirectNotice);

       bool isFound = false;
       size_t j=0;
       while (!isFound && j<indirectNotice.size())
       {
          isFound = !m_settings->divideByArea() || 
                     (spec->effectiveAreas()[indirectNotice[j]] != 0.0);
          if (!isFound)
             ++j;       
       }
       if (isFound)
       {
          minChan = std::min(minChan, static_cast<int>(indirectNotice[j]));
          // Now work backwards and find the max.
          // If we're here, we know indirectNotice isn't empty and that
          // we will find a contributing chan.  At worst it will be the
          // same as minChan.
          j  = indirectNotice.size()-1;
          isFound = false;
          while (!isFound)
          {
             isFound =  !m_settings->divideByArea() || 
                    (spec->effectiveAreas()[indirectNotice[j]] != 0.0);
             if (!isFound)
                --j;
          } 
          maxChan = std::max(maxChan,static_cast<int>(indirectNotice[j]));
       }
       ++itGs;       
   }

   // chanXSpacing is used to separate plot groups when they are
   // viewed in channel space.
   // lastInputChannel is 0-based.
   gr->channelPlotSpacing = (maxChan - minChan);
   if ( gr->channelPlotSpacing < 0 )
   {
       // group is totally ignored.
       gr->channelPlotSpacing = 10;
       gr->lastInputChannel = 0;
   }
   else if ( gr->single )
   {
       // single point (data contains observation through a filter,
       // not a spectrum).
       gr->channelPlotSpacing = 20;
       gr->lastInputChannel = 0;
   }
   else
   {
       // square up to nearest multiple of 10.
       gr->channelPlotSpacing = 10*( (gr->channelPlotSpacing+2)/10 + 1 );       
       gr->lastInputChannel = maxChan;
   }

   const std::map<size_t,size_t>& sourceAssignments = 
               m_modelPositionDirectory[groupIndex - 1];
   std::map<size_t,size_t>::const_iterator itAssignment =
               sourceAssignments.begin();
   std::map<size_t,size_t>::const_iterator itAssignmentEnd =
               sourceAssignments.end();
   while (itAssignment != itAssignmentEnd)
   {
      size_t sourceNum = itAssignment->first;
      size_t sourcePos = itAssignment->second;
      // Any info in sourceAssigments has already been verified to
      // correspond to an active model object.  No need to check things
      // again in here.
      const string modName = models->lookupModelForSource(sourceNum);
      const Model* mod = models->lookup(modName);
      const size_t compOffset = gr->model.size() > 1 ? 1 : 0;

      if (m_settings->showAddComponent() || m_isUnfolded)
      {
         size_t numSumComps = mod->sources().size(); 
         if (numSumComps > 1)
         {
            std::list<PlotVector>& currentComps =
                  gr->sources[sourcePos-compOffset];
            for (size_t i=0; i<numSumComps; ++i)
            {
               currentComps.push_back(PlotVector(gr->n));
            }
         }
      } // end if show add components      
      ++itAssignment;
   }

   if (m_isUnfolded)
   {
      gr->auxData.resize(gr->model.size(),PlotVector(gr->n));
   }
   return gr;
}

bool CreateBinnedPlotGroups::getNextPlotBin (PlotGroup* gr, int npts, std::list<SpectralData*>& groupSpectra, int& latestChannel)
{

  // This is called once for each plot bin.  Function returns TRUE if ANY noticed channels
  // have been added to the bin. A return value of FALSE implies that it has reached
  // the last channel before finding a bin contributor, and therefore no more calls
  // are required to this function for this particular PlotGroup.


  // Note: This algorithm works with the ASSUMPTION that when in energy/wavelength mode,
  // all spectra in the PlotGroup have exactly the same energy bin values (though they
  // are allowed to have different ignore/notice settings).  If this assumption ever
  // becomes invalid, the older more complicated algorithm can be retrieved from 
  // the repository.

  // Also can ASSUME that gr has already been checked to contain AT LEAST 1 spectrum
  // with AT LEAST 1 noticed channel that is not nullified by the zero-area defect.

  // Input:  npts = the 0-based index of the current bin.
  //         latestChannel = the first 0-based channel to examine.
  // Output: latestChannel = 1 beyond the last examined channel.

  // gr->lastInputChannel = The highest 0-based noticed channel in any of the
  //        spectra within the group.  We can always assume that it is safe
  //        to access the i_th element from one of gr's vectors,
  //        where 0 <= i <= lastInputChannel.

  if (groupSpectra.empty() || latestChannel > gr->lastInputChannel) 
        return false;

  // This is the 1-based xspec plot group index, and is the
  // same for all spectra in the list.
  const size_t iXsPlotGroup = (*groupSpectra.begin())->plotGroup();

  Real count = 0, backgroundCount = 0;

  // if there is more than one source per spectrum, the models are stored
  // in the model std:vector<Real*> container enumerated from 1.
  // if there isn't then that vector only has one element, enumerated as zero.
  // for the latter case, the source number is 1 and the binned model is stored
  // in location zero. for the former case, the binned models are stores in
  // location == source number and the sum will ultimately be stored in location
  // zero.

  bool doneBin = false;
  bool notChannelSpace = m_settings->xOption() != CHANNELS;

  int firstChannelInBin = -1;
  int lastChannelInBin = -1;

  int chanEnd = gr->lastInputChannel;
  int numChannelsBinned = 0;

  // These will store the emin of the first channel in bin and
  // the emax of the last channel.  If energies are in descending order,
  // then startingEnergy > endingEnergy.
  Real startingEnergy = 0.0;
  Real endingEnergy = 0.0;

  Real critVar ( gr->critSignificance * gr->critSignificance );  
  Real& datapt = gr->yData.data[npts];
  Real& errorpt = gr->yData.errors[0][npts];
  Real& backpt = gr->background.data[npts];
  Real& backErrorpt = gr->background.errors[0][npts];
  // Using saveData array as a miscellaneous storage area.  By default
  // it will pass along area*time info for the bin, and is needed
  // for plot chisq when using C-stat.
  Real& areaSum = gr->saveData[npts];
  Real backSum = .0;
  Real backScaleRatio = .0;
  Real nBackChans = 0;
  const std::map<size_t,size_t>& modAssignments =
        m_modelPositionDirectory[iXsPlotGroup-1];

  // Needed to determine whether to apply min error.
  bool allUsingChiSquare = true; 

  while (!doneBin )
  {
     std::list<SpectralData*>::const_iterator gs    (groupSpectra.begin());
     std::list<SpectralData*>::const_iterator gsEnd (groupSpectra.end());

     // Issue zero area warning at most one time for each spectrum.
     size_t idxSpec = 0;
     static std::vector<bool> issuedAreaWarning;           
     if (latestChannel == 0)
     {
        //reset for new plot group
        issuedAreaWarning.clear();
        issuedAreaWarning.resize(groupSpectra.size(),false);
     }

     while (gs != gsEnd)
     {       
        const SpectralData* current = *gs;
        size_t spectrumNumber (current->spectrumNumber());
        if (current->statName() != "chi")
           allUsingChiSquare = false;
        const int offset = current->startChan() - current->firstChan();
        // if we got this far, detector must exist or be irrelevant.
        const Response* firstResp = 0;
        // This refers to the EBOUNDS energies.
        bool isEngDescending = false;
        std::vector<Response*>::const_iterator itResp = current->detector().begin();
        while (itResp != current->detector().end() && !firstResp)
        {
           // This could be a dummyrsp with no ebounds array.  Need to check.
           // But each spectrum in plot group must have at least one response
           // which passes this test if energy/wavelength is to be plotted.
           // Otherwise initializePlotGroup will have thrown.
           if (*itResp && (*itResp)->eboundsMin().size())
           {
              firstResp = *itResp;
              isEngDescending = firstResp->eboundsMin()[0] > 
                        firstResp->eboundsMin()[firstResp->eboundsMin().size()-1];
           }
           ++itResp;
        }

        // If dividing by area, treat channels with zero effective
        // area exactly as ignored channels. 

        bool areaDefect = (m_settings->divideByArea() && 
                        current->effectiveAreas()[latestChannel] == 0.0);

        // Don't issue warning if channel is already ignored
        if ( (*gs)->noticedChannels(latestChannel + offset) && areaDefect)
        {
           if (!issuedAreaWarning[idxSpec])
           {
              tcout << "***Warning: 1 or more channels in spectrum "
                 << spectrumNumber << " have zero effective area.\n"
                 << "   They will be ignored in this plot." << std::endl;
              issuedAreaWarning[idxSpec] = true;
           }
        }

        if ( (*gs)->noticedChannels(latestChannel + offset) && !areaDefect)
        {
           if (firstChannelInBin == -1)
           {
              firstChannelInBin = latestChannel;
              chanEnd = std::min(chanEnd,gr->critNumChans+latestChannel-1);
              if (notChannelSpace)
              {
                 startingEnergy = isEngDescending ? firstResp->eboundsMax()[latestChannel] :
                     firstResp->eboundsMin()[latestChannel];
              }
           }
           if ( notChannelSpace )
           {
              endingEnergy = isEngDescending ? firstResp->eboundsMin()[latestChannel] :
                     firstResp->eboundsMax()[latestChannel];
           }

           // accumulate data from SpectralData
           Real areaTimeFactor ( current->exposureTime()*current->areaScale(latestChannel));
           areaSum += areaTimeFactor;
           Real factor = m_counts ? areaTimeFactor : 1;
           if (m_settings->divideByArea())
           {
              Real effArea = current->effectiveAreas()[latestChannel];
              // We've already checked for zero area.
              factor /= effArea;
           }
           datapt += current->spectrum(latestChannel)*factor; 
           count += current->spectrum(latestChannel)*areaTimeFactor;
           errorpt += factor*factor*current->variance(latestChannel);
           if ( current->background() )
           {
              const SpectralData* bckSpec = current->background()->data();
              const RealArray& backgroundSpectrum = bckSpec->spectrum();
              const RealArray& backgroundVariance = bckSpec->variance();
              Real backContribution = backgroundSpectrum[latestChannel]*factor;
              backpt += backContribution;
              datapt -=  backContribution;
              backgroundCount += backgroundSpectrum[latestChannel]*areaTimeFactor;
              Real backErrContribution = factor*factor*backgroundVariance[latestChannel];
              errorpt += backErrContribution;
              backErrorpt += backErrContribution;
              // The following info is only needed if a minimum
              // variance needs to be applied in setError.  Taking
              // the average of these quantities across the bins is
              // merely a convenient way (and not necessarily the best)
              // for determining a minimum variance rate.
              backSum += bckSpec->exposureTime()*bckSpec->areaScale(latestChannel);
              backScaleRatio += 
                current->backgroundScale(latestChannel)/bckSpec->backgroundScale(latestChannel);
              ++nBackChans;

           }
           if ( current->correction())
           {
              const RealArray& correctionSpectrum 
                              = current->correction()->spectrum();
              datapt -= current->correctionScale()*
                                      correctionSpectrum[latestChannel]*factor;
              count -=  current->correctionScale()*
                              correctionSpectrum[latestChannel]*areaTimeFactor;      
           }   


           std::vector<const Model*>::const_iterator itMod = 
                m_modelsForSpectra[spectrumNumber-1].begin();
           std::vector<const Model*>::const_iterator itModEnd = 
                m_modelsForSpectra[spectrumNumber-1].end();
           while (itMod != itModEnd)
           {
              const Model* m = *itMod;
              const size_t sourceIndex = 
                        modAssignments.find(m->sourceNumber())->second;
              const ArrayContainer& folded = m->foldedModel();
              ArrayContainer::const_iterator mmf = folded.find(spectrumNumber);
              if (mmf != folded.end())
              {
                  const RealArray& spectrumFit = mmf->second;
                  gr->model[sourceIndex].data[npts] 
                                    += spectrumFit[latestChannel]*factor;                                           
              }
              // The sources vector(s) for unfolded plots will be filled in
              // during integrateModelFluxes.
              if ( m_settings->showAddComponent() && !m_isUnfolded )
              {
                  const SourceList& sources = m->sources();
                  const size_t compOffset = gr->model.size() > 1 ? 1 : 0;
                  // gr->sources will be empty if number of m->sources isn't > 1,
                  // otherwise it should be the same size as m->sources.
                  if (!gr->sources[sourceIndex-compOffset].empty())
                  {
                     SourceList::const_iterator sl = sources.begin();    
                     SourceList::const_iterator slEnd = sources.end(); 
                     PlotVectorList::iterator vl  = 
                        gr->sources[sourceIndex-compOffset].begin();
                     while (sl != slEnd)
                     {                          
                         ArrayContainer::const_iterator slf 
                              = (*sl)->foldedPhotonFlux().find(spectrumNumber);
                         if ( slf != (*sl)->foldedPhotonFlux().end())
                         {
                            const RealArray& sourceFit = slf->second;
                            vl->data[npts] += sourceFit[latestChannel]*factor;       
                         }
                         ++vl, ++sl;
                     }  
                  } 
              } // end if show/add components
              ++itMod;
           }

           lastChannelInBin = latestChannel;
           ++numChannelsBinned;

        } // end if noticed channel and no area defect                   
        ++gs, ++idxSpec;
     } // end loop over spectra in group

     doneBin = ( latestChannel >= chanEnd ) || 
                ( datapt > 0 && datapt*datapt >= errorpt*critVar);

     ++latestChannel;
  } // end while !doneBin

  // Early exit to prevent possible divides by zero.
  if ( numChannelsBinned == 0 ) 
        return false;

// Now set the min and max channel energies. This convoluted procedure is
// necessary to handle the cases of both channels increasing and decreasing
// with energy

  Real chanEmin = std::min(startingEnergy,endingEnergy);
  Real chanEmax = std::max(startingEnergy,endingEnergy);

  // ASSUME redshiftToSource value of -1.0 has been prevented.
  chanEmin *= (1.0 + m_settings->redshiftToSource());
  chanEmax *= (1.0 + m_settings->redshiftToSource());

  areaSum /= numChannelsBinned;
  if (nBackChans)
  {
     backSum /= nBackChans;
     backScaleRatio /= nBackChans;
  }
  else
  {
     // Negative area*time tells calcMinVariance function that no
     // background exists.
     backSum = -1.0; 
     backScaleRatio = 1.0;
  }

// Set the error. If we are summing in quadrature simply convert from 
// variance to standard deviations.
// N.B. Should check the error method used against the variance weighting
// scheme set for data (DataUtility::statWeight). 

  setError(gr, npts, areaSum, backSum, count, backgroundCount, 
                backScaleRatio, allUsingChiSquare);

// Set the scaling factor to get data, error, and model in the correct
// units (note that the energy and wavelength normalizations assume that
// the no. of square cm. is the same in each grouped file). 


  Real norm(0);

  switch (m_settings->xOption())
  {
     default:
     case CHANNELS:
         norm = 1./Real(numChannelsBinned);
         break;
     case ENERGY:
        {
           Real unitFactor = m_settings->getUnitFactor(ENERGY);
           Real chans(latestChannel - firstChannelInBin);
           norm = (chanEmin == chanEmax) ? 1 
                 : chans/(std::abs(chanEmax-chanEmin)*unitFactor*Real(numChannelsBinned));            
        }    
        break;
     case WAVELENGTH:
        {
            Real chans(latestChannel - firstChannelInBin);
            if (m_settings->isWavePerHz())
               norm =  ( chanEmin == chanEmax || chanEmin <= 0 || chanEmax <= 0 ) ? 1
                   : chans/(std::abs( Numerics::KEVTOHZ*(chanEmax - chanEmin)
                                             *Real(numChannelsBinned)));
            else
            {
               Real unitFactor = m_settings->getUnitFactor(WAVELENGTH);
               norm =  ( chanEmin == chanEmax || chanEmin <= 0 || chanEmax <= 0 ) ? 1
                   : chans/(std::abs( Numerics::KEVTOA*(1./chanEmax - 1./chanEmin)
                              *unitFactor*Real(numChannelsBinned)));
            }               
        }    
        break;    
   }

   if ( norm != 1)
   {
        errorpt *= norm;
        datapt  *= norm;  
        backErrorpt *= norm;
        backpt *= norm;
        if ( gr->model.size() == 1) 
        {
           gr->model[0].data[npts] *= norm;
        }
        else
        {
           for ( size_t k = 1;  k < gr->model.size(); ++k)     
           {
              gr->model[k].data[npts] *= norm;       
           }
        }

        if (m_settings->showAddComponent() && !m_isUnfolded)
        {
            for (size_t j = 0 ; j  < gr->sources.size(); ++j )
            {   
                PlotVectorList::iterator vl = gr->sources[j].begin();    
                PlotVectorList::iterator vlEnd = gr->sources[j].end(); 
                while (vl != vlEnd)
                {                          
                   vl->data[npts] *= norm;
                   ++vl;
                }   
            }
        }        
    }

    // flux arrays

    if (m_isUnfolded)
       integrateModelFluxes(groupSpectra,chanEmin,chanEmax,gr,npts);

    // x axis and x bin width arrays. could be done earlier but better
    // to separate responsibilities.

    setXaxis(*m_settings,gr,firstChannelInBin,lastChannelInBin+1,chanEmin,chanEmax,npts);

  return true;
}

void CreateBinnedPlotGroups::integrateModelFluxes (std::list<SpectralData*>& spectra, Real eMin, Real eMax, PlotGroup* group, int point)
{
   const size_t iXsPlotGroup = (*spectra.begin())->plotGroup();
   // modelSum not be used unless there is more than 1 model in group
   std::vector<Real>& modelSum = group->auxData[0].data;
   const std::map<size_t,size_t>& modAssignments =
         m_modelPositionDirectory[iXsPlotGroup-1];
   const size_t compOffset = group->model.size() > 1 ? 1 : 0;
   std::list<SpectralData*>::const_iterator gs    (spectra.begin());
   std::list<SpectralData*>::const_iterator gsEnd (spectra.end());     

   while (gs != gsEnd)
   {
      size_t spectrumNumber = (*gs)->spectrumNumber();
      std::vector<const Model*>::const_iterator itMod = 
           m_modelsForSpectra[spectrumNumber-1].begin();
      std::vector<const Model*>::const_iterator itModEnd = 
           m_modelsForSpectra[spectrumNumber-1].end();
      while (itMod != itModEnd)
      {
         const Model* m = *itMod;
         const size_t sourceIndex = 
                   modAssignments.find(m->sourceNumber())->second;
         const ArrayContainer& flux = m->modelFlux();
         if ( flux.find(spectrumNumber) != flux.end())
         {
            Real energyFlux(0);
            Real ergFlux(0);
            m->integrateFlux(spectrumNumber,eMin,eMax,energyFlux,ergFlux);
            Real width(1.);
            const bool TOO_SMALL = std::abs((eMax - eMin)/eMax) < 1.E-15;
            if (!TOO_SMALL)
            {
               switch (m_settings->xOption())
               {
                  case CHANNELS:
                  case ENERGY:
                  default:
                  {
                     Real unitFactor = m_settings->getUnitFactor(ENERGY);
                     width /= (eMax - eMin)*unitFactor;
                  }
                  break;          
                  case WAVELENGTH:
                     if (m_settings->isWavePerHz())
                        width /= Numerics::KEVTOHZ*(eMax - eMin);
                     else
                     {
                        Real unitFactor = m_settings->getUnitFactor(WAVELENGTH);
                        width /= Numerics::KEVTOA*(1./eMin - 1./eMax)*unitFactor;
                     }
                  break;
               }
               Real integratedFlux = energyFlux*width;
               group->auxData[sourceIndex].data[point] = integratedFlux;
               if (sourceIndex)
               {
                  // Index is not 0, so we have multiple models.  Add this to sum.
                  modelSum[point] += integratedFlux;
               }
            }
            else
            {
               // If 1 model gets in here, all of them will.
               group->auxData[sourceIndex].data[point] 
                                    =  NODATA();
               if (sourceIndex)
                  modelSum[point] = NODATA();
            }

            // compute the integrals also for the source components, if there are any.
            if (!group->sources[sourceIndex-compOffset].empty() )
            {
                std::list< std::pair<Real,Real> > fluxes 
                    = m->integrateSourceFlux(spectrumNumber,eMin,eMax); 
                std::list<std::pair<Real, Real> >::const_iterator fl = fluxes.begin();
                std::list<std::pair<Real, Real> >::const_iterator flEnd = fluxes.end();
                PlotVectorList::iterator sfl = group->sources[sourceIndex-compOffset].begin();
                if (!TOO_SMALL)
                {                            
                   while ( fl != flEnd )
                   {
                      sfl->data[point] = (fl->first)*width;
                      ++fl, ++sfl;       
                   }
                }
                else
                {
                   while ( fl != flEnd )
                   {
                      sfl->data[point] = NODATA();
                      ++fl, ++sfl;       
                   }                                        
                }
            }

         } // end if flux array found
         ++itMod;
      }                                                            
      ++gs;
   }
}

void CreateBinnedPlotGroups::setError (PlotGroup* gr, int npts, Real areaSum, Real backAreaSum, 
        Real count, Real backgroundCount, Real bscaleRatio, bool allUsingChiSquare)
{
  Real& error = gr->yData.errors[0][npts];
  Real& backError = gr->background.errors[0][npts];
  switch (gr->errorType)
  {
     case PlotSettings::ROOTN:
             error = sqrt(count + backgroundCount);
             backError = sqrt(backgroundCount);
             if (!m_counts) 
             {
                error /= areaSum;
                backError /= areaSum;
             }
             break;
     case PlotSettings::GEHRELS1:
             error = (1 + sqrt(0.75+count))*(1 + sqrt(0.75+count));
             if ( backgroundCount != 0) 
             {
                backError = (1 + sqrt(0.75+backgroundCount))*(1 + sqrt(0.75+backgroundCount));      
                error += backError;
             }
             else 
                backError = 0.0;
             if (!m_counts)
             {
                error /= areaSum;
                backError /= areaSum; 
             }               
             break;
     case PlotSettings::GEHRELS2:
             error = count - 0.25;
             if ( backgroundCount != 0)
             {
                backError = backgroundCount - 0.25;
                error += backError;
             }
             else
                backError = 0.0;
             error = sqrt(error);
             backError = sqrt(backError);
             if (!m_counts)
             {
                error /= areaSum; 
                backError /= areaSum;
             }   
             break;
     case PlotSettings::GEHRELSM:
             error = ((1 + sqrt(count+0.75) + sqrt(count - 0.25))*0.5)*
                       ((1 + sqrt(count+0.75) + sqrt(count - 0.25))*0.5);
             if ( backgroundCount != 0)
             {
                backError = ((1 + sqrt(backgroundCount + 0.75) 
                                + sqrt(backgroundCount - 0.25))*0.5)*
                         ((1 + sqrt(backgroundCount + 0.75) 
                                + sqrt(backgroundCount - 0.25))*0.5);
                error += backError;                                               
             }
             else
                backError = 0.0;  
             error = sqrt(error);
             backError = sqrt(backError);
             if (!m_counts)
             {
                error /= areaSum; 
                backError /= areaSum;
             }   
             break;          
     case PlotSettings::STD:
     default:
             error = sqrt(error);
             backError = sqrt(backError);
             break;

  }

  // If chisq is the current statistic, plot chisq and delchi can't
  // have zero errors.
  if (allUsingChiSquare)
  {
     if (error == 0.0)
     {
         error = ChiSquare::calcMinVariance(areaSum, backAreaSum, bscaleRatio);
         error = sqrt(error);
         if (m_counts)
            error *= areaSum;
     }
  }
}

void CreateBinnedPlotGroups::initEffectiveAreas (std::list<SpectralData*>& spectraInGroup)
{
   std::list<SpectralData*>::iterator it = spectraInGroup.begin();
   std::list<SpectralData*>::iterator itEnd = spectraInGroup.end();
   while (it != itEnd)
   {
      (*it)->calcEffectiveAreas();
      ++it;
   }
}

void CreateBinnedPlotGroups::clearEffectiveAreas (std::list<SpectralData*>& spectraInGroup)
{
   std::list<SpectralData*>::iterator it = spectraInGroup.begin();
   std::list<SpectralData*>::iterator itEnd = spectraInGroup.end();
   while (it != itEnd)
   {
      (*it)->clearEffectiveAreas();
      ++it;
   }
}

// Additional Declarations
