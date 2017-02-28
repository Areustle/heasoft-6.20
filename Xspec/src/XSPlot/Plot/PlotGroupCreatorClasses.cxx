//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSContainer.h>
#include <XSstreams.h>
#include <XSFit/Fit/Fit.h>
#include <XSFit/MCMC/Chain.h>
#include <XSFit/MCMC/ChainManager.h>
#include <XSModel/Data/Detector/Response.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/GlobalContainer/ModelTypes.h>
#include <XSModel/Model/Model.h>
#include <XSModel/Model/Component/SumComponent.h>
#include <XSPlot/Plot/PlotGroup.h>
#include <XSUtil/Error/Error.h>
#include <XSUtil/FunctionUtils/FunctionUtility.h>
#include <memory>
#include <sstream>
#include <utility>
#include <cmath>

// PlotGroupCreatorClasses
#include <XSPlot/Plot/PlotGroupCreatorClasses.h>
using namespace XSContainer;


// Class CreateModelPlotGroups 

CreateModelPlotGroups::CreateModelPlotGroups()
  :PlotGroupCreator(),
   m_modNames()
{
}


CreateModelPlotGroups::~CreateModelPlotGroups()
{
}


std::vector<PlotGroup*> CreateModelPlotGroups::createPlotGroups (const PlotSettings& settings)
{

   string mapName(m_modNames.front());
   m_modNames.pop();
   if ( !mapName.size() ) mapName = Model::DEFAULT();

   if (models->modelSet().empty())
   {
      throw YellowAlert(" no model defined\n");
   }
   std::pair<ModelMapConstIter, ModelMapConstIter> itRange = models->modelSet().equal_range(mapName);

   if (itRange.first == itRange.second)
   {
      string msg(" model not found\n");
      if (mapName == Model::DEFAULT())
      {
         msg += "No model has been entered with the default name\n";
      }
      else
      {
         msg += "No model has been entered with name = " + mapName + "\n";
      }
      throw YellowAlert(msg);
   }  

   if (datasets->numSourcesForSpectra() && itRange.first->second->sourceNumber() > datasets->numSourcesForSpectra())
   {
      string msg("Cannot plot model of higher source number than data allows.\n");
      throw YellowAlert(msg);
   }

   std::vector<PlotGroup*> plotGroups;
   ModelMapConstIter itMod (itRange.first);

   try
   {
      while (itMod  != itRange.second )
      {
         const Model* mod = itMod->second;
         const ArrayContainer& energies = mod->energy();
         ArrayContainer::const_iterator itEn = energies.begin();
         ArrayContainer::const_iterator itEnEnd = energies.end();
         while (itEn != itEnEnd)
         {
            int specNum = itEn->first;
            const RealArray& eng = itEn->second;
            PlotGroup* gr = initializePlotGroup(mod, specNum, eng, settings);
            plotGroups.push_back(gr);
            ++itEn;
         }
         ++itMod;
      }
   }
   catch (...)
   {
      for (size_t i=0; i<plotGroups.size(); ++i)
         delete plotGroups[i];
      throw;
   }

   return plotGroups;
}

PlotGroup* CreateModelPlotGroups::initializePlotGroup (const Model* model, int specNum, const RealArray& energy, const PlotSettings& settings)
{
  const int NE (energy.size() - 1); 

  std::auto_ptr<PlotGroup> apGr(new PlotGroup(NE,1,false)); 
  PlotGroup* gr = apGr.get();

  // set the x axis array.

  for (int j = 0; j < NE; ++j)
  {
     // dummies are only used for channel mode, which has to be
     // switched off.
     int dummy(0);
     const Real chanEmin = energy[j]*(1.0 + settings.redshiftToSource());
     const Real chanEmax = energy[j+1]*(1.0 + settings.redshiftToSource());
     setXaxis(settings,gr,dummy,dummy,chanEmin,chanEmax,j);      
  }      

  const RealArray& flux = model->modelFlux(specNum);
  const RealArray& fluxErr = model->modelFluxError(specNum);

  gr->model[0].data.resize(NE,0);
  for ( int j = 0; j < NE; ++j)
  {
     gr->model[0].data[j] = flux[j];
  }
  if ( fluxErr.size())
  {
     gr->model[0].errors.push_back(std::vector<Real>(NE));
     for ( int j = 0; j < NE; ++j)
     {
        gr->model[0].errors[0][j] = fluxErr[j];
     }     
  }
  else 
  {
     gr->model[0].errors.push_back(std::vector<Real>());             
  }

  // if sources are present and required, put them in the right 
  // place too.

  const SourceList& sources = model->sources();
  size_t numberOfSourcesInModel(sources.size()); 
  if (numberOfSourcesInModel > 1)
  {
     const size_t& n = gr->n;
     PlotVectorList& currentSources = gr->sources[0];

     SourceList::const_iterator sl = sources.begin();    

     for (size_t j = 0; j < numberOfSourcesInModel; ++j)
     {
         currentSources.push_back(PlotVector(n));
         PlotVector& cs = currentSources.back();
         const SumComponent* c = *sl;
         for (size_t k = 0; k < n; ++k)
         {
            cs.data[k]  = c->photonArray(specNum)[k];       
         }
         ++sl;
     }
  }

  return apGr.release();
}

// Additional Declarations

// Class CreateDemPlotGroups 

CreateDemPlotGroups::CreateDemPlotGroups()
  : PlotGroupCreator()
{
}


CreateDemPlotGroups::~CreateDemPlotGroups()
{
}


std::vector<PlotGroup*> CreateDemPlotGroups::createPlotGroups (const PlotSettings& settings)
{
   int nPts = static_cast<int>(FunctionUtility::DEM().size());

   //did demSize come back with a size of 0? Could indicate model 
   //wasn't calculated properly, or at all. Force a calculation
   if(!nPts)
   {
       ModelMap::const_iterator 
	   itModSet = models->modelSet().begin(),
	   itModSetEnd = models->modelSet().end();

       while(itModSet != itModSetEnd)
       {
	   models->calculate(itModSet->first);
	   ++itModSet;
       }
       nPts = static_cast<int>(FunctionUtility::DEM().size());

       //if it's still 0, them we have an unrecoverable error, quit
       if(!nPts)
	   throw YellowAlert("Unable to do dem plot: dem size is 0\n");
   }

   PlotGroup* newGroup = new PlotGroup(nPts);

   for(int i = 0; i < nPts; ++i) 
   {
       newGroup->xAxis.data[i] = FunctionUtility::tempsDEM()[i];
       newGroup->yData.data[i] = FunctionUtility::DEM()[i];
   }
   newGroup->xAxis.errors.resize(0);
   newGroup->yData.errors.resize(0);

   std::vector<PlotGroup*> plotGroups(1,newGroup);

   return plotGroups;
}

// Class CreateEqwPlotGroups 

CreateEqwPlotGroups::CreateEqwPlotGroups()
  : PlotGroupCreator()
{
}


CreateEqwPlotGroups::~CreateEqwPlotGroups()
{
}


std::vector<PlotGroup*> CreateEqwPlotGroups::createPlotGroups (const PlotSettings& settings)
{

  std::vector<PlotGroup*> plotGroups;
  const size_t nSpec = datasets->numberOfSpectra();

  for (size_t iSpec=1; iSpec<=nSpec; iSpec++) {

    PlotGroup* newGroup = 0;
    SpectralData* sd = datasets->lookup(iSpec);
    const SpectralData::FluxCalc& eqwidth = sd->lastEqWidthCalc();
    size_t nPts = eqwidth.errorTrialVals.size();

    if(!nPts)
      throw YellowAlert("Unable to plot eqw: you need to have done an eqw command with the err argument\n");

    // set up histogram data. At present assumed 20 bins between min and max. Note that
    // the errorTrialVals array is in increasing order.

    size_t N(20);
    RealArray ranges(N+1);
    RealArray histo(N);

    Real eqwBinSize = (eqwidth.errorTrialVals[nPts-1]-eqwidth.errorTrialVals[0])/((Real)N);
    for (size_t i=0; i<=N; i++) ranges[i] = eqwidth.errorTrialVals[0] + i*eqwBinSize;
    for (size_t i=0; i<N; i++) histo[i] = 0.0;

    size_t irange = 1;
    for (size_t i=0; i<nPts; i++) {
      while ( eqwidth.errorTrialVals[i] > ranges[irange] && irange < N ) irange++;
      histo[irange-1]++;
    }

    newGroup = new PlotGroup(N);

    for(size_t i = 0; i < N; i++) {
      newGroup->xAxis.data[i] = ranges[i]+0.5*eqwBinSize;
      newGroup->xAxis.errors[0][i] = 0.5*eqwBinSize;
      newGroup->yData.data[i] = histo[i]/nPts/eqwBinSize;
    }
    newGroup->yData.errors.resize(0);

    plotGroups.push_back(newGroup);

  }

  return plotGroups;
}


// Additional Declarations

// Class CreateEfficiencyPlotGroups 

CreateEfficiencyPlotGroups::CreateEfficiencyPlotGroups()
  : PlotGroupCreator()
{
}


CreateEfficiencyPlotGroups::~CreateEfficiencyPlotGroups()
{
}


std::vector<PlotGroup*> CreateEfficiencyPlotGroups::createPlotGroups (const PlotSettings& settings)
{
   std::vector<PlotGroup*> plotGroups;
   const size_t nSpec = datasets->numberOfSpectra();

   try
   {
      for (size_t iSpec=1; iSpec<=nSpec; ++iSpec)
      {
         PlotGroup* gr=0;
         SpectralData* spectrum = datasets->lookup(iSpec);
         const std::vector<Response*>& dets = spectrum->detector();
         bool anyDets = false;
         for (size_t iDet=0; iDet<dets.size(); ++iDet)
         {
            const Response* resp = dets[iDet];
            if (resp)
            {
               anyDets = true;
               const RealArray& energy = resp->energies();
               const size_t NE (energy.size() - 1);
               gr = new PlotGroup(NE);
               plotGroups.push_back(gr);

               for (size_t j = 0; j < NE; ++j)
               {
                     // dummies are only used for channel mode, which has to be
                     // switched off.
                     int dummy(0);
                     setXaxis(settings,gr,dummy,dummy,energy[j],energy[j+1],j);      
               }                     
            }
         } // end dets loop

         if (!anyDets)
         {
               // One could put in a dummy response here, but for the uses
               // this function is likely to be put to the plot won't be very
               // interesting (e.g. detector efficiency).  Still continue
               // loop though, don't throw.
               std::ostringstream ss;
               ss << "\n***XSPEC Error: No energies defined for spectrum " 
                  << iSpec << "\n";
               tcerr << ss.str() << std::endl;
         }

      } // end spectra loop
   }
   catch (...)
   {
      for (size_t i=0; i<plotGroups.size(); ++i)
         delete plotGroups[i];
      throw;
   }

   if (plotGroups.empty())
     throw YellowAlert("No response energies for any loaded spectra.\n");

   return plotGroups;
}

// Additional Declarations

// Class CreateChainPlotGroups 

CreateChainPlotGroups::CreateChainPlotGroups()
  : PlotGroupCreator(),
   m_paneIndex(0),
   m_colNums(),
   m_nSkip()
{
}


CreateChainPlotGroups::~CreateChainPlotGroups()
{
}


std::vector<PlotGroup*> CreateChainPlotGroups::createPlotGroups (const PlotSettings& settings)
{
   size_t totalChainLength = 0;
   const size_t xColNum = m_colNums[m_paneIndex].first;
   const size_t yColNum = m_colNums[m_paneIndex].second;
   
   ChainManager* chainManager = ChainManager::Instance();

   //more efficient to get the total number of chains and allocate 
   //memory once in PlotGroup rather than growing it as necessary
   ChainManager::ChainContainer::const_iterator
       itChain = chainManager->chains().begin(),
       itChainEnd = chainManager->chains().end();

   while(itChain != itChainEnd)
   {
       totalChainLength += itChain->second->length();
       ++itChain;
   }
   const size_t nTotalPoints = 
      static_cast<size_t>(ceil(static_cast<double>(totalChainLength)/m_nSkip[m_paneIndex]));
   std::auto_ptr<PlotGroup> apGr(new PlotGroup(nTotalPoints));
   PlotGroup* chainGroup = apGr.get();
   chainGroup->xAxis.errors.clear();
   chainGroup->yData.errors.clear();

   //read points from chain file and fill new PlotGroup	
   std::vector<Real> row;
   size_t iChain=0;
   size_t iPlot=0;
   itChain = chainManager->chains().begin();
   while (itChain != itChainEnd)
   {
      const Chain& chain = *(itChain->second);
      chain.openForReadPoints();
      const size_t chainLength = chain.length();
      const size_t nSkip = m_nSkip[m_paneIndex];

      if (xColNum > 0)
      {
	 for(size_t i = 0; i < chainLength; ++i, ++iChain)
	 {
	     chain.readPoint(row);
             if (iChain % nSkip == 0)
             {
	        //user-supplied column nums are 1-based
	        chainGroup->xAxis.data[iPlot] = row[xColNum - 1];
	        chainGroup->yData.data[iPlot++] = row[yColNum - 1];
             }
	 }
      }
      else
      {
	 for(size_t i = 0; i < chainLength; ++i, ++iChain)
	 {
	     chain.readPoint(row);
             if (iChain % nSkip == 0)
             {
	        chainGroup->xAxis.data[iPlot] = iChain+1;
	        chainGroup->yData.data[iPlot++] = row[yColNum - 1];
             }
	 }
      }

      chain.closeFile();
      ++itChain;
   }
   std::vector<PlotGroup*> plotGroups;
   plotGroups.push_back(apGr.release());   
   return plotGroups;
}

// Class CreateGoodnessPlotGroups 

CreateGoodnessPlotGroups::CreateGoodnessPlotGroups()
  : PlotGroupCreator()
{
}


CreateGoodnessPlotGroups::~CreateGoodnessPlotGroups()
{
}


std::vector<PlotGroup*> CreateGoodnessPlotGroups::createPlotGroups (const PlotSettings& settings)
{
  RealArray goodness = Fit::goodnessSims();
  size_t nGood = goodness.size();

  if (!nGood) throw YellowAlert("Unable to do goodness plot. You must use the goodness command first\n");

  // set up histogram data. At present assume 20 bins between min and max. Note that the
  // goodness array is in increasing order.

  size_t N(20);

  RealArray ranges(N+1);
  RealArray histo(N);

  Real goodBinSize = (goodness[nGood-1]-goodness[0])/((Real)N);
  for (size_t i=0; i<=N; i++) ranges[i] = goodness[0] + i*goodBinSize;
  for (size_t i=0; i<N; i++) histo[i] = 0.0;

  size_t irange = 1;
  for (size_t i=0; i<nGood; i++) {
    while ( goodness[i] > ranges[irange] && irange < N ) irange++;
    histo[irange-1]++;
  }

  PlotGroup* newGroup = new PlotGroup(N);

  for(size_t i = 0; i < N; i++) {
    newGroup->xAxis.data[i] = ranges[i]+0.5*goodBinSize;
    newGroup->xAxis.errors[0][i] = 0.5*goodBinSize;
    newGroup->yData.data[i] = histo[i]/nGood/goodBinSize;
  }
  newGroup->yData.errors.resize(0);

  std::vector<PlotGroup*> plotGroups(1,newGroup);

  return plotGroups;
}

// Additional Declarations
