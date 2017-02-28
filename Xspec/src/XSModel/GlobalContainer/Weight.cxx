//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <xsTypes.h>

// SpectralData
#include <XSModel/Data/SpectralData.h>
// Model
#include <XSModel/Model/Model.h>
// Weight
#include <XSModel/GlobalContainer/Weight.h>

#include <XSModel/Data/BackCorr/Background.h>
#include <XSModel/Data/Detector/Response.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSContainer.h>
#include <XSsymbol.h>
#include <sstream>

namespace XSContainer {
        Weight::~Weight()
        {
                // body for abstract dtor. 
        }

    // Class XSContainer::Weight::ArrayInitializationError 

    Weight::ArrayInitializationError::ArrayInitializationError (size_t num)
        : RedAlert(" initializing statistic workspace arrays:\n ")  
    {
        std::ostringstream message;
        message << " Statistic calculation found spectrum number " << num << " array incorrect size ";
        reportAndExit(message.str(),-5);
    }


    // Class XSContainer::Weight 


    Weight::Weight (const string& name)
      : m_name(name)
    {
    }


    void Weight::dataVariance (const Model& model, ArrayContainer& variance) const
    {

       // add model systematic error estimate to the variance.

       // With multi-StatMethod enhancement, we can no longer assume
       // the every folded model spectrum has a counterpart in the
       // variance container.  The variance container contains only
       // those spectra using a particular StatMethod.
       Real msyst = XSContainer::models->modelSystematicError();
       ArrayContainer::const_iterator itFoldedEnd = model.foldedModel().end();
       ArrayContainer::iterator itVar = variance.begin();
       ArrayContainer::iterator itVarEnd = variance.end();
       while (itVar != itVarEnd)
       {
          const size_t index = itVar->first;
          ArrayContainer::const_iterator itFolded = model.foldedModel().find(index);
          if (itFolded != itFoldedEnd)
          {
             const SpectralData* s = model.spectrum(index);
             const std::valarray<size_t>& IN = s->indirectNotice();
             RealArray& varArray = itVar->second;
             if (varArray.size() != IN.size()) 
             {
                throw ArrayInitializationError(index);
             }        
             if (!model.foldedModelError().empty() && model.foldedModelError(index).size())
             {
                varArray += model.foldedModelError(index)[IN]; 
             }
             if (msyst > SMALL)
             {
                RealArray f (model.foldedModel(index)[IN]);
                varArray += msyst*msyst*f*f;       
             }
          }
          ++itVar;
       }
    }

    void Weight::varianceDeriv (const Model& model, ArrayContainer& dv) const
    {
       Real msyst = XSContainer::models->modelSystematicError();
       if ( msyst >= SMALL)
       {
          std::map<size_t,Response*>::const_iterator ma = model.detector().begin();
          std::map<size_t,Response*>::const_iterator maEnd = model.detector().end();
          for ( ; ma != maEnd ; ++ma)
          {
             size_t index = ma->first;
             SpectralData* s = model.spectrum(index);
             const std::valarray<size_t>& IN = s->indirectNotice();
             RealArray& varDeriv = dv[index];
             if ( varDeriv.size() != IN.size()) 
             {
                varDeriv.resize(IN.size(),0.);
             }        
             varDeriv += msyst*msyst*model.foldedModel(index)[IN];
          }
       }
    }

    // Class XSContainer::StandardWeight 

    StandardWeight::StandardWeight ()
      : Weight("standard")
    {
    }


    StandardWeight::~StandardWeight()
    {
    }


    void StandardWeight::operator () (SpectralData& spectrum, const RealArray& norm) const
    {
       spectrum.setVariance(spectrum.rawVariance());
    }

    void StandardWeight::dataVariance (const Model& model, ArrayContainer& variance) const
    {
       // do nothing unless systematics are required.      

       Weight::dataVariance(model,variance);
    }

    StandardWeight* StandardWeight::clone () const
    {

      return new StandardWeight(*this);
    }

    // Class XSContainer::ModelWeight 

    ModelWeight::ModelWeight ()
      : Weight("model")
    {
    }


    ModelWeight::~ModelWeight()
    {
    }


    void ModelWeight::dataVariance (const Model& model, ArrayContainer& variance) const
    {
       Weight::dataVariance(model,variance);

       // add model systematic error estimate to the variance.
       ArrayContainer::const_iterator itFoldedEnd = model.foldedModel().end();
       ArrayContainer::iterator itVar = variance.begin();
       ArrayContainer::iterator itVarEnd = variance.end();
       while (itVar != itVarEnd)
       {
          const size_t index = itVar->first;
          ArrayContainer::const_iterator itFolded = model.foldedModel().find(index);
          if (itFolded != itFoldedEnd)
          {
             const SpectralData* s = model.spectrum(index);
             const std::valarray<size_t>& IN = s->indirectNotice();
             if (itVar->second.size() != IN.size()) 
             {
                throw ArrayInitializationError(index);
             }
             RealArray areaT = s->areaScale()[IN]*s->exposureTime();
             RealArray f = itFolded->second[IN];

             itVar->second += f / areaT;
             if (s->background())
             {
                const RealArray& backArray = s->background()->data()->spectrum();
                itVar->second += backArray / areaT;
             }
          }
          ++itVar;
       }
    }

    void ModelWeight::varianceDeriv (const Model& model, ArrayContainer& dv) const
    {
       Weight::varianceDeriv(model,dv);

       std::map<size_t,Response*>::const_iterator i = model.detector().begin();
       std::map<size_t,Response*>::const_iterator iEnd = model.detector().end();

       for ( ; i != iEnd ; ++i)
       {
          size_t index = i->first;
          SpectralData* s  = model.spectrum(index);
          const std::valarray<size_t>& IN = s->indirectNotice();
          RealArray& varDeriv = dv[index];
          if (varDeriv.size() != IN.size())
          {
             varDeriv.resize(IN.size());       
          }       
          varDeriv += 1./(2.*s->areaScale()[IN]*s->exposureTime());
       }
    }

    void ModelWeight::operator () (SpectralData& spectrum, const RealArray& norm) const
    {
       // Treat as standard weighting here.  Model values will be applied
       // during fitting stage.
       spectrum.setVariance(spectrum.rawVariance());
    }

    ModelWeight* ModelWeight::clone () const
    {

      return new ModelWeight(*this);
    }

    // Class XSContainer::GehrelsWeight 

    GehrelsWeight::GehrelsWeight ()
      : Weight("gehrels")
    {
    }


    GehrelsWeight::~GehrelsWeight()
    {
    }


    void GehrelsWeight::operator () (SpectralData& spectrum, const RealArray& norm) const
    {
        const RealArray& rate = spectrum.spectrum();
        size_t n = rate.size();
        RealArray gwVar(0.,n);
        static const Real OFFSET(0.75);

        // checking for zeros in exposure and area is performed by ]
        // DataSet::setDescription.

        // Doing these valarray arithmetic operations in a more tortured
        // manner to get around g++4.0.x runtime crashes with -O2 optimization.
        for (size_t i=0; i<n; ++i)
        {
           Real norm_i = norm[i];
           gwVar[i] = (sqrt(norm_i*rate[i] + OFFSET) + 1.0)/norm_i;
        }
        gwVar *= gwVar;
        spectrum.setVariance(gwVar);
    }

    void GehrelsWeight::dataVariance (const Model& model, ArrayContainer& variance) const
    {
  return Weight::dataVariance(model,variance);
    }

    GehrelsWeight* GehrelsWeight::clone () const
    {

      return new GehrelsWeight(*this);
    }

    // Class XSContainer::ChurazovWeight 

    ChurazovWeight::ChurazovWeight ()
      : Weight("churazov")
    {
    }


    ChurazovWeight::~ChurazovWeight()
    {
    }


    void ChurazovWeight::operator () (SpectralData& spectrum, const RealArray& norm) const
    {

        const RealArray& rate = spectrum.spectrum();
        int n = rate.size();
        RealArray cwVar(0.,n);
        static const Real MAXCOUNTS = 20.;
        for (int l = 0; l < n; ++l)
        {
                Real current = rate[l]*norm[l];
                bool terminate = current >= MAXCOUNTS;
                int lower = l;
                int upper = l;
                size_t bins  = 1;
                while (!terminate)
                {
                        --lower;       
                        ++upper;
                        if (lower >=0 )
                        {
                                ++bins;
                                current += rate[lower]*norm[lower];      
                        }
                        if (upper < n)
                        {
                                ++bins;
                                current += rate[upper]*norm[upper];
                        }
                        terminate = ( (lower < 0 && upper >= n) || current >= MAXCOUNTS);
                }

                cwVar[l] = current / (bins*norm[l]*norm[l]);
        }

        spectrum.setVariance(cwVar);

    }

    void ChurazovWeight::dataVariance (const Model& model, ArrayContainer& variance) const
    {
  return Weight::dataVariance(model,variance);
    }

    ChurazovWeight* ChurazovWeight::clone () const
    {

      return new ChurazovWeight(*this);
    }

} // namespace XSContainer
