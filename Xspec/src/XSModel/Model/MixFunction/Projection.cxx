//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// DataContainer
#include <XSModel/GlobalContainer/DataContainer.h>
// Projection
#include "Projection.h"

using namespace XSContainer;
#include <XSModel/Data/SpectralData.h>
#include <XSUtil/Numerics/LinearInterp.h>
#include <cmath>
#include <XSContainer.h>
#include <XSstreams.h>


// Class Projection::Ellipsoid 
Real Projection::Ellipsoid::FLAG = 999.;

Projection::Ellipsoid::Ellipsoid (Real maj, Real min, Real orient)
      : a(maj),
        b(min),
        orientation(orient),
        innerShellNum(-1)
{
}


int Projection::Ellipsoid::operator==(const Projection::Ellipsoid &right) const
{
  // for ellipsoids, equality is defined only up to major, minor axes and
  // orientation. sections can be different.
  return   (a == right.a) && (b == right.b) && (orientation == right.orientation);  
}

int Projection::Ellipsoid::operator!=(const Projection::Ellipsoid &right) const
{
  return !operator==(right);
}


// Class Projection 
const Real Projection::DEG2RAD = std::atan(1.)/45.;
const Real Projection::FOURPIBY3 = 16.*std::atan(1.)/3.;

Projection::Projection (const string& name)
  : MixUtility(name)
{
}


Projection::~Projection()
{
}


void Projection::initialize (const std::vector<Real>& params, const IntegerArray& spectrumNums, const std::string& modelName)
{
  setSpecNums(spectrumNums);
  verifyData();

  m_shell.resize(m_observations);
  // If a center ellipsoid is found to be in use in initializeForFit
  //  (as indicated by parameter values), these m_shell vectors
  //  will be expanded by size 1 to accommodate it.
  for (size_t iObs=0; iObs<m_observations; ++iObs)
     m_shell[iObs].resize(m_numberOfShells);
          
  // If the observation 1 files are o1a1, o1a2, o1a3 and the 
  // observation 2 files are o2a1, o2a2, o2a3 then they should
  // be read in using: 
  // XSPEC> data 1:1 o1a1 1:2 o2a1 2:3 o1a2 2:4 o2a2 3:5 o1a3 3:6 o2a3

  // ...but, since we're now allowing interspersed spectra that don't use
  // this model, we can no longer so easily obtain the obs number from
  // the spectrum number.  We CAN still assume obs number increases
  // consecutively within each data group for the spectra that ARE using
  // this model, so keep track of things with a counter for each 
  // data group (or annulus).
  std::vector<size_t> obsCounter(m_numberOfShells, 0);
  for (size_t iSpec=0; iSpec<specNums().size(); ++iSpec)
  {
     const SpectralData* spec = datasets->lookup(specNums()[iSpec]);
     size_t groupNum = spec->parent()->dataGroup();
     std::map<size_t,size_t>::const_iterator itOrder = m_dgOrder.find(groupNum);
     if (itOrder == m_dgOrder.end())
        throw RedAlert("Data group indexing error in Projection::initialize");
     size_t groupPosNum = itOrder->second;
     size_t iObs = obsCounter[groupPosNum]++;
     createEllipsoidDescriptions(iObs, specNums()[iSpec], groupPosNum);
  }
} 
// end initialize

void Projection::initializeForFit (const std::vector<Real>& params, bool paramsAreFrozen)
{
  bool isCentre = (params[0] > 0 && params[1] > 0);
  if (isCentre)
  {
     if (!m_includesCentreShell)
     { 
        ++m_numberOfShells;
        for (size_t iObs=0; iObs<m_observations; ++iObs)
           m_shell[iObs].resize(m_numberOfShells);
        m_includesCentreShell = true;
     }
     for (size_t iObs=0; iObs<m_observations; ++iObs)
     {
        Ellipsoid& centre = m_shell[iObs][m_numberOfShells-1];
        centre.a = params[0];
        centre.b = params[1];
        centre.orientation = params[2];         
        centre.angleBound.resize(1);
        centre.angleBound[0].first = 0;
        centre.angleBound[0].second = 360.;
     }
  }
  else
  {
     if (m_includesCentreShell)
     {
        --m_numberOfShells;
        for (size_t iObs=0; iObs<m_observations; ++iObs)
           m_shell[iObs].resize(m_numberOfShells);
        m_includesCentreShell = false;
     }
  }

  orderEllipsoids(isCentre);

  if ( changedProjection() )
  {      
     m_matrix.resize(m_observations);
     for (size_t observation = 0; observation < m_observations; ++observation)
     {
        m_matrix[observation].assign(m_numberOfShells*m_numberOfShells,0.);
        defineMatrix(observation);
     }        
  }        
  m_previousShell = m_shell;
} 
// end initializeForFit

Real Projection::element (size_t observation, int shellNumber, int ellipseNumber, int sectionNumber) const
{
  Real result(0);
  static const size_t NSTEPS (5000);
  static const Real twoThirds (2./3.);
// Calculate the fraction of the volume of the prolate ellipsoid with axes/orientation
// defined by m_shell[shellNumber] that lies within the projected ellipse with
// axes/orientation defined by m_shell[ellipseNumber] with sections defined by the
// m_shell angleBound vector pair.   [ m_shell is of type vector<Ellipsoid> ]


// Integral to be calculated is 
//
//  V = 2/3 Int_(ph1-theta)^(ph2-theta) dx b^3 (1 + (b^2/a^2-1) cos^2 x)^-1
//
//    - 2/3 Int_(ph1-theta)^(ph2-theta) dx beta^3 
//
//             (1 + (beta^2/alpha^2-1) cos^2 (x+theta-phi))^-3/2
//
//            ((b^2/beta^2)(1 + (beta^2/alpha^2-1) cos^2 (x+theta-phi)) - 1
//
//            - (b^2/a^2-1) cos^2 x)^3/2 (1 + (b^2/a^2-1) cos^2 x)^-1
//
//  where a,b,theta describe the ellipsoid and alpha,beta,phi,ph1,ph2 the 
//  2-D ellipse.

     const std::vector<Ellipsoid>& shell = m_shell[observation];


     const Real& shellMinor = shell[shellNumber].b;
     const Real& shellMajor = shell[shellNumber].a;
     const Real& shellOrient = shell[shellNumber].orientation;
     const Real& annulusMinor = shell[ellipseNumber].b;
     const Real& annulusMajor = shell[ellipseNumber].a;
     const Real& annulusOrient = shell[ellipseNumber].orientation;
     const std::vector<std::pair<Real,Real> >& sections  = shell[sectionNumber].angleBound;

     const size_t nSects (sections.size());

     Real shMin3 (shellMinor*shellMinor*shellMinor);
     Real annMin3 (annulusMinor*annulusMinor*annulusMinor);
     Real shellAnn2    = (shellMinor/annulusMinor)*(shellMinor/annulusMinor);
     Real shellRat2minus1  = (shellMinor/shellMajor)*(shellMinor/shellMajor) - 1.;
     Real annRat2minus1    = (annulusMinor/annulusMajor)*(annulusMinor/annulusMajor) - 1.;
     Real orientDiff   = ( shellOrient - annulusOrient )*DEG2RAD;

     // Loop round sections. There's always at least one...
     for ( size_t j = 0; j < nSects; ++j )
     {
        Real angle1 ( DEG2RAD*(sections[j].first  - shellOrient) );
        Real angle2 ( DEG2RAD*(sections[j].second - shellOrient) );
        Real dx  ( (angle2 - angle1)/ NSTEPS );
        Real Sum (0.);
        Real x (angle1 - 0.5*dx);
        // The integrand is smoothly varying so can use BFI and divide into NSTEP
        // steps from the start to end angles.

        for ( size_t i = 0; i < NSTEPS; ++i )
        {
           x += dx;
           Real cosx  (std::cos(x));
           Real cosx0 (std::cos(x + orientDiff));
           Real cosSqX  (cosx*cosx);
           Real cosSqXO (cosx0*cosx0);    
           Real f1 ( 1. +  shellRat2minus1*cosSqX );
           Real f2 ( 1. +    annRat2minus1*cosSqXO );
           if (  (shellAnn2*f2 - f1) > 0 )
           {
              Real RR ( (shellAnn2*f2 - f1)/f2 );
              Sum += shMin3/f1 - annMin3 * sqrt ( RR*RR*RR );
           }
           else
           {
              Sum += shMin3/f1;
           }
        }   
        result += twoThirds*Sum*dx;                        
     }

  return result;
}

void Projection::defineMatrix (size_t observation)
{

  // equivalent of calprj. returns a projection matrix for each observation.



  for (size_t j = 0; j < m_numberOfShells; ++j)
  {
     int Inshell (0);
// j is the target ellipse. Find the ellipse immediately interior to the 
// current one so that we can subtract off the contributions interior to the
// defined annulus...

     Real majorInShell = interiorVolume(observation, j, Inshell);

// loop around contributing shells. 

     for (size_t i = 0; i < m_numberOfShells; ++i )
     {
        std::vector<Ellipsoid>& shell = m_shell[observation];
        if ( shell[i].a >= shell[j].a )
        {
            Real el_ijj (element(observation,i,j,j));
            if ( majorInShell != 0) el_ijj -= element(observation,i,Inshell,j);
            setProjectionElement(observation,i,j,el_ijj);       
            if (tpout.maxChatter() >= 30)
            { 
               tcout << "matrix(i,j) @ obs " << i << " " << j << " " 
                     << observation  << " : " << el_ijj << std::endl;
            }
        }
     }
  }
}

void Projection::createEllipsoidDescriptions (size_t observation, size_t spectrumNumber, size_t iShell)
{

    // set the size of the Ellipsoid vector (m_shell) before entering this function.

    std::vector<Ellipsoid>& shell = m_shell[observation];

    // get the contents of the XFLT#### keywords describing the annulus from
    // which the spectrum was extracted. These use the keys major, minor, orient
    // for the compulsory filter keywords. For backward compatibility also try
    // values from XFLT0001, XFLT0002, and XFLT0003 which have the keys key1, key2
    // and key3. If none of these keywords are available then the model will have
    // thrown an error in testEllipsoid and will have not reached this point.

    SpectralData* spectrum (datasets->lookup(spectrumNumber));
    Ellipsoid& currShell = shell[iShell];

    if ( spectrum->inxflt("major") ) {
      currShell.a = spectrum->xflt("major");
    } else {
      currShell.a = spectrum->xflt(1);
    }

    if ( spectrum->inxflt("minor") ) {
      currShell.b = spectrum->xflt("minor");
    } else {
      currShell.b = spectrum->xflt(2);
    }

    if ( spectrum->inxflt("orient") ) {
      currShell.orientation = spectrum->xflt("orient");
    } else {
      currShell.orientation = spectrum->xflt(3);
    }

    // Now check for any angle boundaries on the annulus. These have keys
    // angle1l, angle1u, angle2l, angle2u etc. or alternatively key4, key5,
    // and so forth

    std::vector<Real> angles;

    size_t iangle = 0;
    while (true) {

      std::ostringstream astr;
      astr << "angle" << (iangle+1);

      if ( spectrum->inxflt(astr.str()+"l") && spectrum->inxflt(astr.str()+"u") ) {

	angles.push_back(spectrum->xflt(astr.str()+"l"));
	angles.push_back(spectrum->xflt(astr.str()+"u"));

      } else {

	size_t ikey = 4 + iangle*2;
	if ( spectrum->inxflt(ikey) && spectrum->inxflt(ikey+1) ) {

	  angles.push_back(spectrum->xflt(ikey));
	  angles.push_back(spectrum->xflt(ikey+1));

	} else {

	  if ( iangle == 0 ) {
	    currShell.angleBound.resize(1);
	    currShell.angleBound[0].first = 0.0;
	    currShell.angleBound[0].second = 360.0;
	  } else {
	    currShell.angleBound.resize(iangle);
	    for (size_t i=0; i<iangle; i++) {
	      currShell.angleBound[i].first = angles[i*2];
	      currShell.angleBound[i].second = angles[i*2+1];
	    }
	  } 
	  return;
	
	}

      }

      iangle++;
    }

}

Real Projection::interiorVolume (size_t observation, int ellipseNumber, int& shellNumber) const
{
  Real axisLength = 0.0;
  const std::vector<Ellipsoid>& shells = m_shell[observation];
  // sets the shell number of the volume interior to ellipseNumber and returns
  // its major axis length
  const Ellipsoid& shell = shells[ellipseNumber];
  shellNumber = shell.innerShellNum;
  if (shellNumber >= 0)
     axisLength = shells[shellNumber].a;

  return axisLength;
}

bool Projection::changedProjection () const
{
  bool changed(true);
  if (!m_shell.empty() && m_shell.size() ==  m_previousShell.size()) 
  {
     changed = false;
     if (m_shell[0].size() != m_previousShell[0].size())
        changed = true;
     for (size_t i = 0; i < m_numberOfShells && !changed; ++ i)
     {
        for ( size_t j = 0; j < m_observations && !changed; ++j)
        {                
           const std::vector<Ellipsoid>& shell = m_shell[j];        
           const std::vector<Ellipsoid>& previousShell = m_previousShell[j]; 

           if ( shell[i].a != previousShell[i].a ||  
                shell[i].b != previousShell[i].b ||      
                shell[i].orientation != previousShell[i].orientation ||
                shell[i].angleBound.size() !=  previousShell[i].angleBound.size() )
           {
              changed = true;
           } 
           else
           {
              size_t Nsects (shell[i].angleBound.size());
              for (size_t k = 0; k < Nsects; ++k)
              {
                 if (shell[i].angleBound[k].first 
                               != previousShell[i].angleBound[k].first ||
                     shell[i].angleBound[k].second 
                               != previousShell[i].angleBound[k].second )
                 {
                    changed = true;
                    break;
                 }            
              }
           }                                  
        }       
     }     
  }

  return changed;
}

Real Projection::projectionElement (size_t observation, int nshell, int mshell) const
{
  return m_matrix[observation][m_numberOfShells*mshell + nshell];
}

void Projection::setProjectionElement (size_t observation, int nshell, int mshell, Real value)
{
  m_matrix[observation][m_numberOfShells*mshell + nshell] = value;
}

void Projection::perform (const EnergyPointer& energy, const std::vector<Real>& params, GroupFluxContainer& flux, GroupFluxContainer& fluxError)
{

  // each array container in flux, containing one Real Array per spectrum being fitted,
  // has the same number of RealArrays.  The number of spectra in each ArrayContainer
  // is the number of observations. The model proceeds by transforming each observation
  // separately. In general, every array in an observation depends on all the others.

  // allocate scratch and initialize. The arrays in the input flux need to be 
  // preserved until the calculation is complete, and then be overwritten

  // the error arrays, used rarely, are not filled unless necessary.
  // since this is a marginal path the code is written to avoid pessimizing the 
  // most likely execution path.
  bool isError (!fluxError.empty());


  doPerform (energy, flux);

  if (isError) doPerform (energy,fluxError);

}

void Projection::verifyData ()
{
  if (!specNums().size())
  {
     string msg("Projct Model Error: No data assigned to projct, mixing will not be performed.");
     throw YellowAlert(msg);
  }
  m_dgOrder.clear();
  // Check that an equal # of spectra in each data group are using this
  // model. (This may be overly lenient but we're not requiring that
  // EVERY spectrum in the group has to use this.)
  // Check also that each SpectralData object in the group has identical XFLT
  // values.
  // Ellipsoid is initialized to Ellipsoid::FLAG on construction.
   static Ellipsoid flagValues;
   std::map<size_t,size_t> groupCount;
   std::map<size_t,Ellipsoid> ellipseTest;
   
   size_t count=0;
   for (size_t iSpec=0; iSpec<specNums().size(); ++iSpec)
   {
      const SpectralData* spec = datasets->lookup(specNums()[iSpec]);
      const size_t groupNum = spec->parent()->dataGroup();
      if (groupCount.find(groupNum) == groupCount.end())
      {
         groupCount[groupNum] = 0;
         ellipseTest[groupNum] = Ellipsoid();
      }
      groupCount[groupNum] += 1;
      // get the Ellipsoid definition of the first 
      // spectrum found in this group, and then test subsequent ellipsoids
      // against it.

      // the refactoring static function testEllipsoid can throw an
      // IncompatibleData exception.
      if (ellipseTest[groupNum].a == Ellipsoid::FLAG )
      {
         ellipseTest[groupNum] = testEllipsoid(spec->xflt(), iSpec);
      } 
      else
      {
         Ellipsoid& settingForGroup = ellipseTest[groupNum];
         Ellipsoid currentEllipsoid(testEllipsoid(spec->xflt(), iSpec));
         if ( currentEllipsoid != settingForGroup )
         {
            std::ostringstream msg;
            msg << "\n***  Spectrum: " 
                << iSpec
                << "in data group " << groupNum
                        << "\n***  contains ellipsoid definition inconsistent "
                        << "with the group (check data entry order)";
            throw DataOrderingError(msg.str());                        
         }                       
      } 
   } 

   std::map<size_t,size_t>::const_iterator itGroupCount = groupCount.begin();
   std::map<size_t,size_t>::const_iterator itGroupCountEnd = groupCount.end();
   count = itGroupCount->second;
   m_dgOrder[itGroupCount->first] = 0;
   size_t iGroup=1;
   ++itGroupCount;
   while (itGroupCount != itGroupCountEnd)
   {
      if (itGroupCount->second != count) 
      {
         string diag(": all projct data groups must have the same number of spectra (observations).");
         throw IncompatibleData(diag);
      }
      m_dgOrder[itGroupCount->first] = iGroup;
      ++itGroupCount;
      ++iGroup;
   }
   m_numberOfShells = groupCount.size();
   m_includesCentreShell = false;
   m_observations = count;  
}

void Projection::doPerform (const EnergyPointer& energy, GroupFluxContainer& fluxes)
{

  GroupFluxContainer tmpFlux(fluxes);  
  GroupFluxContainer::iterator t (tmpFlux.begin());
  GroupFluxContainer::iterator tEnd(tmpFlux.end());
  // recall ArrayContainer == std::map<size_t,std::valarray<Real> > so that
  // s->second has a vectorized assignment operator that takes a scalar. 
  while ( t != tEnd )
  {
     ArrayContainer::iterator s (t->second.begin());
     ArrayContainer::iterator sEnd (t->second.end());
     for ( ; s != sEnd; ++s ) s->second = 0;
     ++t;         
  }


  // the size of each array container is the number of observations 
  // GroupFluxContainer::iterator d (flux.begin());
  // ArrayContainer& primary = d->second;
  // size_t Nobs     (primary.size());
  // this will also be the number of data groups. 


  const size_t Nregions (fluxes.size());
  GroupFluxContainer::const_iterator itOuterFluxes = fluxes.begin(); 

  // roi = region of interest - the region whose observations are currently being
  // computed
  for (size_t roi = 1; roi <= Nregions; ++roi, ++itOuterFluxes)
  {
     const size_t outerDgNum = itOuterFluxes->first;
     const EnergyPointer::const_iterator en_roi = energy.find(outerDgNum);
     const ArrayContainer& energyContainer = *(en_roi->second); 
     ArrayContainer::const_iterator e (energyContainer.begin());
     ArrayContainer& flux_roi = tmpFlux[outerDgNum];
     ArrayContainer::iterator tf (flux_roi.begin());
     ArrayContainer::iterator tfEnd (flux_roi.end());
     // counter for the observations, and index for the Ellipsoid objects
     // that define the projection matrix and its indices.
     size_t obs (0);
     while ( tf != tfEnd )
     {
        // this is the flux we are going to modify on this loop iteration,
        // and its corresponding energy.
        const RealArray& en_obs = e->second;
        RealArray& tf_obs = tf->second;
        RealArray tmp(tf_obs.size());
        //     
        // region to be added to the roi flux. Recall that m_numberOfShells can
        // be +1 greater than Nregions if the virtual shell is present. This is
        // used only for volume factor calculations, it can't modify arrays as
        // there are no corresponding arrays.
        GroupFluxContainer::const_iterator itInnerFluxes = fluxes.begin(); 
        for ( size_t region = 0; region < m_numberOfShells; ++region )
        {
           const size_t innerDgNum = itInnerFluxes->first;
           int interiorShellIndex (0);
           Real volumeFactor(0);
           Real majorAxisOfInteriorShell (interiorVolume(obs,region,interiorShellIndex));                
           const Ellipsoid& shell = m_shell[obs][region];
           if (tpout.maxChatter() >= 30 ) tcout << " axis major  " << shell.a << std::endl;
           int r0 (roi-1);
           if ( majorAxisOfInteriorShell != 0 )
           {
              const Ellipsoid& insideShell = m_shell[obs][interiorShellIndex];
              volumeFactor = (projectionElement(obs,region,r0)
                               - projectionElement(obs,interiorShellIndex,r0) ) 
                              /(FOURPIBY3*(shell.a*shell.b*shell.b  
                                - insideShell.a*insideShell.b*insideShell.b));
              if (tpout.maxChatter() >= 30)
              { 
                 tcout << "vol, proj reg, roi, isi, out, in " 
                         << region << " " << r0 << " " 
                         << interiorShellIndex << " "
                         <<  volumeFactor << "  " 
                         << projectionElement(obs,region,r0) 
                         << "  " << projectionElement(obs,interiorShellIndex,r0) 
                         << std::endl;
                 tcout << " inside shell " << insideShell.a  << std::endl;  
              }      
           }
           else
           {
              volumeFactor = projectionElement(obs,region,r0)
                              /(FOURPIBY3*shell.a*shell.b*shell.b);                                  
              if (tpout.maxChatter() >= 30)
              { 
                 tcout << "vol, proj out " << region << " " << r0  << " " 
                         <<  volumeFactor << "  " << projectionElement(obs,region,r0) 
                         << std::endl; 
              }       
           }  
           if ( volumeFactor > 0 && region + 1 <= Nregions )
           {
              const EnergyPointer::const_iterator en = energy.find(innerDgNum);
              ArrayContainer& fluxr = fluxes[innerDgNum];
              ArrayContainer::iterator ir (fluxr.begin());
              const ArrayContainer& en_calc = *(en->second);
              ArrayContainer::const_iterator er (en_calc.begin());
              for (size_t i = 0; i < obs; ++i, ++ir, ++er);
              const RealArray& en_r = er->second;
              RealArray& fluxir = ir->second;
              if ( XSutility::equalVector(en_obs, en_r) )
              {
                 tf_obs  += fluxir * volumeFactor;               
              }
              else
              {
                 using namespace Numerics::Rebin;
                 size_t inputBin(0);
                 size_t outputBin(0);
                 static const Real FUZZY(1.e-06);

                 initializeBins(en_r, en_obs, FUZZY, inputBin, outputBin,
                         m_startBin, m_endBin, m_startWeight, m_endWeight); 
                 rebin(fluxir, m_startBin, m_endBin, m_startWeight, m_endWeight, tmp);
                 tf_obs += volumeFactor*tmp;                                
              }
           }
           // Careful, m_numberOfShells will be 1 greater than size of fluxes map
           //   when center shell exists.  Do NOT try to increment fluxes iterator
           //   if it's already reached the end.
           if (itInnerFluxes != fluxes.end())
              ++itInnerFluxes;            
        } // end regions -- inner loop
        
        ++obs;
        ++tf;
        ++e;
     }  
  }  

  // this will work as a copy assignment, since the dimensions of the arrays under
  // each are identical by definition.
  fluxes = tmpFlux;   

  if (tpout.maxChatter() >= 30)
  { 
     t = tmpFlux.begin();
     using namespace std;
     ios_base::fmtflags save (tcout.flags());
     streamsize p(tcout.precision(6));
     GroupFluxContainer::iterator f ( fluxes.begin());
     while ( t != tEnd )
     {
        ArrayContainer::iterator s (t->second.begin());
        ArrayContainer::iterator ff (f->second.begin());
        ArrayContainer::iterator sEnd (t->second.end());
        const EnergyPointer::const_iterator en_j = energy.find(t->first);
        const ArrayContainer& energyContainer = *(en_j->second);                 
        ArrayContainer::const_iterator e (energyContainer.begin());
        tcout << " DataGroup " << t->first << '\n';
        for ( ; s != sEnd; ++s, ++e ) 
        {
           const RealArray& en = e->second;
           const RealArray& projFlux = s->second;
           const RealArray& unprojFlux = ff->second;
           size_t Nvec = projFlux.size();
           for (size_t k = 0; k < Nvec; ++k)
           {
              tcout << setw(4) << k << "   "  << setw(9)  << en[k+1]
                << "   "  << setw(9) << unprojFlux[k] << "   " 
                << setw(9) << projFlux[k] << endl;            

           } 
        }
        tcout << "\n\n" << flush;
        ++t, ++f;         
     }

     tcout.flags(save);
     tcout.precision(p);

  }
}

Projection::Ellipsoid Projection::testEllipsoid (const std::map<string, Real>& xflt, int spectrumNumber)
{
  // check whether the correct XFLT keywords are available. this also attempts
  // to be backward compatible in the case that the "key: value" format was not
  // used

  Real major=0.0, minor=0.0, orientation=0.0;

  if ( xflt.count("major") == 0 ) {
    if ( xflt.count("key1") == 0 ) {
      std::ostringstream diag;
      diag << "  Cannot find keyword for major axis size in spectrum " << spectrumNumber;
      throw IncompatibleData(diag.str());
    } else {
      major = xflt.find("key1")->second;
    }
  } else {
    major = xflt.find("major")->second;
  }      

  if ( xflt.count("minor") == 0 ) {
    if ( xflt.count("key2") == 0 ) {
      std::ostringstream diag;
      diag << "  Cannot find keyword for minor axis size in spectrum " << spectrumNumber;
      throw IncompatibleData(diag.str());
    } else {
      minor = xflt.find("key2")->second;
    }
  } else {
    minor = xflt.find("minor")->second;
  }      

  if ( xflt.count("orient") == 0 ) {
    if ( xflt.count("key3") == 0 ) {
      std::ostringstream diag;
      diag << "  Cannot find keyword for orientation in spectrum " << spectrumNumber;
      throw IncompatibleData(diag.str());
    } else {
      orientation = xflt.find("key3")->second;
    }
  } else {
    orientation = xflt.find("orient")->second;
  }      

  return Ellipsoid(major, minor, orientation);      
}

void Projection::orderEllipsoids (bool usingCenter)
{
  // This works on the assumption that all observations contain the
  // same ordering, since verifyData should already have checked that
  // major/minor axes match for all spectra in a group.  Therefore
  // only check the first obs and copy results to the rest.
  std::vector<Ellipsoid>& firstObsShells = m_shell[0];
  // Sets the 0-based shell number of the shell immediately to the
  // inside, and -1 for the innermost.  Shells will often already
  // be loaded in order, but they don't have to be.  

  // After a shell is identified as another's inner shell, it is removed
  // from the set.
  std::set<int> remainingShells;
  for (size_t i=0; i<m_numberOfShells; ++i)
     remainingShells.insert(static_cast<int>(i));

  for (size_t i=0; i<m_numberOfShells; ++i)
  {
     Real largestInner = .0;
     int idxLargest = -1;
     Ellipsoid& shell_i = firstObsShells[i];
     std::set<int>::iterator itRemaining = remainingShells.begin();
     std::set<int>::iterator itEnd = remainingShells.end();
     while (itRemaining != itEnd)
     {
        int testIdx = *itRemaining;
        if (shell_i.a > firstObsShells[testIdx].a)
        {
           if (firstObsShells[testIdx].a > largestInner)
           {
              largestInner = firstObsShells[testIdx].a;
              idxLargest = testIdx;
           }
        }
        ++itRemaining;
     }
     shell_i.innerShellNum = idxLargest;
     if (idxLargest >= 0) 
        remainingShells.erase(idxLargest);
  }

  // Enforce the rule that if a center hole is specified, its major
  // axis doesn't extend beyond any of the shells.  ie. it must
  // truly be innermost.
  if (usingCenter)
  {
     if (firstObsShells[m_numberOfShells-1].innerShellNum != -1)
     {
        string errMsg("The 3 fixed parameters of the projct component must specify the inner-most boundary.");
        errMsg += "\n  Their axes must not extend beyond any of those given by the XFLT keywords";
        errMsg += "\n  in the data sets.\n";
        throw YellowAlert(errMsg); 
     }
  }

  for (size_t iObs=1; iObs < m_observations; ++iObs)
  {
     std::vector<Ellipsoid>& obsShells = m_shell[iObs];
     for (size_t iShell=0; iShell<m_numberOfShells; ++iShell)
        obsShells[iShell].innerShellNum = firstObsShells[iShell].innerShellNum;  
  }
}

// Additional Declarations
