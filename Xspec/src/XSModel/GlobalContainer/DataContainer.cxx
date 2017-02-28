//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// DataUtility
#include <XSModel/Data/DataUtility.h>
// XSparse
#include <XSUtil/Parse/XSparse.h>
// SpectralData
#include <XSModel/Data/SpectralData.h>
// DataContainer
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSUtil/Error/Error.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSUtil/FunctionUtils/FunctionUtility.h>
#include <XSModel/Data/Detector/Response.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/GlobalContainer/ResponseContainer.h>
#include <XSModel/Data/Detector/RealResponse.h>
#include <XSModel/Data/Detector/MultiResponse.h>
#include <XSModel/Data/Detector/UserDummyResponse.h>
#include <XSModel/Data/BackCorr/Background.h>
#include <XSModel/GlobalContainer/TrashPtr.h>
#include <XSModel/GlobalContainer/TrashAdapter.h>
#include <XSModel/Parameter/ResponseParam.h>
#include <xsTypes.h>
#include <XSContainer.h>
#include <XSstreams.h>
#include <algorithm>
#include <queue>
#include <vector>
#include <sstream>
#include <set>


  const Real SMALL = FLT_MIN;
  const Real LARGE = FLT_MAX;
  const int PARLEN = 8;
  const int XSPEC_OK = 0;
  const int XSPEC_ERROR = -1;
  const int MAXLINELEN = 81;
  const int MAX_FILENAMELEN = 81;
  bool PRINT_DIAGS = true;

  // console channel for tkcon to write to stdout
  std::ostream& tccon = tpcon;

  // analogues of 3 of the usual defined streams.

  std::ostream& tcout = tpout;
  std::ostream& tcerr = tperr;
  std::istream& tcin = tpin;


namespace XSContainer {
      DataContainer* datasets = 0;
      XspecRegistry* xsRegistry = 0;

    // Class XSContainer::DataContainer::NoSuchSpectrum 

    DataContainer::NoSuchSpectrum::NoSuchSpectrum (int number)
        : YellowAlert("Spectrum not loaded: ")              
    {
        tcerr << number << '\n';
    }


    // Class XSContainer::DataContainer::DataMemento 

    DataContainer::DataMemento::DataMemento(const DataContainer::DataMemento &right)
	: m_spectra(right.m_spectra)
    {
    }

    DataContainer::DataMemento::DataMemento (size_t size)
	: m_spectra(size)
    {
    }


    DataContainer::DataMemento::~DataMemento()
    {
    }


    // Additional Declarations

    // Class XSContainer::DataContainer::DataGroupHistory 

    DataContainer::DataGroupHistory::DataGroupHistory()
       : m_reassignment(),
         m_reverseLookup()
    {
    }


    DataContainer::DataGroupHistory::~DataGroupHistory()
    {
    }


    void DataContainer::DataGroupHistory::initOldArray (size_t nGroups)
    {
       m_reassignment.resize(nGroups);
       std::fill(m_reassignment.begin(), m_reassignment.end(), 0);
       // Whatever was in reverseLookup is obsolete now.
       m_reverseLookup.clear();
    }

    void DataContainer::DataGroupHistory::swapHistory ()
    {
       std::swap(m_reassignment, m_reverseLookup);
    }

    void DataContainer::DataGroupHistory::reassign (size_t oldVal, size_t newVal)
    {
       // It's possible for oldVal to be larger than m_reassignment if
       // it refers to a just-added data group, in which case do nothing.
       if (oldVal <= m_reassignment.size())
       {
          m_reassignment[oldVal-1] = newVal;
       }
    }

    size_t DataContainer::DataGroupHistory::getReassignment (size_t oldVal) const
    {
       if (!oldVal || oldVal > m_reassignment.size())
          throw RedAlert("DataGroupHistory reassigment vector out-of-bounds access.");
       return m_reassignment[oldVal-1];
    }

    size_t DataContainer::DataGroupHistory::getOriginal (size_t newVal) const
    {
       if (!newVal || newVal > m_reverseLookup.size())
          throw RedAlert("DataGroupHistory reverseLookup vector out-of-bounds access.");
       return m_reverseLookup[newVal-1];
    }

    void DataContainer::DataGroupHistory::createReverseLookup (size_t nGroups)
    {
       m_reverseLookup.resize(nGroups,0);
       if (nGroups)
       {
          for (size_t i=1; i<=m_reassignment.size(); ++i)
          {
             size_t assignee = m_reassignment[i-1];
             // If assignee = 0, that particular data group no longer exists.
             if (assignee)
             {
                if (assignee > nGroups)
                {
                   throw RedAlert("Out-of-bounds array access in DataGroupHistory::createReverseLookup");
                }
                m_reverseLookup[assignee-1] = i;
             }
          }
       }
     //  tcout <<"\nData group reassignment:"<<std::endl;
     //  for (size_t i=0; i<m_reassignment.size(); ++i)
     //      tcout <<"   "<<i+1<<"   "<<m_reassignment[i]<<std::endl;
     //  tcout << "Reverse lookup:"<<std::endl;
     //  for (size_t i=0; i<m_reverseLookup.size(); ++i)
     //     tcout <<"   "<<i+1<<"   "<<m_reverseLookup[i]<<std::endl;
    }

    void DataContainer::DataGroupHistory::erase ()
    {
      m_reassignment.clear();
      m_reverseLookup.clear();
    }

    bool DataContainer::DataGroupHistory::isEmpty ()
    {
       return m_reassignment.empty() && m_reverseLookup.empty();
    }

    // Additional Declarations

    // Class XSContainer::DataContainer 
    DataContainer* DataContainer::s_instance = 0;

    DataContainer::DataContainer()
	: m_numberOfSpectra(0),
	  m_numberOfGroups(1),
	  m_numSourcesForSpectra(1),
	  m_numberOfPlotGroups(0),
          m_realRange(0.,0.),
          m_bundledSpectra(),
          m_trash(),
          m_dgHistory(),
          m_dgToSources(),
          m_sourceToDgs(),
          m_integerRange(1,1),
          m_spectrumRange(1,1),      
          m_dataArray(),
          m_plotGroupNums()
    {
       // The rest of the program can ASSUME that for every
       //  data group up to m_numberOfGroups, there will ALWAYS
       //  be an entry for it in the m_dgToSources map -- ie.
       //  it will NOT have to compare the results of a find()
       //  operation to the end() iterator.
       m_dgToSources[1] = std::set<size_t>();
    }


    DataContainer::~DataContainer()
    {
	clear();
	emptyTrash();
    }


    DataContainer* DataContainer::Instance ()
    {
	if (s_instance == 0) s_instance = new DataContainer;
	return s_instance;
    }

    void DataContainer::plot (const string& dataSet, const string& args)
    {
    }

    void DataContainer::addToList (DataSet* newDataSet)
    {

	if (m_numberOfSpectra==0)
	{
	    m_numSourcesForSpectra = newDataSet->origNumSources();
            XSContainer::responses->adjustNumSources(m_numSourcesForSpectra);
	}
	// remove dataSets that are overwritten by the new one, and then
	// add in the new one.     
        bool anyRemoved = false;
	if (newDataSet->isMultiple())
	{
	    // remember Data is a friend class of DataSet...
	    size_t newPlotGroup;

	    const SpectralDataMap& newSpectra = newDataSet->multiSpectralData();
	    SpectralDataMapConstIt sm = newSpectra.begin();
	    SpectralDataMapConstIt smEnd(newSpectra.end());
	    size_t initNumberOfSpectra = m_numberOfSpectra;
	    while (sm != smEnd)
	    {
		size_t newSpectraNumber = sm->second->spectrumNumber();
		// the parse routine needs to check whether the new numbers
		// are 'valid' in the sense of being less than or equal to 
		// one greater than the number before the command is called.
		if (newSpectraNumber <= initNumberOfSpectra)
		{
		    newPlotGroup = m_plotGroupNums[newSpectraNumber-1];
		    removeNumberedSpectrum(newSpectraNumber);
                    anyRemoved = true;			   
		}
		else
		{
		    newPlotGroup = ++m_numberOfPlotGroups;
		    m_plotGroupNums.push_back(newPlotGroup);
		}
		sm->second->plotGroup(newPlotGroup);
		++sm;           
	    }

	}
	else
	{
	    size_t newSpectrumNumber = newDataSet->spectralData()->spectrumNumber();
	    size_t newPlotGroup;

	    if (newSpectrumNumber <= m_numberOfSpectra) 
	    {
		newPlotGroup = m_plotGroupNums[newSpectrumNumber-1];
		newDataSet->spectralData()->plotGroup(newPlotGroup);   
		removeNumberedSpectrum(newSpectrumNumber);
                anyRemoved = true;
	    }
	    else 
	    {
		newDataSet->spectralData()->plotGroup(++m_numberOfPlotGroups);
		m_plotGroupNums.push_back(m_numberOfPlotGroups);
	    }
	}
        if (anyRemoved)
           adjustNumSources(0);

	// add new item to the data array, now assured that the spectrum numbers will be
	// unique.
	dataArray(newDataSet->dataName(),newDataSet);

	// recount total number of spectra, and update the global indicator.
	enumerateSpectra();
        // This ASSUMES addToList only inserts brand new data sets (from data
        // or fakeit command).  Therefore all spectra in newDataSet still 
        // have origNumSources.
        size_t origNSources = static_cast<size_t>(newDataSet->origNumSources());  
        if (origNSources > m_numSourcesForSpectra)
           adjustNumSources(origNSources);
        else if (origNSources < m_numSourcesForSpectra)
        {
           bool handleSingle = !newDataSet->isMultiple();
           SpectralDataMapConstIt itSd = newDataSet->multiSpectralData().begin();
           SpectralDataMapConstIt itSdEnd = newDataSet->multiSpectralData().end();
           while (handleSingle || itSd != itSdEnd)
           {
              SpectralData* sd = handleSingle ? newDataSet->spectralData()
                        : itSd->second;
              sd->increaseNDets(m_numSourcesForSpectra);
              if (handleSingle)
                 handleSingle = false;
              else
                 ++itSd;
           }
        }
        bundleSpectra();
    }

    SpectralData* DataContainer::lookup (size_t spectrumNumber) const
    {
        SpectralData* sd = (spectrumNumber > 0 && spectrumNumber <= 
                m_bundledSpectra.size()) ? m_bundledSpectra[spectrumNumber-1] : 0;
	return sd;
    }

    void DataContainer::enumerateSpectra ()
    {
	m_numberOfSpectra = 0;
	if (!m_dataArray.empty())
	{
	    DataArrayConstIt dsEnd = m_dataArray.end();
	    for (DataArrayConstIt ds = m_dataArray.begin(); ds != dsEnd; ++ds)
	    {
		const DataSet* current = ds->second;
		if (!current->isMultiple())
		{        
		    m_numberOfSpectra++;
		}
		else
		{
		    m_numberOfSpectra += current->multiSpectralData().size();
		}
	    }          
	} 
        else
        {
           m_numSourcesForSpectra = 1;
           XSContainer::responses->adjustNumSources(1);
        }     
    }

    void DataContainer::removeNumberedSpectrum (size_t index, bool remove)
    {
	DataArrayIt  dm    = m_dataArray.begin();
	DataArrayIt  dmEnd = m_dataArray.end(); // non-const.
	static const string REMOVED(" removed ");
	static const string REPLACED(" replaced ");

	// testing this function is gonna be tricky.
	while (dm != dmEnd)
	{
	    // DataSet::removeNumberedSpectrum returns true if the DataSet is
	    // empty after the spectrum has been removed.
	    int status = dm->second->removeNumberedSpectrum(index,remove);
	    if (status)
	    {
		if (status == -1)
		{     
		    //Just erase from m_dataArray, don't delete memory yet.
		    //Memory will be deleted when SpectralData is deleted
                    //delete dm->second;
		    moveToTrash(new TrashAdapter<DataSet*>(dm->second));
                    m_dataArray.erase(dm);
		}
		--m_numberOfSpectra;
		break;
	    }
	    else ++dm;    
	}
    }

    std::vector<bool> DataContainer::insertAndDelete (DataUtility::recordList& records, size_t spectraDefined, bool preserve)
    {
	//Only way spectra are to be removed entirely is if the user
	//didn't specify preservation. 
	std::vector<bool> markForDeletion(spectraDefined+1, false);

	if (records.empty()) 
	    return markForDeletion;

	DataInputRecord& lastRecord = records.back();

	// Mark for "preservation" anything that's going to be replaced
	// rather than removed.  Assumption:  By this point, the record
	// list is sorted (by spectrumNumber[0]), and there is no
	// overlapping between records (such as 1 file1{1-3} 2 file2{7}).

	// All existing spectra with numbers equal or below that of the end
	// of the highest record should be marked for preservation.  
	// Assumption: a "none" command will only exist in the last
	// record of the list.

	// markedForDeletion is 1-based, first element is not used.

	size_t lastRecordStart = lastRecord.spectrumNumber().front();
	size_t veryLast = lastRecord.spectrumNumber().back();
	size_t nCommaPreserve = lastRecord.numTrailCommas();

	if (lastRecordStart > spectraDefined)
	{
	    //basically do nothing, user specified spectrum # that is
	    //not loaded
	    if (lastRecord.fileName() == XSparse::NONE())
	        records.pop_back();

	    // if there are trailing commas they make no sense, do nothing.
	}
	else if (lastRecord.fileName() == XSparse::NONE())
	{
	    // if the command ends in 'none/' a spectrum is deleted and the spectra with
	    // higher numbers are renumbered down one.
	    // mark this for deletion.
	    if(preserve)
		markForDeletion[lastRecordStart] = true; 
	    else
		std::fill(markForDeletion.begin() + lastRecordStart,
			  markForDeletion.end(), true); 
	    // get rid of the last record.
	    records.pop_back();
	}
	else if (nCommaPreserve > 1)
	{
	    // Check for trailing commas, and leave them marked for preservation.
	    // They are only contained in the last input record.	     
	    // Ignore the first trailing comma.
	    for (size_t i=veryLast+nCommaPreserve; i<=markForDeletion.size(); ++i)
		markForDeletion[i] = true;
	}
	else if(!preserve)
	{
	    if(veryLast < spectraDefined)
		std::fill(markForDeletion.begin() + veryLast + 1, 
			  markForDeletion.end(), true);
	}

	return markForDeletion;    
    }

    void DataContainer::clear ()
    {
        // Even though all spectra are being moved to trash, 
        // don't want to just call responses->clear().  Spectra 
        // in DataContainer aren't necessarily 1 to 1 with 
        // Responses in ResponseContainer at all times, ie.
        // a fake DataSet waiting to be inserted has already
        // placed its Responses in ResponseContainer. 
        for (size_t i=0; i<m_numberOfSpectra; ++i)
        {
           SpectralData* sd = m_bundledSpectra[i];
           XSContainer::responses->removeByToken(sd->detector());
        }

	DataArrayIt d (m_dataArray.begin());
	DataArrayIt dEnd (m_dataArray.end());
	while ( d != dEnd ) 
	{
	    moveToTrash(new TrashAdapter<DataSet*>(d->second));
	    ++d;
	}
	m_dataArray.clear();

	enumerateSpectra();
        enumerateGroups();
	m_numberOfPlotGroups = 0;
        m_bundledSpectra.clear();
	m_plotGroupNums.clear();
        FunctionUtility::clearXFLT();
    }

    void DataContainer::deleteRange (size_t begin, size_t end)
    {
	if (m_dataArray.empty() || begin > end) return;       


	for (size_t j = begin; j <= end; ++ j)
	{
	    datasets->removeNumberedSpectrum(j,true);
	}      

        adjustNumSources(0);
	enumerateSpectra();
        enumerateGroups();
        bundleSpectra();
	m_plotGroupNums.resize(m_numberOfSpectra);
	m_numberOfPlotGroups=0;
	for (size_t j=0; j<m_numberOfSpectra; ++j)
	{
	    if (static_cast<size_t>(m_plotGroupNums[j]) > m_numberOfPlotGroups)
	    {
	        m_numberOfPlotGroups = m_plotGroupNums[j];
	    }
	}
	if (m_numberOfSpectra > 0)
	{
	    string spectra = (m_numberOfSpectra == 1) ? " spectrum " : " spectra ";

	    tcout << "\n" << m_numberOfSpectra   <<  spectra << " in use\n " << std::endl;
	}
    }

    void DataContainer::renumberSpectra (size_t start, int offset)
    {
	// Change by offset all spectrumNumbers greater than start. offset defaults to -1
        // DON'T ASSUME m_bundledSpectra.size() == m_numberOfSpectra, since this may 
        // be called AFTER a deletion but BEFORE m_bundledSpectra has been updated.
        // start is 1-based.

        const int nSpec = static_cast<int>(m_bundledSpectra.size());
        for (int i=start; i<nSpec; ++i)
        {
           m_bundledSpectra[i]->renumber(i+1+offset);
        }

        // The responses container keeps an internal map which is
        // indexed by spectrum number keys.  Therefore it needs
        // to be told of this renumbering.
        if (offset > 0)
           throw RedAlert("Invalid attempt to increase spectrum numbers in DataContainer::renumberSpectra.");
        size_t downShift = static_cast<size_t>(-1*offset);
        XSContainer::responses->renumberSpectrum(start, downShift);
    }

    void DataContainer::deleteRange (const std::vector<bool>& marked, bool preserve)
    {
	size_t n = marked.size() - 1;
	static const bool removed = true;
	for (size_t i = 1; i <= n; ++i) 
	{
	    if (marked[i]) 
	    {
		removeNumberedSpectrum(i,removed);       
		size_t removedPlotGroupNum = m_plotGroupNums[i-1];
		bool isFirst = firstInPlotGroup(i);
		for (size_t j=i-1; j<m_numberOfSpectra; ++j)
		{
		    m_plotGroupNums[j] = m_plotGroupNums[j+1];
		}
		m_plotGroupNums.pop_back();
		if (preserve)
		{
		    renumberSpectra(i); // renumber all spectra with higher numbers
                    // renumberPlotGroups calls the spectra lookup function, which
                    // requires that the m_bundledSpectra vector be current...
                    bundleSpectra();
		    if (isFirst && i < m_numberOfSpectra+1)
		    {
			renumberPlotGroups(i, removedPlotGroupNum);
		    }
		}
	    }
	}   
	enumerateSpectra();
        enumerateGroups();
        adjustNumSources(0);
        if (!preserve)  bundleSpectra();

	countPlotGroups();

	if (m_numberOfSpectra > 0)
	{
	    string spectra = (m_numberOfSpectra == 1) ? " spectrum " : " spectra ";

	    tcout << "\n" << m_numberOfSpectra   <<  spectra << " in use\n " << std::endl;
	} 
    }

    void DataContainer::enumerateGroups ()
    {
        // This ASSUMES m_dgHistory's reassignment array has already been set to
        // the proper size of the number of data groups PRIOR to the changes made
        // in whatever function is calling this.
	if (m_dataArray.empty())
	{
	    m_numberOfGroups = 1;
            m_dgHistory.reassign(1,1);
            m_dgHistory.createReverseLookup(1);
	}
	else
	{
	    DataArrayConstIt dsEnd = m_dataArray.end();
	    size_t mgMax = 0;
	    for (DataArrayConstIt ds = m_dataArray.begin(); ds != dsEnd; ++ds)
	    {

		const DataSet* current = ds->second;
                const size_t dgNum = current->dataGroup();
                m_dgHistory.reassign(dgNum,dgNum);
		mgMax = std::max(mgMax,dgNum);               
	    }  
	    m_numberOfGroups = mgMax;
            fixDataGroupGaps();
	} 
        determineDgSourceRelations();     
    }

    DataSet* DataContainer::dataArray (const string& name, size_t index) const
    {
	size_t N(m_dataArray.count(name));
	if ( N == 0 )
	{
	    return 0;
	}      
	else
	{
	    DataArrayConstIt id(m_dataArray.lower_bound(name));
	    if ( N == 1 ) 
	    {
		return (*id).second;
	    }
	    else
	    {
		DataArrayConstIt idEnd(m_dataArray.upper_bound(name));
		for ( ; id != idEnd; ++id )
		{
		    if ( (*id).second->index() == index)
		    {
			return (*id).second;
		    } 
		}
		return 0;
	    }

	}
    }

    void DataContainer::ignoreBadChannels ()
    {
	DataArrayConstIt id(m_dataArray.begin());
	DataArrayConstIt idEnd(m_dataArray.end());

	while (id != idEnd)
	{
	    (*id).second->ignoreBadChannels();
	    ++id;       
	}
    }

    void DataContainer::setChannels (bool value, IntegerArray& channelRange, IntegerArray& spectrumRange)
    {

        if (channelRange.size() != 2)
        {
           throw RedAlert("Improper channelRange size in DataContainer::setChannels");
        }
        bool resetLow = channelRange[0] == -2;
        bool resetHigh = channelRange[1] == -2;

	size_t N(spectrumRange.size()); 

	if (N > 0)
	{
	    if(spectrumRange[0] == -2) spectrumRange[0] = 1;
	    if(spectrumRange[1] == -2 || spectrumRange[1] > static_cast<int>(m_numberOfSpectra)) 
		spectrumRange[1] = m_numberOfSpectra;

	    IntegerArray rrange(spectrumRange);

	    rrange = XSparse::expandRange(rrange);

            std::vector<SpectralData*> spectra;

            // This function may throw.
            bundleSpectra(rrange, spectra); 

	    for (size_t j = 0; j < spectra.size(); ++j)
            {
		// this may throw
		spectra[j]->setChannels(value,channelRange);
                if (resetLow) channelRange[0] = -2;
                if (resetHigh) channelRange[1] = -2;
            }
	}
	//may not be necessary...
	else
	{
	    // do the lot.
	    DataArrayConstIt it(m_dataArray.begin());
	    DataArrayConstIt itEnd(m_dataArray.end());
	    for ( ; it != itEnd; ++it)  
	    {
		(*it).second->setChannels(value,channelRange);
                if (resetLow) channelRange[0] = -2;
                if (resetHigh) channelRange[1] = -2;
	    }

	}
    }

    void DataContainer::setChannels (bool value, std::pair<Real,Real>& realRange, IntegerArray& spectrumRange)
    {
	if(spectrumRange[0] == -2) spectrumRange[0] = 1;
	if(spectrumRange[1] == -2 || spectrumRange[1] > static_cast<int>(m_numberOfSpectra))
	    spectrumRange[1] = m_numberOfSpectra;

	IntegerArray rrange(spectrumRange);

	rrange = XSparse::expandRange(rrange);

        std::vector<SpectralData*> spectra;
        // This function may throw.
        bundleSpectra(rrange, spectra);
        bool resetLow = realRange.first == -2;
        bool resetHigh = realRange.second == -2;
	for (size_t j = 0; j < spectra.size(); ++j)
	{
	    SpectralData* spectrum = spectra[j];
           // this may throw
	   spectrum->setChannels(value, realRange);
           if (resetLow) realRange.first = -2;
           if (resetHigh) realRange.second = -2;
	}
    }

    void DataContainer::saveData (std::ostream& s, const string& defaultStat)
    {
	using namespace std;

	ostringstream ignoreInfo(ostringstream::app),
	    respFuncInfo, respParLinks, statInfo;

	ignoreInfo << "ignore";

        IntegerArray rows;
        vector<const SpectralData*> specsForDataCmd;
        bool processPendingSpectra = false;

        // This holds the absolute path of the current working directory,
        // and will be modified each time a "cd" command is written to
        // the save file.  However note that only relative paths are
        // actually written to the file. 
        string currentDir(XSutility::getRunPath());

	std::vector<SpectralData*>::const_iterator
	    itCurrSpectra = m_bundledSpectra.begin(),
	    itEndSpectra = m_bundledSpectra.end();

        if (itCurrSpectra == itEndSpectra)
           return;

        while (itCurrSpectra != itEndSpectra)
        {
           const SpectralData* spectrum = *itCurrSpectra;
           bool isType1 = !spectrum->rowNumber();
           string relativePath;
           if (isType1)
           {
              // If any type2 spectra are pending they will get processed now.
              // In that case we don't increment itCurrSpectra, so this type1
              // spectrum will get processed next time through.
              if (specsForDataCmd.empty())
              {
                 saveIgnoredChannels(spectrum->parent(), spectrum, ignoreInfo);
                 examineResponseFunctions(spectrum, respFuncInfo, respParLinks);
                 specsForDataCmd.push_back(spectrum);
                 ++itCurrSpectra;              
              }
              processPendingSpectra = true;
           }
           else
           {
              // type-II spectrum
              if (!specsForDataCmd.empty() &&
                  (specsForDataCmd[0]->parent()->dataGroup() != spectrum->parent()->dataGroup() ||
		  specsForDataCmd[0]->parent()->dataName() != spectrum->parent()->dataName()))
              {
                 // Just finish off the previous type2 set and
                 // then come back to handling the current spectrum.
                 processPendingSpectra = true;
              }
              else
              {
                 rows.push_back(spectrum->rowNumber());
                 saveIgnoredChannels(spectrum->parent(), spectrum, ignoreInfo);
                 examineResponseFunctions(spectrum, respFuncInfo, respParLinks);
                 specsForDataCmd.push_back(spectrum);
                 ++itCurrSpectra;
                 processPendingSpectra = (itCurrSpectra == itEndSpectra);
              }
           }

	   if (processPendingSpectra)
           {
              // If processing multiple type2 spectra, they must all be
              // in the same file (and hence the same directory).
              // Can't get here with an empty specsForDataCmd.
              const DataSet* parent = specsForDataCmd[0]->parent();
              relativePath = XSparse::absToRelPath(currentDir,parent->getRunPath());
              if (relativePath.length())
              {
	         s << "cd " << relativePath << endl;
                 currentDir = parent->getRunPath();
              }
	      s  << "data " << parent->dataGroup() << ':' << specsForDataCmd[0]->spectrumNumber()
	        << ' ' << XSutility::insertEscapeChars(parent->dataName());
              if (!rows.empty())
                 s << '{' << XSparse::ArrayToRange(rows) << '}'; 
              s << endl;

              examineResponse(specsForDataCmd, s, currentDir);
              examineArf(specsForDataCmd, s, currentDir);
              examineBackCorr(specsForDataCmd, s, true, currentDir);
              examineBackCorr(specsForDataCmd, s, false, currentDir);
              if (itCurrSpectra != itEndSpectra)
                 s << endl;

              rows.clear();
              specsForDataCmd.clear();
              processPendingSpectra = false;
           }

           if (spectrum->statName() != defaultStat)
           {
              statInfo << "statistic " << spectrum->statName() << " "
                        << spectrum->spectrumNumber() << "\n";
           }

        }  // end spectra loop

        // Make sure script moves things back to the original 
        // working directory.
        string relPath(XSparse::absToRelPath(currentDir,XSutility::getRunPath()));
        if (relPath.length())
           s << "cd " << relPath << endl;


	string strIgnore = ignoreInfo.str();
	if(strIgnore != "ignore")
	    s << strIgnore << endl;

        if (!statInfo.str().empty())
           s << statInfo.str() << endl;

        // Order is important.  Gain commands must be entered AFTER
        // all responses and arfs have been set up.  Response
        // parameter links must be entered AFTER all gain pars
        // have been loaded.
        if (respFuncInfo.str() != "")
           s << respFuncInfo.str() << endl;
        if (respParLinks.str() != "")
           s << respParLinks.str() << endl;

        s << endl;
    }

    void DataContainer::dataArray (const std::string& name, DataSet* value)
    {
	m_dataArray.insert(DataArray::value_type(name,value));
    }

    DataSet* DataContainer::dataSetLookup (size_t specNum, size_t& row) const
    {
	SpectralData* sd (0);
	DataSet* dset(0);
	bool isFound = false;
	if (!m_dataArray.empty())
	{
	    DataArrayConstIt dsEnd = m_dataArray.end();
	    for (DataArrayConstIt ds = m_dataArray.begin(); !isFound && ds != dsEnd; ++ds)
	    {
		dset = ds->second;
		if (!dset->isMultiple())
		{        
		    sd  = dset->spectralData();
		    if (sd->spectrumNumber() == specNum) isFound=true;
		}
		else
		{
		    const SpectralDataMap& ssd = dset->multiSpectralData();
		    SpectralDataMapConstIt siEnd = ssd.end();
		    SpectralDataMapConstIt si = ssd.begin();
		    while (!isFound && si != siEnd)
		    { 
			sd = si->second;
			if (sd->spectrumNumber() == specNum) isFound=true;
			++si;
		    } 
		}
	    }
	}
	if (isFound)
	{
	    row = sd->rowNumber();
	}
	else
	{
	    row = 0;
	    dset = 0;
	}	  
	return dset;
    }

    void DataContainer::renumberPlotGroups (size_t specNum, size_t high)
    {

	// Stage 1.  All spectra prior to specNum(1-based) will not be modified.  
	// Parameter "high" is the largest group number assigned prior to
	// specNum.  All numbers reassigned in this stage will be larger than
	// "high." Note that the spectrum with plotGroup = high might no longer
	// exist.  It's removal being the reason this function was called.  In
	// this case, high = plot group number of the removed spectrum.

	size_t sz = m_numberOfSpectra;
	size_t start = specNum-1;
	std::queue<size_t> needsUpdate;
	bool renumberFlag = false;


	for (size_t i=start; i<sz; ++i)
	{
	    if (m_plotGroupNums[i] == static_cast<int>(high))
	    {
		needsUpdate.push(i);
		renumberFlag = true;
	    }
	}

	while (!needsUpdate.empty())
	{
	    size_t qsz = needsUpdate.size();
	    size_t currElem = needsUpdate.front();
	    // find new high
	    for (size_t i=start; i<currElem; ++i)
	    {
		if (m_plotGroupNums[i] > static_cast<int>(high))
		{
		    high = m_plotGroupNums[i];
		}
	    }
	    ++high;
	    // Flag the new numbers which will need to be updated.
	    for (size_t i=currElem+1; i<sz; ++i)
	    {
		if (m_plotGroupNums[i] == static_cast<int>(high))
		{
		    needsUpdate.push(i);
		}
	    }
	    // Update those that were originally in queue, remove them
	    // from the queue and repeat the process.
	    for (size_t i=0; i<qsz; ++i)
	    {
		m_plotGroupNums[needsUpdate.front()] = high;
		needsUpdate.pop();
	    }
	    start = currElem+1;
	}

	// This 2nd stage detects and closes any gaps in the plot group numbering.  Gaps
	// may occur from deleting spectral data, or from the setplot group command.
	start = 0;
	high = 0;
	while (start < sz)
	{
	    int diff = m_plotGroupNums[start] - high;
	    if (diff > 0)
	    {
		high = m_plotGroupNums[start];
		if (diff > 1)
		{
		    for (size_t i=start; i<m_numberOfSpectra; ++i)
		    {
			m_plotGroupNums[i] -= diff-1;
		    }
		    high -= diff-1;
		    renumberFlag = true;
		}
	    }
	    ++start;
	}
	m_numberOfPlotGroups = high;

	if (renumberFlag)
	{
	    for (size_t i=specNum-1; i<m_numberOfSpectra; ++i)
	    {
		lookup(i+1)->plotGroup(m_plotGroupNums[i]);
	    }
	}
    }

    bool DataContainer::firstInPlotGroup (size_t specNum)
    {
	size_t plotGroupNumber = m_plotGroupNums[specNum-1];
	for (size_t i=0; i<specNum-1; ++i)
	{
	    if (m_plotGroupNums[i] == static_cast<int>(plotGroupNumber))
	    {
		return false;
	    }
	}
	return true;
    }

    bool DataContainer::resetDetectors (const IntegerArray& spectra)
    {
	bool isChanged = false;
	size_t nS = spectra.size();
	// Unlike in setChannels, this function assumes the IntegerArray has
	// already had any wildcards parsed, and that all values correspond
	// to actual spectra.
	if (!nS)
	{
	    // Reset for all spectra.
	    DataArrayConstIt iD(m_dataArray.begin());
	    DataArrayConstIt iDend(m_dataArray.end());
	    while (iD != iDend)
	    {
		if (iD->second->resetAllDetectors())
		{
		    isChanged = true;
		}
		++iD;
	    } 
	}
	else
	{
	    for (size_t j = 0; j < nS; ++j)
	    {
		SpectralData* spectrum(lookup(spectra[j]));
		if (spectrum)
		{
		    if (spectrum->removeAllDummies())
		    {
			isChanged = true;
		    }
		}
	    }
	}

        if (isChanged)
        {
            // If no original response existed, removing a dummy can affect
            // the source-dg configuration.  So...
            adjustNumSources(0);
            determineDgSourceRelations();           
        }
	return isChanged;
    }

    void DataContainer::saveIgnoredChannels (const DataSet* pDataSet, const SpectralData* pSData, std::ostringstream& ignoreInfo)
    {
	const BoolArray& noticedChannels = pSData->noticedChannels();
	size_t n(pSData->channels());
	size_t offset = pSData->startChan() - pSData->firstChan();

	bool lastChanState = noticedChannels[offset];
	int chanStartRange = lastChanState == 0 ? static_cast<int>(offset) : -1;

	BoolArray::const_iterator 
	    i_begNoticed = noticedChannels.begin() + offset,
	    i_endNoticed = i_begNoticed + n;

	if(std::find(i_begNoticed, i_endNoticed, false) != i_endNoticed)
	{
	    ignoreInfo << ' ' << pSData->spectrumNumber()  << ':';

	    for(size_t i = offset + 1; i <= n; ++i) {
		if(i == n || noticedChannels[i] != lastChanState) {
		    if(lastChanState == 0) {
			ignoreInfo << XSparse::IntToString(chanStartRange - offset + 1);

			if(chanStartRange != static_cast<int>(i - 1))
			    ignoreInfo << '-' << XSparse::IntToString(i - offset);

			ignoreInfo << ',';
		    }
		    else
			chanStartRange = i;
		}

		if(i != n) lastChanState = noticedChannels[i];
	    }

	    string strIgnore = ignoreInfo.str();
	    strIgnore.erase(strIgnore.length() - 1, 1);
	    ignoreInfo.str(strIgnore);
	}
    }

    DataArray& DataContainer::dataArray ()
    {
      return m_dataArray;
    }

    void DataContainer::examineResponse (const std::vector<const SpectralData*>& spectra, std::ostream& outFile, string& currentDir)
    {

       for (size_t iSpec=0; iSpec<spectra.size(); ++iSpec)
       {
          const SpectralData* spec = spectra[iSpec];      
	  const std::vector<bool>& rspChanged = spec->responseChanged();
	  size_t specNum = spec->spectrumNumber(), nSize = rspChanged.size();

	  for(size_t nSrcNum = 0; nSrcNum < nSize; ++nSrcNum)
          {
	      const Response* t_pRsp = spec->detector(nSrcNum);
	      if(rspChanged[nSrcNum] == true) 
	      {
                  // Since MultiResponses are unimplemented in response command,
                  // they should never get in here.
		  const RealResponse* t_pRealRsp=0;

                  string rspName;
                  if (!t_pRsp)
		      rspName = XSparse::NONE();
                  else
                  {
                      // First check if this a dummy resp.  If so, check
                      // if there's a corresponding real response and use that.
		      if(dynamic_cast<const UserDummyResponse*>(t_pRsp) != 0) {
                         const Response* correspondingResp = spec->responseHooks(nSrcNum);
                         if (correspondingResp)
                         {
                            t_pRealRsp = dynamic_cast<const RealResponse*>(correspondingResp);
                         }
                         else
                         {
                            // Do nothing for stand alone dummy.
                            continue;
                         }
		      }
		      else 
                         t_pRealRsp = dynamic_cast<const RealResponse*>(t_pRsp);

                      if (!t_pRealRsp)
                         throw RedAlert("Response type error during response save operation");
		      rspName = t_pRealRsp->rmfName();

                      if (rspName != XSparse::NONE())
                      {
                         string relativePath = XSparse::absToRelPath(currentDir,t_pRealRsp->rspRunPath());

		         if(relativePath.length())
		         {
			     outFile << "cd " << relativePath << std::endl;
			     currentDir = t_pRealRsp->rspRunPath();

		         }
                      }		   
                  }

		  outFile << "response " << ' ' << nSrcNum + 1 << ':' << specNum 
                      << ' ' << XSutility::insertEscapeChars(rspName) << std::endl;
	      } // end if rspChanged
	   } // end srcNum loop

        } // end spectra loop
    }

    void DataContainer::verifySpectrumRange ()
    {
      // Used by ignore/notice, this will remove any specnums from
      // the m_spectrumRange which are higher than the number of
      // currently loaded spectra.  
      IntegerArray tmpArray;
      size_t sz = m_spectrumRange.size();
      for (size_t i=0; i<sz; ++i)
      {
         if (static_cast<size_t>(m_spectrumRange[i]) <= m_numberOfSpectra)
         {
            tmpArray.push_back(m_spectrumRange[i]);
         }
      }
      m_spectrumRange = tmpArray;
    }

    void DataContainer::bundleSpectra (const IntegerArray& specRanges, std::vector<SpectralData*>& spectra) const
    {
      size_t sz = specRanges.size();
      spectra.clear();
      for (size_t i=0; i<sz; ++i)
      {
         SpectralData* spectrum = lookup(specRanges[i]);
         if (spectrum)
         {
            spectra.push_back(spectrum);
         }
         else
         {
            throw NoSuchSpectrum(specRanges[i]);
         }
      }
    }

    void DataContainer::fixDataGroupGaps ()
    {
      // This function will detect and fix any gaps in the data group
      // numbers of all the loaded datasets.  As an example:
      // dgnums = {1,2,2,2,2,6,1,4,7} will be corrected to:
      // dgnums = {1,2,2,2,2,4,1,3,5}.

      using namespace std;
      vector<size_t> inputDG;
      set<size_t> store;
      map<size_t, size_t> corrections;

      // NOTE: Datasets are not necessarily traversed in any 
      // particular order.  All that matters is that they are 
      // traversed in the same order during the correction stage as
      // they are here.
      DataArray::iterator itDat = m_dataArray.begin();
      DataArray::iterator itDatEnd = m_dataArray.end();
      while (itDat != itDatEnd)
      {
         const DataSet* ds = itDat->second;
         inputDG.push_back(ds->dataGroup());
         ++itDat;
      }
      // NOTE: Because store is not a multiset, duplicate dgnums will
      // not actually be stored.
      size_t sz = inputDG.size();
      for (size_t i=0; i<sz; ++i)
      {
         store.insert(inputDG[i]);
      }
      // Calculate gap values (values to decrement dgnum by)
      // required to close data group gaps, store in corrections
      // map where current dgnums are the keys.
      bool isGap = false;
      size_t prevVal = 0;
      size_t gapVal = 0;
      set<size_t>::const_iterator itStore = store.begin();
      set<size_t>::const_iterator itStoreEnd = store.end();
      while (itStore != itStoreEnd)
      {
         size_t val = *itStore;
         if (val > prevVal+1)
         {
            isGap = true;
            gapVal += val - (prevVal+1);
         }
         if (isGap)
         {
            corrections[val] = gapVal;
         }
         prevVal = val;
         ++itStore;
      }

      if (isGap)
      {
         map<size_t,size_t>::const_iterator itMapEnd = corrections.end();
         size_t newMax = 0;
         itDat = m_dataArray.begin();
         // sz is also the size of m_dataArray (see above).
         for (size_t i=0; i<sz; ++i)
         {
            size_t oldVal = inputDG[i];
            size_t dec = 0;
            map<size_t,size_t>::const_iterator itMap = corrections.find(oldVal);
            if (itMap != itMapEnd)
            {
               dec = itMap->second;
               size_t newVal = oldVal - dec;
               itDat->second->dataGroup(newVal); 
               newMax = std::max(newMax, newVal);
               m_dgHistory.reassign(oldVal, newVal);                             
            }
            else
               newMax = std::max(newMax, oldVal);
            ++itDat;
         }
         tcout << "\n***Warning: Gaps detected in data group numbering.\n"
               << "    Xspec has modified data group numbers to fill in gaps."
               << endl;
         m_numberOfGroups = newMax;
      }
      m_dgHistory.createReverseLookup(m_numberOfGroups);
    }

    void DataContainer::examineArf (const std::vector<const SpectralData*>& spectra, std::ostream& outFile, string& currentDir)
    {
       for (size_t iSpec=0; iSpec<spectra.size(); ++iSpec)
       {
          const SpectralData* spec = spectra[iSpec];
	  const std::vector<bool>& arfChanged = spec->arfChanged();
	  size_t specNum = spec->spectrumNumber(), nSize = arfChanged.size();

	  for(size_t iSrcNum = 0; iSrcNum < nSize; ++iSrcNum) 
          {
	      if(arfChanged[iSrcNum]) 
              {
                  // Can't get here with MultiResponses.
                  const Response* resp = spec->detector(iSrcNum);
                  const RealResponse* rresp = 0;
                  // First check if this a dummy resp.  If so, check
                  // if there's a corresponding real response and use that.
		  if(dynamic_cast<const UserDummyResponse*>(resp) != 0) {
                     const Response* correspondingResp = spec->responseHooks(iSrcNum);
                     if (correspondingResp)
                     {
                        rresp = dynamic_cast<const RealResponse*>(correspondingResp);
                     }
                     else
                     {
                        // Do nothing for stand alone dummy.
                        continue;
                     }
		  }
		  else 
                     rresp = dynamic_cast<const RealResponse*>(resp);

                  if (!rresp)
                  {
                     throw RedAlert("Response type error during arf save operation");
                  }

                  string fileName = rresp->arfName();
                  size_t arfRow = rresp->arfRow();

                  if (!fileName.length())
                     fileName = XSparse::NONE();

		  if(fileName != XSparse::NONE())
		  {
                     string relativePath = XSparse::absToRelPath(currentDir,rresp->arfRunPath());
                     if (relativePath.length())
                     {
	                outFile << "cd " << relativePath << std::endl;
                        currentDir = rresp->arfRunPath();
                     }
		  }

		  outFile << "arf " << iSrcNum + 1 << ':' << specNum
		      << ' ' << XSutility::insertEscapeChars(fileName);

		  if(arfRow)
		      outFile << '{' << arfRow << '}';
                  outFile << std::endl;
	      }
	  }
       } // end spectra loop
    }

    void DataContainer::examineBackCorr (const std::vector<const SpectralData*>& spectra, std::ostream& outFile, bool isBack, string& currentDir)
    {
       for (size_t iSpec=0; iSpec<spectra.size(); ++iSpec)
       {
          const SpectralData* spec = spectra[iSpec];
          bool isChanged = false;

          string fileName, cmd = isBack ? "backgrnd" : "corfile";

          const SpectralData* backCorr=0;
          const BackCorr* ancPtr = 0;

          if (isBack)
          {
             isChanged = spec->backgroundChanged();
             fileName = spec->backgroundFile();
             if (spec->background())
	         ancPtr = spec->background();
          }
          else
          {
             isChanged = spec->correctionChanged();
             fileName = spec->correctionFile();

             if (spec->correction())
	         ancPtr = spec->correction();
          }

          if(ancPtr)
          {
	      backCorr = ancPtr->data();

          }

          if (isChanged)
          {
              if (!isBack)
              {
                 // Unless it's 0.0, save the cornorm setting regardless of whether 
                 // there's a corfile.
                 if (spec->correctionScale() != 0.0)
                 {
                    outFile << "cornorm " << spec->spectrumNumber() << " "
                       << spec->correctionScale() << std::endl;
                 }

              }
	      if(!fileName.length())
	          fileName = XSparse::NONE();

	      if (fileName != XSparse::NONE())
	      {
                 string relativePath = XSparse::absToRelPath(currentDir,ancPtr->runPath());
                 if (relativePath.length())
                 {
	            outFile << "cd " << relativePath << std::endl;
                    currentDir = ancPtr->runPath();
                 }
	      }

	      outFile << cmd << ' ' << spec->spectrumNumber() << ' ' 
                   << XSutility::insertEscapeChars(fileName);

	      // If fileName exists, then so should backCorr pointer. 
	      // This is just a precaution.
	      if (backCorr && backCorr->rowNumber() != 0)
	          outFile << '{' << backCorr->rowNumber() << '}';
              outFile << std::endl;
          }
       }       
    }

    void DataContainer::bundleSpectra ()
    {
      m_bundledSpectra.resize(m_numberOfSpectra,0);
      DataArrayConstIt ds = m_dataArray.begin();
      DataArrayConstIt dsEnd = m_dataArray.end();
      while (ds != dsEnd)
      {
	  DataSet* current = ds->second;
          SpectralData* sd=0;
	  if (!current->isMultiple())
	  {        
	      sd  = current->spectralData();
              size_t specNum = sd->spectrumNumber();
              if (specNum > m_numberOfSpectra || specNum < 1)
              {
                 throw RedAlert("Spectrum enumeration error in bundleSpectra function");
              }
              m_bundledSpectra[specNum-1] = sd;
	  }
	  else
	  {
	      const SpectralDataMap& ssd = current->multiSpectralData();
	      SpectralDataMapConstIt siEnd = ssd.end();
	      SpectralDataMapConstIt si = ssd.begin();
	      while (si != siEnd)
	      { 
		  sd = si->second;
                  size_t specNum = sd->spectrumNumber();
                  if (specNum > m_numberOfSpectra || specNum < 1)
                  {
                     throw RedAlert("Spectrum enumeration error in bundleSpectra function");
                  }
                  m_bundledSpectra[specNum-1] = sd;
		  ++si;
	      } 
	  }
          ++ds;
      }
      
      FunctionUtility::clearXFLT();
      for (size_t i=0; i<m_numberOfSpectra; ++i)
      {
         FunctionUtility::loadXFLT(i+1, m_bundledSpectra[i]->xflt());
      }
    }

    Memento* DataContainer::CreateMemento ()
    {
	static DataMemento* m = 0;

	if(m) 
        {
           emptyTrash();
           delete m;
        }

	m = new DataMemento(m_bundledSpectra.size());

	std::copy(m_bundledSpectra.begin(), 
		  m_bundledSpectra.end(), m->m_spectra.begin());

	return m;
    }

    void DataContainer::SetMemento (Memento const* m)
    {
	std::vector<SpectralData*> result(m_bundledSpectra.size());

	DataMemento const* memento = 0;

	if((memento = dynamic_cast<DataMemento const*>(m)) != 0)
	{
	    std::vector<SpectralData*>::iterator end_result;
	    //an arg of false means don't delete TrashAdapter<T>->m_obj,
	    //just the memory allocated for the adapter itself
	    emptyTrash(false);
	    //set_difference requires two SORTED sets as input.
	    //make a copy of m_bundledSpectra so as not to disturb it's
	    //current ordering. Insertion into a set automatically
	    //sorts. then, sort memento->m_spectra

	    std::set<SpectralData*> 
		mementoSet(memento->m_spectra.begin(), memento->m_spectra.end());
	    std::set<SpectralData*> 
		bundledSet(m_bundledSpectra.begin(), m_bundledSpectra.end());
            // Looks for specs in bundledSet and NOT in mementoSet, and
            // places them in result.
	    end_result = set_difference(bundledSet.begin(), bundledSet.end(),
					  mementoSet.begin(), mementoSet.end(), result.begin());
	    result.resize(end_result - result.begin());

	    //dumps spectra into the trash, but we can remove right after
	    for(size_t i = 0; i < result.size(); ++i)
            {
               size_t specNum = result[i]->spectrumNumber(); 
	       removeNumberedSpectrum(specNum, true);
               renumberSpectra(specNum);
            }
	    emptyTrash();

            adjustNumSources(0);
	    bundleSpectra(); //re-bundle
            // m_bundledSpectra now contains only those spectra that 
            // existed both before and after the most recent data command 
            // call.  However, their spectrum numbers may have changed.  
            // We can reset the original values though simply by finding 
            // the location of the spectrum in the memento->m_spectra vector.
            const std::vector<SpectralData*>& oldSpecs = memento->m_spectra;
            const size_t nOldSpecs = oldSpecs.size();
            for (size_t i=0; i<nOldSpecs; ++i)
            {
               // Careful, bundledSet now may contain dangling pointers 
               // (anything that was just removed above).  But none of 
               // these can be in memento, so this should be OK.
               std::set<SpectralData*>::iterator itPersistent =
                        bundledSet.find(oldSpecs[i]);
               if (itPersistent != bundledSet.end())
               {
                  (*itPersistent)->spectrumNumber(i+1);
               }
            }
            // Now put back any data group numbers that may have been changed.
            DataArrayIt itDs = m_dataArray.begin();
            DataArrayIt dsEnd = m_dataArray.end();
            while (itDs != dsEnd)
            {
               size_t curDgNum = itDs->second->dataGroup();
               size_t orgDgNum = m_dgHistory.getOriginal(curDgNum);
               itDs->second->dataGroup(orgDgNum);
               ++itDs;
            }


	    result.clear();
	    result.resize(mementoSet.size());

	    bundledSet.clear();
	    bundledSet.insert(m_bundledSpectra.begin(), m_bundledSpectra.end());

	    end_result = set_difference(mementoSet.begin(), mementoSet.end(),
					  bundledSet.begin(), bundledSet.end(), result.begin());

	    result.resize(end_result - result.begin());;

	    std::vector<SpectralData*>::const_iterator
		beg_result = result.begin();

	    end_result = result.end();
            int highestNewSource = 1;
	    while(beg_result != end_result)
	    {
		SpectralData* s = *beg_result;
                const std::vector<Response*>& dets = s->detector();
                for (int i=static_cast<int>(dets.size())-1; i>1; --i)
                {
                   if (dets[i])
                   {
                      if (i > highestNewSource)
                         highestNewSource = i;
                      break;
                   }
                }
		reinsertSpectrum(s);

		size_t size = s->detector().size();

		for(size_t i = 0; i < size; ++i)
                {
                   if (s->detector(i))
                   {
                      if (const MultiResponse* mult = s->detector(i)->toMultiResponse())
                      {
                         string allNames;
                         for (size_t iRmf=0; iRmf<mult->rmfNames().size(); ++iRmf)
                            allNames += mult->rmfNames(iRmf) + " ";
                         XSContainer::responses->addToList(allNames,s->detector(i));
                      }
                      else if (const RealResponse* rresp = s->detector(i)->toRealResponse())
                      {
                         XSContainer::responses->addToList(rresp->rmfName(),s->detector(i));
                      }
                      else
                      {
                         XSContainer::responses->addToList(USR_DUMMY_RSP,s->detector(i));
                      }
                   }
                }

		++beg_result;
	    }
            adjustNumSources(static_cast<size_t>(highestNewSource));
            m_dgHistory.swapHistory();
	}

	enumerateSpectra();
        enumerateGroups();
	bundleSpectra();
	countPlotGroups();
    }

    void DataContainer::moveToTrash (TrashCan::value_type ptr)
    {
	m_trash.push_back(ptr);
    }

    void DataContainer::emptyTrash (bool deleteObj)
    {
	TrashCan::iterator
	    beg_trash = m_trash.begin(),
	    end_trash = m_trash.end();

	while(beg_trash != end_trash)
	{
	    if(deleteObj)
		(*beg_trash)->empty();

	    delete *beg_trash;
	    ++beg_trash;
	}

	m_trash.clear();
    }

    void DataContainer::reinsertSpectrum (SpectralData* s)
    {
        DataSet* parent = const_cast<DataSet*>(s->parent());

        bool found = false;

        DataArray::const_iterator
	    beg_data = datasets->dataArray().begin(),
	    end_data = datasets->dataArray().end();

        while (beg_data != end_data && !found)
	    if (beg_data->second == parent)
		found = true;
	    else ++beg_data;

        if (!found)
	    dataArray(parent->dataName(), parent);

        parent->insertSpectrum(s);
    }

    void DataContainer::countPlotGroups ()
    {
	m_numberOfPlotGroups=0;

	for (size_t j=0; j<m_numberOfSpectra; ++j)
	    if (static_cast<size_t>(m_plotGroupNums[j]) > m_numberOfPlotGroups)
	        m_numberOfPlotGroups = m_plotGroupNums[j];
    }

    bool DataContainer::adjustNumSources (size_t addedSourceNum)
    {
       // ASSUME the actual detector insertion or removal occured just 
       // before calling this function.  This DOES NOT require that
       // spectral data is in an up-to-date bundled state.
       // addSourceNum is 1-based.
       bool isChanged = false;
       if (!addedSourceNum)
       {
          // Find out if no more spectra are using the highest numbered source.
          // If so, remove this source from all detector arrays, go to 
          // the next lowest sourceNum and repeat till an actual response
          // is found.
          bool isRespFound = false;
          size_t iNumToCheck = m_numSourcesForSpectra;
          size_t nToRemove = 0;
          while (!isRespFound && iNumToCheck > 1)
          {
             DataArrayIt itDs = m_dataArray.begin();
             DataArrayIt itDsEnd = m_dataArray.end();
             while (!isRespFound && itDs != itDsEnd)
             {
                DataSet* currentDs = itDs->second;
                bool handleSingle = !currentDs->isMultiple();
                //SpectralData* sd = currentDs->spectralData();
                SpectralDataMapConstIt itSd = currentDs->multiSpectralData().begin();
                SpectralDataMapConstIt itSdEnd = currentDs->multiSpectralData().end();
                while (handleSingle || itSd != itSdEnd)
                {
                   SpectralData* sd = handleSingle ? currentDs->spectralData()
                             : itSd->second;
                   const Response* det = sd->detector(iNumToCheck-1);
                   if (det)
                   {
                      isRespFound = true;
                      break;
                   }
                   if (handleSingle)
                      handleSingle = false;
                   else
                      ++itSd;
                }
                ++itDs;
             }
             if (!isRespFound)
                ++nToRemove;
             --iNumToCheck;
          }
          if (nToRemove)
          {
             m_numSourcesForSpectra -= nToRemove;
             XSContainer::responses->adjustNumSources(m_numSourcesForSpectra);
             isChanged = true;
             DataArrayIt itDs = m_dataArray.begin();
             DataArrayIt itDsEnd = m_dataArray.end();
             while (itDs != itDsEnd)
             {
                DataSet* currentDs = itDs->second;
                bool handleSingle = !currentDs->isMultiple();
                //SpectralData* sd = currentDs->spectralData();
                SpectralDataMapConstIt itSd = currentDs->multiSpectralData().begin();
                SpectralDataMapConstIt itSdEnd = currentDs->multiSpectralData().end();
                while (handleSingle || itSd != itSdEnd)
                {
                   SpectralData* sd = handleSingle ? currentDs->spectralData()
                             : itSd->second;
                   sd->reduceNDets(m_numSourcesForSpectra);
                   if (handleSingle)
                      handleSingle = false;
                   else
                      ++itSd;
                }
                ++itDs;
             }
          }
       }
       else // source is inserted
       {
          if (addedSourceNum > m_numSourcesForSpectra)
          {
             m_numSourcesForSpectra = addedSourceNum;
             XSContainer::responses->adjustNumSources(m_numSourcesForSpectra);
             isChanged = true;
             DataArrayIt itDs = m_dataArray.begin();
             DataArrayIt itDsEnd = m_dataArray.end();
             while (itDs != itDsEnd)
             {
                DataSet* currentDs = itDs->second;
                bool handleSingle = !currentDs->isMultiple();
                //SpectralData* sd = currentDs->spectralData();
                SpectralDataMapConstIt itSd = currentDs->multiSpectralData().begin();
                SpectralDataMapConstIt itSdEnd = currentDs->multiSpectralData().end();
                while (handleSingle || itSd != itSdEnd)
                {
                   SpectralData* sd = handleSingle ? currentDs->spectralData()
                             : itSd->second;
                   sd->increaseNDets(m_numSourcesForSpectra);
                   if (handleSingle)
                      handleSingle = false;
                   else
                      ++itSd;
                }
                ++itDs;
             }
          }
       }
       return isChanged;
    }

    void DataContainer::determineDgSourceRelations ()
    {
       using namespace std;
       // This function IS GUARANTEED to place an entry into m_dgToSources
       // for every group up through m_numberOfGroups (even if container
       // is empty).  It is NOT GUARANTEED to place an entry into
       // inverse m_sourceToDgs map for every possible source.  Therefore
       // an iterator obtained with the find function should be checked
       // before it is used. 
       m_dgToSources.clear();
       m_sourceToDgs.clear();

       m_dgToSources[1] = set<size_t>();

       DataArrayConstIt itDataSet = m_dataArray.begin();
       DataArrayConstIt itDataEnd = m_dataArray.end();
       while (itDataSet != itDataEnd)
       {
          const DataSet* dSet = itDataSet->second;
          size_t groupNum = dSet->dataGroup();
          set<size_t>& sourceNums = m_dgToSources[groupNum];
          bool usingAllSources = (sourceNums.size() == m_numSourcesForSpectra);
          if (dSet->isMultiple())
          {
             map<size_t,SpectralData*>::const_iterator itSpec = 
                        dSet->multiSpectralData().begin();
             map<size_t,SpectralData*>::const_iterator itSpecEnd = 
                        dSet->multiSpectralData().end();
             while (!usingAllSources && itSpec != itSpecEnd)
             {
                const vector<Response*>& dets = itSpec->second->detector();
                const size_t nDets = dets.size();
                for (size_t i=0; !usingAllSources && i<nDets; ++i)
                {
                   if (dets[i])
                   {
                      sourceNums.insert(i+1);
                      usingAllSources = 
                                (sourceNums.size() == m_numSourcesForSpectra);
                   }
                }
                ++itSpec;
             }
          }
          else
          {
             const vector<Response*>& dets = 
                        dSet->spectralData()->detector();
             const size_t nDets = dets.size();
             for (size_t i=0; !usingAllSources && i<nDets; ++i)
             {
                if (dets[i])
                {
                   sourceNums.insert(i+1);
                   usingAllSources = 
                             (sourceNums.size() == m_numSourcesForSpectra);
                }
             }
          }
          ++itDataSet;
       }

       // m_dgToSources is now completed, use it to make the inverse map.
       map<size_t,set<size_t> >::const_iterator itGroup = m_dgToSources.begin();
       map<size_t,set<size_t> >::const_iterator itGroupEnd = m_dgToSources.end();
       while (itGroup != itGroupEnd)
       {
          set<size_t>::const_iterator itSources = itGroup->second.begin();
          set<size_t>::const_iterator itSourcesEnd = itGroup->second.end();
          size_t groupNum = itGroup->first;
          while (itSources != itSourcesEnd)
          {
             size_t sourceNum = *itSources;
             map<size_t,size_t>& groups = m_sourceToDgs[sourceNum];
             // The value part of the entry will represent the sorted position of
             // each group number.  Set it to 0 for now and fill in during later section.
             groups.insert(make_pair(groupNum, size_t(0)));
             ++itSources;
          }
          ++itGroup;
       }
       // Now fill in sorted position of each group number.  This is particularly
       // useful for parameter indexing among multiple data group model objects.
       map<size_t,map<size_t,size_t> >::iterator itSource = m_sourceToDgs.begin();
       map<size_t,map<size_t,size_t> >::iterator itSourceEnd = m_sourceToDgs.end();
       while (itSource != itSourceEnd)
       {
          map<size_t,size_t>::iterator itGroup = itSource->second.begin();
          map<size_t,size_t>::iterator itGroupEnd = itSource->second.end();
          // Making group position 1-BASED:
          size_t groupPos = 1;
          while (itGroup != itGroupEnd)
          {
             itGroup->second = groupPos;
             ++groupPos;
             ++itGroup;
          }
          ++itSource;
       }
    }

    size_t DataContainer::getLowestGroupForSource (size_t source) const
    {
       size_t lowestGroup = 0;
       std::map<size_t,std::map<size_t,size_t> >::const_iterator itSource = 
                m_sourceToDgs.find(source);
       if (itSource != m_sourceToDgs.end())
       {
          std::map<size_t,size_t>::const_iterator itGroup = 
                itSource->second.begin();
          if (itGroup != itSource->second.end())
             lowestGroup = itGroup->first;
       }
       return lowestGroup;
    }

    size_t DataContainer::getNumberOfGroupsForSource (size_t sourceNum) const
    {
       size_t nGroups = 0;
       std::map<size_t,std::map<size_t,size_t> >::const_iterator itSource = 
                m_sourceToDgs.find(sourceNum);
       if (itSource != m_sourceToDgs.end())
       {
          nGroups = itSource->second.size();
       }
       return nGroups;
    }

    void DataContainer::showData (bool isAll) const
    {
       using namespace std;
       if (m_dataArray.empty())
       {
          tcout << "\n No Spectra defined." << endl;
       }
       else
       {
          string filx =  
                  m_dataArray.size() > 1 ? " files " : " file ";
          string 
                  spec =  m_numberOfSpectra > 1 ? " spectra " : " spectrum ";
          tcout << '\n' << m_dataArray.size() << filx
                << m_numberOfSpectra << spec << endl;
          for (size_t i=1; i<=m_numberOfSpectra; ++i)
          {
             const SpectralData* sd = lookup(i);
             sd->report(false);
             if (isAll)
             {
                tcout << " Spectral data counts: " 
                      << sd->totalFlux()*sd->exposureTime() << endl;
                XSContainer::models->reportModelRate(sd);
                tcout << endl;
             }
          }
          tcout << flush;
       }
    }

    void DataContainer::examineResponseFunctions (const SpectralData* spectrum, std::ostringstream& respFuncs, std::ostringstream& respParLinks)
    {
       const size_t specNum = spectrum->spectrumNumber();
       for (size_t iSource=0; iSource < spectrum->detector().size(); ++iSource)
       {
          const Response* resp = spectrum->detector(iSource);
          if (resp)
          {
             const ResponseParam* linearGain = resp->getLinearGain();
             const ResponseParam* constGain = resp->getConstGain();
             if (linearGain && constGain)
             {
                // gain fit is on
                const ResponseParam* respPars[2];
                respPars[0] = linearGain;
                respPars[1] = constGain;
                respFuncs << "gain fit " << iSource+1 <<":"<< specNum <<"\n";
                for (size_t iPar=0; iPar<2; ++iPar)
                {
                   if (respPars[iPar]->isLinked())
                   {
                      respFuncs << "/\n";
                      respParLinks << "rnewpar " << resp->sourceNumber() << ":"
                         << respPars[iPar]->index() << " " 
                         << respPars[iPar]->parameterSetting() <<"\n";
                   }
                   else if (XSContainer::models->proportionalDelta() > 0.0)
                   {
                      // Need to remove parenthetical portion of string.
                      // This is a patch fix that should really go into a new
                      // function, as it duplicates code from ModelContainer's
                      // saveData function.
                      string parInfo(respPars[iPar]->parameterSetting());
                      string::size_type startLoc = parInfo.find('(');
                      if (startLoc != string::npos)
                      {
                         string::size_type endLoc = parInfo.find(')',startLoc);
                         if (endLoc == string::npos)
                         {
                            string errMsg("Unbalanced parentheses detected while saving string:\n    ");
                            errMsg += parInfo;
                            errMsg += "\n";
                            throw YellowAlert(errMsg);
                         }
                         parInfo.erase(startLoc, endLoc-startLoc+1);
                      }
                      respFuncs << parInfo << '\n';
                   }
                   else
                      respFuncs << respPars[iPar]->parameterSetting() << "\n";
                }
             }
             else if (resp->isGainApplied())
             {
                const std::vector<Real>& gainCoeffs = resp->gainFactor();
                respFuncs << "gain " << iSource+1 <<":"<< specNum <<"  "
                    << gainCoeffs[1] <<"  "<< gainCoeffs[0] <<"\n";
             }
          }
       }
    }

    void DataContainer::setPlotGroupNums (size_t index, int value)
    {
	m_plotGroupNums[index] = value;
	lookup(index+1)->plotGroup(value);
    }

    // Additional Declarations

} // namespace XSContainer
