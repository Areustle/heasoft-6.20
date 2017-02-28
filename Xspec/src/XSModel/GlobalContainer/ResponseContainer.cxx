//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <sstream>

// DummyResponse
#include <XSModel/Data/Detector/DummyResponse.h>
// XSutility
#include <XSUtil/Utils/XSutility.h>
// ResponseContainer
#include <XSModel/GlobalContainer/ResponseContainer.h>

#include <XSModel/Data/Detector/MultiResponse.h>
#include <XSModel/Data/Detector/RealResponse.h>
#include <XSModel/Data/Detector/UserDummyResponse.h>
#include <XSModel/Parameter/ParameterLink.h>
#include <XSModel/Parameter/ResponseParam.h>
#include <XSUtil/Utils/XSstream.h>
#include <XSstreams.h>
#include <XSContainer.h>
#include <XSsymbol.h>
#include <iomanip>
#include <iostream>

namespace XSContainer {
     ResponseContainer* responses = 0;

    // Class XSContainer::ResponseContainer 
    ResponseContainer* ResponseContainer::s_instance = 0;

    ResponseContainer::ResponseContainer (DummyResponse* dummy)
       :m_respParContainer(1),
       m_respParIndexOffsets(1),
       m_totalResponseParams(0)
    {
      m_responseList.insert(ResponseMap::value_type(DUMMY_RSP,dummy));

      XSstream& xscout = static_cast<XSstream&>(tcout);
      if (std::min(xscout.consoleChatterLevel(),xscout.logChatterLevel()) > 25)
      { 
            tcout << "ResponseContainer Initialization:  eMin: " << dummy->eLow() << " eMax: " 
                            << dummy->eHigh() <<  " nE: " << dummy->numEnergies() 
                            << " ln: ? " << std::boolalpha << dummy->isLog() << std::endl;
      }
    }


    ResponseContainer::~ResponseContainer()
    {
    }


    ResponseContainer* ResponseContainer::Instance (DummyResponse* dummy)
    {
      if (s_instance == 0) {s_instance = new ResponseContainer(dummy);} return s_instance;

    }

    void ResponseContainer::addToList (const string& name, Response* newResponse)
    {

          // supplying a released auto_ptr would be a better way perhaps.

          responseList(name,newResponse); 
    }

    void ResponseContainer::addToList (const RefCountPtr<ResponseMatrix>& newRMF)
    {
        RMFmap(newRMF->name(), RefCountPtr<ResponseMatrix>(newRMF));
    }

    const Response* ResponseContainer::responseList (const string& name, size_t index) const
    {
        return internalFind(name,index);      
    }

    Response* ResponseContainer::responseList (const string& name, size_t index)
    {
        return internalFind(name,index);
    }

    void ResponseContainer::responseList (const string& name, Response* value)
    {
        m_responseList.insert(ResponseMap::value_type(name,value));
    }

    void ResponseContainer::RMFmap (const string& name, const RefCountPtr<ResponseMatrix>& rmf)
    {
        m_RMFmap.insert(RMFMap::value_type(name,rmf));
    }

    size_t ResponseContainer::numberOfResponses () const
    {

      return m_responseList.size();
    }

    void ResponseContainer::removeByToken (const std::vector<Response*>& doomed, size_t sourceNum)
    {
        size_t n(doomed.size());

	// If sourceNum is 0 (default), remove all responses in the vector,
	// else just remove that specified by sourceNum.
        for (size_t j = (sourceNum==0 ? 0 : sourceNum-1); j < n; ++j)
        {                
           ResponseMapIter d(m_responseList.begin());
	   ResponseMapIter dEnd(m_responseList.end());
           while (d != dEnd)
           {
              Response* current(d->second);
              if (current == doomed[j]) 
              {
                 // No longer calling delete on current.  The SpectralData
                 // obj and NOT ResponseContainer will now be responsible
                 // for Response memory (Jan 06).

                 //remove from list of RMFs.
                 m_responseList.erase(d);
                 // Also delete any response params it may have.  We don't
                 // want these hanging around in the TrashCan.  (Undo will
                 // restore them by calling the gain command anew.) This
                 // also causes response params to be removed from the
                 // global list.
                 current->removeGain();
                 break;       
              } 
              else ++d;
           }
	   if (sourceNum != 0) break;
        }
    }

    void ResponseContainer::removeByToken (const RefCountPtr<ResponseMatrix>& doomed) throw ()
    {

          // associative container erase functions are guaranteed  by
          // the standard not to throw.   This function will be called by
          // RealResponse's dtor.   
          RMFMapIter m(m_RMFmap.begin());
          RMFMapIter mEnd(m_RMFmap.end());
          while ( m != mEnd )
          {
                  RefCountPtr<ResponseMatrix>& current( (*m).second);
	          if ( *current == *doomed )
	          {
                          m_RMFmap.erase(m);
                          break;
	          }	
	          else ++m;
          }
    }

    Response* ResponseContainer::internalFind (const string& name, size_t index) const
    {
          size_t N = m_responseList.count(name);

          if (N == 0) return 0;
          else 
          {
                ResponseMapConstIter f = m_responseList.lower_bound(name);       
                if (N == 1)
                {
                        return (*f).second;
                }      
                else
                {
                        ResponseMapConstIter g = m_responseList.upper_bound(name);
                        while ( f != g)
                        {
                                if ( ((*f).second)->index()  != index )
				{
				    ++f;
				}
				else
				{
                                   return (*f).second; 
				}
                        }
                        return 0;
                }

          }      
    }

    Response* ResponseContainer::swapResponses (Response* inResponse, Response* outResponse)
    {
      ResponseMapIter itResp(m_responseList.begin());
      ResponseMapIter itRespEnd(m_responseList.end());
      bool isFound = false;
      while (itResp != itRespEnd)
      {
         if (itResp->second == outResponse)
	 {
            // Must reconstruct the global map name key for the inReponse:
            string nameKey;
            if (const RealResponse* rr = inResponse->toRealResponse())
               nameKey = rr->rmfName();
            else if (inResponse->toUserDummyResponse())
               nameKey = USR_DUMMY_RSP;
            else if (const MultiResponse* mr = inResponse->toMultiResponse())
            {
               for (size_t i=0; i<mr->rmfNames().size(); ++i)
                  nameKey += mr->rmfNames()[i] + " ";
            }

            m_responseList.insert(ResponseMap::value_type(nameKey,inResponse));
            m_responseList.erase(itResp);

            outResponse->deregisterResponseParams();
            inResponse->registerResponseParams();

	    isFound = true;
	    break;
	 }
	 ++itResp;
      }
      return isFound ? outResponse : static_cast<Response*>(0);
    }

    void ResponseContainer::clearGainParameters ()
    {

      // Intended to remove all gain pointers from respParContainer AND 
      // destroy the gain parameters themselves.

      // For future implementations, ASSUME that not every pointer is a
      // gain pointer (even if that's the case for now).  

      // This algorithm can result in redundant calls to a particular 
      // Response's removeGainParams function, but if we assume that only 
      // a small subset of Responses have ResponseParams then this is 
      // quicker than stepping directly through the list of all stored Responses.
      // (This could most benefit from keeping a container which stores only
      // those Responses containing parameters, but it doesn't seem worth
      // the additional bookkeeping overhead.)

      if (m_totalResponseParams)
      {
         for (size_t i=0; i<m_respParContainer.size(); ++i)
         {
            std::vector<ResponseParam*>& parsForSource = m_respParContainer[i];
            size_t j=0;
            while (parsForSource.size() && j < parsForSource.size())
            {
               Response* resp = parsForSource[j]->responseParent();
               if (resp->getConstGain())
               {
                  // This reduces the size of parsForSource by 2, and
                  // j will now point to a different parameter (if any)
                  resp->removeGainParams();
               }
               else
                  ++j;
            }
         }
      }
    }

    void ResponseContainer::rerouteBrokenParLinks (std::set<Parameter*>& doomedPars) const
    {

       // ASSUME all params in respParContainer are persistent, so there should
       // be no overlap between doomedPars and respParContainer pointers.

       // The isModelDoomed flag is checked by findPersistentLink, and was
       // originally named as such for dealing with ModParams.  isParDoomed
       // would be a more accurate description.
       std::set<Parameter*>::iterator itPar = doomedPars.begin();
       std::set<Parameter*>::iterator itEnd = doomedPars.end();
       while (itPar != itEnd)
       {
          (*itPar)->isModelDoomed(true);
          ++itPar;
       }

       std::map<Parameter*,Parameter*> processedDoomedPars;
       while (!doomedPars.empty())
       {
          Parameter::findPersistentLink(*doomedPars.begin(),doomedPars,
                        processedDoomedPars);
       }

       for (size_t iSource=0; iSource<m_respParContainer.size(); ++iSource)
       {
          const std::vector<ResponseParam*>& parsForSource = m_respParContainer[iSource];
          for (size_t i=0; i<parsForSource.size(); ++i)
          {
             ResponseParam* respPar = parsForSource[i];
             if (respPar->isLinked())
             {
                std::vector<Parameter*> correctedToPars;
                bool correctionMade = false;
                bool removeLink = false;
                const std::vector<const Parameter*>& toPars = respPar->thisLink()->members();
                for (size_t j=0; !removeLink && j<toPars.size(); ++j)
                {
                   Parameter* toPar = const_cast<Parameter*>(toPars[j]);
                   if (toPar->isModelDoomed())
                   {
                      Parameter* reRouted = processedDoomedPars.find(toPar)->second;
                      if (!reRouted)
                      {
                         removeLink = true;
                         respPar->untie(true);
                      }
                      else
                      {
                         correctedToPars.push_back(reRouted);
                         correctionMade = true;
                      }
                   }
                   else
                   {
                      correctedToPars.push_back(toPar);
                   }
                }
                if (!removeLink && correctionMade)
                   respPar->thisLink()->rerouteLink(correctedToPars);
             } // end if linked
          } // end parsForSource loop
       } // end source loop
    }

    void ResponseContainer::adjustNumSources (size_t nSources)
    {
       // Since this can only remove entire batch of parameters associated
       // with a source, no reindexing is ever required.
       m_respParContainer.resize(nSources);
       m_respParIndexOffsets.resize(nSources);
    }

    void ResponseContainer::reindexResponseParams (size_t sourceNum, size_t specNum, size_t iRespStart, int nChanged)
    {

       // Actually do 2 things here: reindex all affected response params, and
       // reset the values in the offsets map.

       // KEY ASSUMPTION: nChanged ResponseParam pointers belonging to the Response
       // at sourceNum:specNum have just been inserted or removed from respPar
       // container.  

       const bool isRemoval = nChanged < 0; 
       IndexOffsetMap& offsetsForSource = m_respParIndexOffsets[sourceNum-1];
       IndexOffsetMap::iterator itOffset = offsetsForSource.find(specNum);
       IndexOffsetMap::iterator itEnd = offsetsForSource.end();
       if (itOffset == itEnd)
       {
          itOffset = offsetsForSource.insert(std::make_pair(specNum,iRespStart)).first;
       }

       // itOffset must be pointing to the entry for the affected spectrum.

       const size_t unNChanged = isRemoval ?  static_cast<size_t>(-1*nChanged) 
                                        : static_cast<size_t>(nChanged); 
       if (isRemoval)
       {
          // If the spectrum is losing all its parameters for this response,
          // then take it out of the map.
          IndexOffsetMap::iterator itNextSpec(itOffset);
          ++itNextSpec;
          if (itNextSpec == itEnd)
          {
             if (m_respParContainer[sourceNum-1].size() == itOffset->second)
                offsetsForSource.erase(itOffset);
          }
          else if ((itNextSpec->second - itOffset->second) == unNChanged)
          {
             offsetsForSource.erase(itOffset);
          }

          while (itNextSpec != itEnd)
          {
             itNextSpec->second -= unNChanged;
             ++itNextSpec;
          }
       }
       else
       {
          ++itOffset;
          while (itOffset != itEnd)
          {
             itOffset->second += unNChanged;
             ++itOffset;
          }
       }

       // Now comes the actual reindexing, which is the easy part.

       std::vector<ResponseParam*>& respPars = m_respParContainer[sourceNum-1];
       const size_t nPars = respPars.size();
       for (size_t i=iRespStart; i<nPars; ++i)
          respPars[i]->index(i+1);
    }

    void ResponseContainer::addToRespParContainer (const Response* resp, size_t startIdx, const std::vector<ResponseParam*>& respParams)
    {
       const size_t sourceNum = resp->sourceNumber();
       const size_t specNum = resp->spectrumNumber();
       std::vector<ResponseParam*>& parsForSource = m_respParContainer[sourceNum-1];
       const IndexOffsetMap& offsetsForSource = m_respParIndexOffsets[sourceNum-1];

       // Find the current starting location in the global vector for this 
       // Response's params. If not found in the index offset map, it takes 
       // the offset from the next spectrum or is placed at the end of the vector.
       size_t iRespStart=0;
       IndexOffsetMap::const_iterator itOffsets = offsetsForSource.find(specNum);
       IndexOffsetMap::const_iterator itEnd = offsetsForSource.end();
       if (itOffsets == itEnd)
       {
          IndexOffsetMap::const_iterator itNextSpec = 
                        offsetsForSource.upper_bound(specNum);
          if (itNextSpec == itEnd)
             iRespStart = parsForSource.size();
          else
             iRespStart = itNextSpec->second;
       }
       else
          iRespStart = itOffsets->second; 

       if (iRespStart + startIdx > parsForSource.size())
          throw RedAlert("Response parameter insert indexing error.\n");   

       // While vectors aren't very efficient for inserting/erasing in 
       // the middle, I decided to do it this way anyhow because the 
       // random-access is useful in other contexts, and because the 
       // reindexing which follows is still bound by O(N). In fact
       // if this were a map, the reindexing would be O(NlogN).    

       parsForSource.insert(parsForSource.begin()+iRespStart+startIdx,
                respParams.begin(), respParams.end());
       m_totalResponseParams += respParams.size();         
       reindexResponseParams(sourceNum,specNum,iRespStart,respParams.size());
    }

    void ResponseContainer::removeFromRespParContainer (const Response* resp, size_t startIdx, size_t nPars)
    {
       const size_t sourceNum = resp->sourceNumber();
       const size_t specNum = resp->spectrumNumber();
       std::vector<ResponseParam*>& parsForSource = m_respParContainer[sourceNum-1];
       const IndexOffsetMap& offsetsForSource = m_respParIndexOffsets[sourceNum-1];

       // Find the current starting location in the global vector for this 
       // Response's params. If not found in the index offset map, throw.
       size_t iRespStart=0;
       IndexOffsetMap::const_iterator itOffsets = offsetsForSource.find(specNum);
       IndexOffsetMap::const_iterator itEnd = offsetsForSource.end();
       if (itOffsets == itEnd)
       {
          // This can occur if a spectrum is deleted (or the program is exiting)
          // while its Response contains ResponseParams AND the Response has been
          // pushed aside by a dummyrsp.  When the real Response resides on the
          // dummyrsp "hook", its ResponseParams are NOT in the global respParContainer.
          // This condition doesn't arise when Response is in the trash, since in
          // that case its ResponseParams are deleted prior to going into the trash.
          return;
       }
       else
          iRespStart = itOffsets->second; 

       if (iRespStart + startIdx + nPars > parsForSource.size())
          throw RedAlert("Response parameter erase indexing error.\n");   

       // Store doomed pars in a set which is needed by Parameter's
       // findPersistentLink function.
       std::set<Parameter*> doomedPars;
       const size_t beginDoomed = iRespStart+startIdx;
       const size_t endDoomed = beginDoomed+nPars;
       for (size_t i=beginDoomed; i<endDoomed; ++i)
          doomedPars.insert(parsForSource[i]);

       // This must be called PRIOR to the rerouteBrokenParLinks function,
       // as that assumes all pointers in RespParContainer are persistent.     
       parsForSource.erase(parsForSource.begin()+beginDoomed,
                parsForSource.begin()+endDoomed);
       m_totalResponseParams -= nPars;
       rerouteBrokenParLinks(doomedPars);

       reindexResponseParams(sourceNum,specNum,iRespStart,-1*static_cast<int>(nPars));
    }

    void ResponseContainer::reportResponseParams () const
    {
       const size_t linelen(72);
       const string top(linelen,'=');
       const string bot(linelen,'_');
       for (size_t iSource=0; iSource<m_respParContainer.size(); ++iSource)
       {
          const std::vector<ResponseParam*>& respParsForSource = 
                        m_respParContainer[iSource];
          const size_t nPars = respParsForSource.size();
          if (nPars)
          {
             tcout << '\n' << top
                << "\nSource No.: " << iSource+1
                << "\nRpar Spectrum Rmodel   Rpar_name  Unit   Value\n" 
                << std::endl;
             for (size_t iPar=0; iPar<nPars; ++iPar)
             {
                tcout << *respParsForSource[iPar];
             }
             tcout << bot << std::endl;
          }
       }
       tcout << std::endl;
    }

    void ResponseContainer::renumberSpectrum (const size_t start, const size_t offset)
    {
       // start is 1-based.  The spectrum which had this number and whose
       // removal is the reason for this call, must have been removed from
       // the IndexOffsetMap BEFORE getting here.
       if (start < offset)
          throw RedAlert("Invalid spectrum offset sent to ResponseContainer::renumberSpectrum.");

       for (size_t iSource=0; iSource<m_respParIndexOffsets.size(); ++iSource)
       {
          IndexOffsetMap& parsForSource = m_respParIndexOffsets[iSource];
          IndexOffsetMap::iterator itEnd = parsForSource.end();
          IndexOffsetMap::iterator itSpec = parsForSource.lower_bound(start);
          // itSpec ought to point to the lowest spec num higher than start.
          if (itSpec != itEnd && itSpec->first == start)
             throw RedAlert("Spectrum number error in ResponseContainer::renumberSpectrum.");
          while (itSpec != itEnd)
          {
             // Lower every key larger than start by offset.  Do this by
             // erasing the old key then inserting the new key.  For a
             // reminder regarding iterator validity, see 23.1.2.8 in the
             // '98 standard.
             size_t oldNum = itSpec->first;
             size_t newNum = oldNum - offset;
             std::pair<size_t,size_t> updatedEntry(newNum,itSpec->second);             
             parsForSource.erase(itSpec++);
             parsForSource.insert(updatedEntry);
          }
       }
    }

    ResponseMap& ResponseContainer::responseList ()
    {
      return m_responseList;
    }

    // Additional Declarations

} // namespace XSContainer
