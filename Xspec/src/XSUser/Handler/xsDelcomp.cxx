//
//  XSPEC12  November 2003
//
//

#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/Model/Model.h>
#include <XSModel/Model/Component/Component.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSContainer.h>
#include <XSstreams.h>
#include <xsTypes.h>



int
XSGlobal::xsDelcomp(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doDelcomp(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int
XSGlobal::doDelcomp(const StringArray& rawArgs)
{
    using XSContainer::models;
    static RangePair prevRange(1,1);
    const char* cmd = "delcomp";
    const size_t nArgs = rawArgs.size();
    if ( nArgs == 1 )
    {
	XSGlobal::printDocs(cmd,"?");      
	return 0;
    }
    else 
    {
	const string arg(rawArgs[1]);
	if ( arg[0] == '?')
	{
	    XSGlobal::printDocs(cmd,"?");      
	    return 0;      
	}    
	else
	{
	    string modelName;
            string rangeStr;

            // This command is set up to handle only one range, so anything beyond
            // the first arg (or first comma in first arg) is ignored.
            string::size_type colPos = arg.find(':');
            if (colPos != string::npos)
            {
               modelName = arg.substr(0,colPos);
               rangeStr = arg.substr(colPos+1);
            }
            else
               rangeStr = arg;
            string::size_type commaPos = rangeStr.find(',');
            if (commaPos != string::npos)
               rangeStr = rangeStr.substr(0, commaPos);            

	    try 
	    {
               // only 1 range arg in this case
               StringArray inputArgs(1,rangeStr);            
	       std::vector<Model*> modelsToBeEdited (models->lookupModelGroup(modelName));	    
	       const size_t N = modelsToBeEdited.size();
	       if (!N) 
	       {
		   string msg;
		   if ( modelName.length())
		   {
		       msg = string("Model not defined: ");
		       msg += modelName;           
		   }
                   else
                      msg = string("Unnamed model not defined");
		   throw YellowAlert (msg);
	       }
               if (!modelName.length())
                  modelName = Model::DEFAULT();
	       const size_t numComponents = modelsToBeEdited[0]->numberOfComponents();
               const RangePair limits(1,numComponents); 
               // this can throw              
	       IntegerArray components = XSparse::getRanges(inputArgs,prevRange,limits);
	       const size_t numComponentsToDelete = components.size();
               if (numComponentsToDelete)
               {
	          if(numComponentsToDelete >= numComponents)
		      throw YellowAlert("Cannot remove all model components model. Use 'model clear' instead.");

                  // If this operation changes the model's spectrum dependency
                  // state, it will require an additional layer of updating
                  // to redo the model's unique energy container.
                  const bool origSpecDep = modelsToBeEdited[0]->areCompsSpecDependent();

                  // Since only 1 range went into getRanges, components array must
                  // be sequentially numbered, no gaps.  Therefore we can keep 
                  // calling delete on the same componentNumber.
                  const size_t componentNumber = components[0];
                  for (size_t i=0; i<numComponentsToDelete; ++i)
                  {
                     for (size_t j=0; j<N; ++j)
		          modelsToBeEdited[j]->deleteComponent(componentNumber);                         
                  }
                  bool specDep = modelsToBeEdited[0]->areCompsSpecDependent();

                  if (specDep != origSpecDep)
                  {
                     // Can no longer trust the UniqueEnergy setup.  Redo by
                     // reattaching all responses.  This is actually overkill since
                     // it operates on all loaded models, but as yet there is no
                     // intermediate updating function.
                     XSContainer::models->Update();               
                  }
                  else
                  {
                     // force computation of the models just edited.
	             XSContainer::models->setCompute(modelName);
                     if (!modelsToBeEdited[0]->isActive())
                     {
                        // For inactive models, need to initialize calculation
                        // here since it won't be done through Fit::Update.
                        std::vector<Component*> remainingComps;
                        modelsToBeEdited[0]->bundleComponents(remainingComps);
                        for (size_t i=0; i<remainingComps.size(); ++i)
                        {
                          // Perhaps this call isn't necessary for delcomp as
                          // it is for addcomp, editmod, newpar, but it can't
                          // hurt.
                           remainingComps[i]->initializeForFit();
                        }
                        XSContainer::models->calculate(modelName);
                     }
	             XSContainer::models->Notify();
                  }
                  modelsToBeEdited[0]->printHeading();
	          for (size_t j = 0; j < N ; ++j)
	          {
		      tcout << *modelsToBeEdited[j] << std::endl;    
	          }
                  modelsToBeEdited[0]->printMixComp();
                  tcout << string(72,'_') << std::endl; 
               }
	       return  0;
	    }   
	    catch (YellowAlert&)
	    {
                // deleteComponent will throw if the seek on componentNumber
                // fails, in which case the model will be unchanged.

                // will throw InvalidModelEdit if the model after deletion
                // of the component fails the context checks.
		return -1;                               
	    }  
	}   
    }
}
