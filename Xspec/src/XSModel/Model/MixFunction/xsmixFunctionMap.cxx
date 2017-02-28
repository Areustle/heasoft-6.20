
#include    <xsmixFunctionMap.h>

#include    <XSModelFunction.h>
#include    <project.h>
#include    <ascaCluster.h>
#include    <suzaku.h>
#include    <xmmc.h>

void 
createxsmixFunctionMap()
{


	XSFunctionMap["ascac"]    = new XSCall<AscaCluster>(0);
	XSFunctionMap["xmmpsf"]   = new XSCall<Xmmc>(0);
        XSFunctionMap["suzpsf"]   = new XSCall<SuzakuMix>(0);
        XSFunctionMap["recorn"]   = new XSCall<XSCCall>(recorn);
        XSFunctionMap["projct"]   = new XSCall<Project>(0);

}
