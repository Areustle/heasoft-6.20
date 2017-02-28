//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// XSModelFunction
#include <XSUtil/FunctionUtils/XSModelFunction.h>
// sstream
#include <sstream>
#include <fstream>

ModelFunctionMap  XSFunctionMap;


// Class XSModelFunction::NoSuchComponent 

XSModelFunction::NoSuchComponent::NoSuchComponent()
   : YellowAlert()  
{
  // probably a gratuitous constructor...
}


// Class XSModelFunction 
NameCacheType XSModelFunction::s_nameCache;
ParInfoContainer XSModelFunction::s_parInfoCache;

void XSModelFunction::updateComponentList (const string& modelDatFile, bool isStandard)
{
  // This function sets up a cache of model component attributes for 
  // use with the model command. It reads information from modelDatFile and
  // saves ComponentInfo records as follows:

  // ComponentInfo:  name, modelDatFile, location, type

  // initial use of this function is to initialize the entries in the model.dat
  // file. It should also be possible to treat user models as load module addins
  // using this technique [this is the reason for the otherwise redundant model.dat
  // file argument. Cache lookup is provided by nameCache and fullName, which
  // are complicated by the fact that models sometimes begin with the same
  // few letters. I chose to resolve this by using a multimap and storing entries
  // against two letter keys, as the least messy way of resolving abbreviations
  // that users use all the time.


  using namespace std;

  ifstream modfile;
  modfile.open(modelDatFile.c_str());

  if (!modfile) 
  {
        string message = "Cannot open model definition file: " + modelDatFile;
        throw RedAlert(message);
  }
  size_t location(0);
  string line("");
  string prevline("");
  size_t linesRead(0);

  /*    the model.dat format consists of stanzas such as 
        bbody          1   1.e-20     1.e20          xsblbd    add  0
        kT      keV     3.0   1.e-4   1.e-2   100.      200.      0.01

        which are delimited by blank lines. The easiest way of telling we have
        a new entry is that the previous line was blank.
  */
  size_t nPar(0);
  Real upper(0);
  Real lower(0);
  string functionName("");
  string type("");
  string name("");
  int errorFlag(0);
  string infoString("");
  while (!modfile.eof())
  {
        // prevline && line are blank initially. after some reading has taken place
        // we look for the case where prevline is blank and line is not.
        location = modfile.tellg();
        getline(modfile,line);
        ++linesRead;
        const string WS(" \t\n\r");
        if ((prevline.find_first_not_of(WS) == string::npos) && line.length() != 0)
        {
                istringstream s(line);  
                s >>  name >> nPar >> upper >> lower >> functionName >> type 
                                        >> errorFlag; 
                if (!(s >> infoString) && s.eof())
                {
                        s.clear();
                }
              // formatted read first six arguments.
                try
                {
                        s.exceptions( ios_base::badbit | ios_base::failbit );
                        // likely to be empty, but that case ought to be harmless.
                        bool error(errorFlag != 0);
                        // If an already existing component record has the same 
                        // full name, then remove it.
                        NameCacheType::iterator match = exactMatch(name);
                        if (match != s_nameCache.end())
                        {
                           s_nameCache.erase(match);
                        }

                        nameCache(XSutility::lowerCase(name.substr(0,2)),
                               ComponentInfo(name,modelDatFile,location,type,error,
                                infoString,isStandard));
                }
                catch ( ios_base::failure & )
                {
                       if ( s.bad() )
                       {
                                ostringstream msg;
                                msg << "In line: " << linesRead << ": " << line 
                                        <<" model definition file has fatal problem\n"
                                        <<" previous line: " << prevline << '\n';
                                throw RedAlert(msg.str());
                        }
                        if (s.fail())
                        {
                                // support for other formats in which the fields
                                // are different. Can finally throw a RedAlert if
                                // it doesn't conform to anything.       
                        }

                }
        }


        prevline = line;       
  }
}

void XSModelFunction::printComponentList (std::ostream& s, const string& name)
{
   using namespace std;
   // this code is not state of the art regarding flexibility for
   // future expansion but it is expected that adding model types will
   // be a rare occurrence.    
   static vector<string> addMods;
   static vector<string> mulMods;
   static vector<string> conMods;
   static vector<string> mixMods;
   static vector<string> acnMods;     
   static vector<string> amxMods;     
   static size_t lastCount = 0;
   size_t nCon = 0;
   size_t nMix = 0;
   size_t nAcn = 0;  
   size_t nAmx = 0;  

   if (name.size() != 0) 
   {
        s << name << " is not a valid model component name.\n";       
   }


   size_t cacheSize = s_nameCache.size();

   if (cacheSize != lastCount)
   {
        // then iterate through the map and build the lists of different model types.
        NameCacheType::const_iterator  comp = s_nameCache.begin();
        NameCacheType::const_iterator  compEnd = s_nameCache.end();
        addMods.clear();
        mulMods.clear();
        conMods.clear();
        mixMods.clear();
        acnMods.clear();
        amxMods.clear();
        while ( comp != compEnd)
        {
                const ComponentInfo& current = comp->second;
                string name = current.name;
                if (!current.isStandardXspec)
                {
                   // Append local user components with a '*' in printout.
                   name += "*";
                }
                if ( current.type == "add")
                {
                        addMods.push_back(name);
                }
                else if ( current.type == "mul" )
                {
                        mulMods.push_back(name);
                } 
                else if (current.type == "con")
                {
                        conMods.push_back(name);
                }       
                else if (current.type == "mix")
                {
                        mixMods.push_back(name);
                } 
                else if (current.type == "acn")
                {
                        acnMods.push_back(name);
                }
                else if (current.type == "amx")
                {
                        amxMods.push_back(name);
                } 
                ++comp;       
        }

        nCon = conMods.size();
        nMix = mixMods.size();
        nAcn = acnMods.size();             
        nAmx = amxMods.size();             
        sort(addMods.begin(),addMods.end());
        sort(mulMods.begin(),mulMods.end());
        if (nMix) sort(mixMods.begin(),mixMods.end());
        if (nCon) sort(conMods.begin(),conMods.end());
        if (nAcn) sort(acnMods.begin(),acnMods.end());
        if (nAmx) sort(amxMods.begin(),amxMods.end());
        lastCount = s_nameCache.size();
   }        


   const int LINE = 6;
   const int WIDTH = 12;

   s.setf(std::ios_base::left);
   s << " Additive Models: \n";

   XSutility::printStrings(addMods,s,LINE,WIDTH);

   s << "\n Multiplicative Models: \n";

   XSutility::printStrings(mulMods,s,LINE,WIDTH);

   if (conMods.size())
   {
        s << "\n Convolution Models: \n";
        XSutility::printStrings(conMods,s,LINE,WIDTH);
   }      

   if (mixMods.size())
   {
        s << "\n Mixing Models: \n";
        XSutility::printStrings(mixMods,s,LINE,WIDTH);
   }

   if (acnMods.size())
   {
        s << "\n Pile-up Models: \n";
        XSutility::printStrings(acnMods,s,LINE,WIDTH);
   }

   if (amxMods.size())
   {
        s << "\n Mixing pile-up Models: \n";
        XSutility::printStrings(amxMods,s,LINE,WIDTH);
   }

   s.setf(std::ios_base::right);

   s << "\n Additional models are available at :";
   s << "\n\t legacy.gsfc.nasa.gov/docs/xanadu/xspec/newmodels.html";
   s << endl; 
}

std::vector<ComponentInfo> XSModelFunction::nameCache (const string& fullName)
{
  // entries in the name cache have two letter keys, so there is multiplicity.
  if (fullName.size() < 2)
  {
        throw XSparse::InputError("Model name must be at least two characters");         
  }
  const string abbrev = XSutility::lowerCase(fullName.substr(0,2));
  size_t n (s_nameCache.count(abbrev)); 
  if (n == 0) throw NoSuchComponent();
  std::vector<ComponentInfo> matches(n);
  std::pair<NameCacheType::iterator,NameCacheType::iterator> ff(s_nameCache.equal_range(abbrev));

  NameCacheType::const_iterator m(ff.first);
  size_t index(0);
  while (m != ff.second)
  {
        matches[index] = m->second;
        ++m;
        ++index;       
  }
  return matches;  
}

ComponentInfo XSModelFunction::fullMatch (const string& fullName)
{
  // so far we know all of the matches start with the same two letters as 
  // fullname.

  std::vector<ComponentInfo> matches(nameCache(fullName));      
  size_t m(fullName.size());
  size_t n(matches.size());
  size_t i(0);
  while ( i < n )
  {
        if (m <= matches[i].name.size()) 
        {
                // example: take bexriv as the intended model.
                // if the users type anything less than 'bexri' they
                // will get bexra, because both will be in the matched list.
                // but it should return bexrav correctly.
                // the test should guarantee a match of "bbody" against anything
                // shorter than "bbodyr".

                if (XSutility::lowerCase(fullName) == 
                        XSutility::lowerCase(matches[i]. name.substr(0,m))) 
                        return matches[i];

        }
        ++i;
  }
  throw NoSuchComponent();    
}

NameCacheType::iterator XSModelFunction::exactMatch (const string& fullName)
{
  const string abbrev = XSutility::lowerCase(fullName.substr(0,2));
  std::pair<NameCacheType::iterator,NameCacheType::iterator> ff(s_nameCache.equal_range(abbrev));
  NameCacheType::iterator match = s_nameCache.end();
  NameCacheType::iterator it = ff.first;
  while (it != ff.second)
  {
     if (XSutility::lowerCase(fullName) == XSutility::lowerCase(it->second.name))
     {
        match = it;
        break;
     }
     ++it;
  }
  return match;
}
XSModelFunction::~XSModelFunction() {}
