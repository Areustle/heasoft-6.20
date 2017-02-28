#include <XSUtil/Utils/ProcessManager.h>
#include <XSUtil/Error/Error.h>
#include <XSUtil/Utils/XSutility.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <sstream>
#include <iostream>
#include <algorithm>
#include <poll.h>
#include <signal.h>

size_t ProcessManager::s_READY = 100;
size_t ProcessManager::s_QUIT = 0xFFFF;
std::map<string,int> ProcessManager::s_maxProcs;

void ProcessManager::initMaxProcsMap()
{
   s_maxProcs.clear();
   s_maxProcs["leven"] = 1;
   s_maxProcs["error"] = 1;
   s_maxProcs["steppar"] = 1;
   s_maxProcs["walkers"] = 1;
   s_maxProcs["goodness"] = 1;
}

ProcessManager::ProcessManager(ParallelFunc* childFunc, const string& contextName):
   m_processes(),
   m_childFunc(childFunc),
   m_singleProcResults(),
   m_processAssignments(),
   m_context(contextName),
   m_doParallel(false)
{
}

ProcessManager::~ProcessManager()
{
   for (size_t i=0; i<m_processes.size(); ++i)
   {
      if (m_processes[i].fp)
         fclose(m_processes[i].fp);
   }
   delete m_childFunc;
}

void ProcessManager::createProcesses(const int nRequestedProcs)
{
   int maxProcs = 1;
   std::map<string,int>::const_iterator itMaxProcs = s_maxProcs.find(m_context);
   if (itMaxProcs != s_maxProcs.end())
      maxProcs = itMaxProcs->second;
   const int nProcs = std::min(nRequestedProcs, maxProcs);
   m_doParallel = (nProcs > 1);
   if (m_doParallel)
   {
      try
      {
         for (int iProc=0; iProc<nProcs; ++iProc)
         {
            int fd[2];
            if (socketpair(AF_UNIX, SOCK_STREAM, 0, fd) < 0)
            {
               std::ostringstream oss;
               oss << "Unable to create socketpair " << iProc << "\n";
               throw YellowAlert(oss.str());
            }
            pid_t pid = fork();
            if (pid < 0 )
            {
               close(fd[0]);
               close(fd[1]);
               std::ostringstream oss;
               oss << "Unable to create process " << iProc << "\n";
               throw YellowAlert(oss.str());
            }
            if (pid == 0)
            {
               // Child process.
               // Exceptions must NEVER propogate out of this block.
               if (close(fd[0]) < 0)
               {
                  std::cerr << "Error closing child process socketpair "
                               << iProc << "\n";
                  exit(-1);
               }
               FILE *fpc = fdopen(fd[1], "w+");
               if (!fpc)
               {
                  std::cerr<< "Error opening stream in process "<<iProc<<"\n";
                  close(fd[1]);
                  exit(-1);
               }
               fwrite(&s_READY, sizeof(size_t), 1, fpc);
               fflush(fpc);

               while (1)
               {
                  // *** BLOCK HERE while waiting for signal from parent. *** 
                  //   This will come sometime AFTER parent process has exited
                  //   createProcesses function.           
                  size_t execInfo[2];
                  size_t nRead = fread(execInfo, sizeof(size_t), 2, fpc);
                  if (nRead != 2)
                  {
                     std::cerr << "Error waiting for execution message in process " 
                            << iProc << "\n";
                     fclose(fpc);
                     exit(-1);
                  }
                  if (execInfo[1] == s_QUIT)
                  {
                     fclose(fpc);
                     exit(0);
                  }
                  else if (execInfo[1])
                  {
                     // Must perform receive/send calls in the same order
                     //  of the matching send/receive calls in the parent.
                     std::vector<TransferStruct> input(execInfo[1]);
                     for (size_t iExec=0; iExec<execInfo[1]; ++iExec)
                     {
                        transferReceive(input[iExec], fpc);
                     }
                     bool stopExecutionLoop=false;
                     for (size_t iExec=0; iExec<execInfo[1]; ++iExec)
                     {
                        TransferStruct results;
                        if (stopExecutionLoop)
                           results.status = -1;
                        else
                        {
                           try
                           {
                              m_childFunc->execute(true, input[iExec], results);
                           }
                           catch(...)
                           {
                              if (results.status<0)
                              {
                                 // Do not try and execute remaining cases.
                                 // Just set their status flags to error and
                                 //  let parent process deal with it.
                                 stopExecutionLoop=true;
                              }
                           }
                        }
                        transferSend(results, fpc);
                     }
                  }
               } // end message loop                                    
            } // end child process
            else
            {
               // parent
               if (close(fd[1]) < 0)
               {
                  kill(pid,SIGTERM);
                  waitpid(pid, (int *)0, 0);
                  std::ostringstream oss;
                  oss << "Error closing parent process socketpair " 
                      << iProc << "\n";
                  throw YellowAlert(oss.str());
               }
               Process proc;
               proc.pid = pid;
               proc.fd = fd[0];
               proc.fp = fdopen(fd[0],"w+");
               proc.resultsPending = false;
               if (!proc.fp)
               {
                  close(proc.fd);
                  kill(pid,SIGTERM);
                  waitpid(pid, (int *)0, 0);
                  string errMsg("Error opening stream in parent process\n");
                  throw YellowAlert(errMsg);
               }
               m_processes.push_back(proc);
            }
         } // end nProcs loop
         // If we made it here, everything succeeded from the parent process's
         //   perspective, but did anything go wrong in any of the child 
         //   processes (as indicated by an EOF during fread)?
         for (size_t i=0; i<m_processes.size(); ++i)
         {
            size_t msg=0;
            if (fread(&msg, sizeof(size_t), 1, m_processes[i].fp) != 1
                   || msg != s_READY)
            {
               // Assume faulty child proc already printed a message
               throw YellowAlert();
            }
         }
      }
      catch (...)
      {
         for (size_t i=0; i<m_processes.size(); ++i)
         {
            Process& proc = m_processes[i];
            fclose(proc.fp);
            kill(proc.pid, SIGTERM);
         }
         for (size_t i=0; i<m_processes.size(); ++i)
            waitpid(m_processes[i].pid, (int *)0, 0);
         m_processes.clear();
         throw;
      }
   } // end if doParallel
} // end createProcesses

void ProcessManager::run(const std::vector<TransferStruct>& input, 
                        ParallelResults& output)
{
   m_singleProcResults.clear();
   m_processAssignments.clear();
   
   // Note that input.size() need not be the same each time this is
   //  called.  An example where it changes is when parameters get
   //  pegged during derivative calculations, which reduces input.size().
   
   if (m_doParallel)
      multiProcesses(input);
   else
      singleProcess(input);
      
   getResults(output); 
}

void ProcessManager::singleProcess(const std::vector<TransferStruct>& input)
{
   // Rather than execute N processes, execute loop N times
   // in one process.
   const int nLoop = input.size();
   bool stopExecutionLoop=false;
   for (int i=0; i<nLoop; ++i)
   {
      TransferStruct& output = m_singleProcResults[i];
      if (stopExecutionLoop)
         output.status = -1;
      else
      {
         try
         {
            m_childFunc->execute(false, input[i], output);
         }
         catch (...)
         {
            if (output.status < 0)
               stopExecutionLoop = true;
         }
      }
   }
}

void ProcessManager::multiProcesses(const std::vector<TransferStruct>& input)
{
   
   const size_t nTotExec = input.size();
   setProcessAssignments(nTotExec);
   startChildProcs();
   
   const size_t nActive = m_processAssignments.size() - 1;
   for (size_t iProc=0; iProc<nActive; ++iProc)
   {
      // nActive may be less than m_processes.size(),
      // but it can never be larger.
      const Process& proc = m_processes[iProc];
      for (size_t iExec=m_processAssignments[iProc];
                  iExec<m_processAssignments[iProc+1]; ++iExec)
      {
         transferSend(input[iExec], proc.fp);
      }
   }
   
}

void ProcessManager::getResults(ParallelResults& results)
{
   results.clear();
   if (m_doParallel)
   {
      // Do not return until ALL child processes have finished.
      const size_t nOrigActiveProcs = m_processAssignments.size() - 1;
      size_t nActiveProcs = nOrigActiveProcs;
      while (nActiveProcs)
      {
         XSutility::auto_array_ptr<pollfd> apFds(new pollfd[nActiveProcs]);
         pollfd* fdarray = apFds.get();
         // activeIndices will be needed later to match iActive values
         // with their corresponding iProc.
         std::vector<size_t> activeIndices;
         size_t iActive=0;
         for (size_t iProc=0; iProc<nOrigActiveProcs; ++iProc)
         {
            if (m_processes[iProc].resultsPending)
            {
               fdarray[iActive].fd = m_processes[iProc].fd;
               fdarray[iActive].events = POLLRDNORM;
               activeIndices.push_back(iProc);
               ++iActive;
            }
         }
         if (iActive != nActiveProcs)
         {
            throw RedAlert("Active process count mismatch.");
         }

         size_t nRetrieved=0;
         // This will keep waiting until 1 or more processes are ready.
         poll(fdarray, static_cast<nfds_t>(nActiveProcs), -1);
         for (size_t iActive=0; iActive<nActiveProcs; ++iActive)
         {
            if (fdarray[iActive].revents)
            {
               size_t iProc = activeIndices[iActive];
               Process& proc = m_processes[iProc];
               if (fdarray[iActive].revents & POLLRDNORM)
               {
                  const int startExec=m_processAssignments[iProc];
                  const int endExec=m_processAssignments[iProc+1];
                  for (int iExec=startExec; iExec<endExec; ++iExec)
                  {
                     // Insert an empty transfer structure into results map,
                     //   then proceed to fill by reading results from child.
                     TransferStruct& transfer = results[iExec];
                     transferReceive(transfer, proc.fp);
                  }
                  proc.resultsPending = false;
                  ++nRetrieved;
               }
               else
               {
                  // Polling error must have occurred.
                  
                  // Throw???              
               }

            }
         } // end active process loop
         nActiveProcs -= nRetrieved;
      } // end while active processes loop
   }
   else // Not parallel - no child processes.
   {
      results = m_singleProcResults;
   }
}

void ProcessManager::setProcessAssignments(const size_t inputVecSize)
{
   if (m_doParallel)
   {
      const size_t availableProcs = m_processes.size();
      if (inputVecSize <= availableProcs)
      {
         // Each active process calls execute just once.
         m_processAssignments.resize(inputVecSize+1);
         for (size_t i=0; i<m_processAssignments.size(); ++i)
            m_processAssignments[i] = i;
      }
      else
      {
         m_processAssignments.resize(availableProcs+1);
         const size_t nExecPerProc = inputVecSize/availableProcs;
         const size_t nRemainder = inputVecSize % availableProcs;
         size_t iExec=0;
         for (size_t i=0; i<m_processAssignments.size(); ++i)
         {
            m_processAssignments[i] = iExec;
            iExec += nExecPerProc;
            if (i < nRemainder)
               iExec += 1;
         }         
      }
   }
} // end setProcessAssignments

void ProcessManager::startChildProcs()
{
   // Note that the number of available processes may be larger than 
   //  m_ProcessAssignments vector. Not all processes may need to 
   //  execute during a given run.  Those that aren't needed will
   //  be sent 0 as the second element of execInfo.
   const size_t nActive = m_processAssignments.size() - 1;
   for (size_t i=0; i<m_processes.size(); ++i)
   {
      Process& proc = m_processes[i];
      size_t execInfo[2];
      execInfo[0] = i;
      execInfo[1] = (i < nActive) ? 
                m_processAssignments[i+1] - m_processAssignments[i] : 0;      
      fwrite(execInfo, sizeof(size_t), 2, proc.fp);
      fflush(proc.fp);
      proc.resultsPending = true;
   }
   
} // end startChildProcs

void ProcessManager::killProcesses()
{
   for (size_t i=0; i<m_processes.size(); ++i)
   {
      Process& proc = m_processes[i];
      size_t execInfo[2];
      execInfo[0] = i;
      execInfo[1] = s_QUIT;
      fwrite(execInfo, sizeof(size_t), 2, proc.fp);
      fclose(proc.fp);
      waitpid(proc.pid, (int *)0, 0);
   }
   m_processes.clear();
   m_processAssignments.clear();
   m_doParallel = false;
}

void ProcessManager::transferSend(const TransferStruct& outgoing, FILE* fp)
{
   int status = outgoing.status;
   fwrite(&status, sizeof(int), 1, fp);
   
   size_t nArrays = outgoing.iValues.size();
   fwrite(&nArrays, sizeof(size_t), 1, fp);
   fflush(fp);
   for (size_t iVec=0; iVec<nArrays; ++iVec)
   {
      const std::vector<int>& intVect = outgoing.iValues[iVec];
      size_t nLength = intVect.size();
      fwrite(&nLength, sizeof(size_t), 1, fp);
      if (nLength)
      {
         fwrite(&intVect[0], sizeof(int), nLength, fp);
         fflush(fp);
      }
   }

   nArrays = outgoing.dValues.size();
   fwrite(&nArrays, sizeof(size_t), 1, fp);
   fflush(fp);
   for (size_t iVec=0; iVec<nArrays; ++iVec)
   {
      const std::vector<double>& dVect = outgoing.dValues[iVec];
      size_t nLength = dVect.size();
      fwrite(&nLength, sizeof(size_t), 1, fp);
      if (nLength)
      {
         fwrite(&dVect[0], sizeof(double), nLength, fp);
         fflush(fp);
      }
   }
   
   nArrays = outgoing.sValues.size();
   fwrite(&nArrays, sizeof(size_t), 1, fp);
   fflush(fp);
   for (size_t iVec=0; iVec<nArrays; ++iVec)
   {
      const std::string& str = outgoing.sValues[iVec];
      string::size_type len = str.length();
      fwrite(&len, sizeof(string::size_type), 1, fp);
      fflush(fp);
      if (len)
      {
         // This does not send a terminating 0.
         fwrite(str.c_str(), sizeof(char), len, fp);
         fflush(fp);
      }
   }
}  // end transferSend

void ProcessManager::transferReceive(TransferStruct& incoming, FILE* fp)
{
   fread(&incoming.status, sizeof(int), 1, fp);
   
   size_t nArrays=0;
   fread(&nArrays, sizeof(size_t), 1, fp);
   incoming.iValues.resize(nArrays);
   for (size_t iVec=0; iVec<nArrays; ++iVec)
   {
      size_t nLength=0;
      fread(&nLength, sizeof(size_t), 1, fp);
      if (nLength)
      {
         std::vector<int>& newVec = incoming.iValues[iVec];
         newVec.resize(nLength);
         fread(&newVec[0], sizeof(int), nLength, fp);
      }
   }
   
   nArrays=0;
   fread(&nArrays, sizeof(size_t), 1, fp);
   incoming.dValues.resize(nArrays);
   for (size_t iVec=0; iVec<nArrays; ++iVec)
   {
      size_t nLength=0;
      fread(&nLength, sizeof(size_t), 1, fp);
      if (nLength)
      {
         std::vector<double>& newVec = incoming.dValues[iVec];
         newVec.resize(nLength);
         fread(&newVec[0], sizeof(double), nLength, fp);
      }
   }
   
   nArrays=0;
   fread(&nArrays, sizeof(size_t), 1, fp);
   incoming.sValues.resize(nArrays);
   for (size_t iVec=0; iVec<nArrays; ++iVec)
   {
      size_t nLength=0;
      fread(&nLength, sizeof(size_t), 1, fp);
      if (nLength)
      {
         char* inChars = new char[nLength];
         fread(inChars, sizeof(char), nLength, fp);
         // inChars will NOT have a terminating 0.
         incoming.sValues[iVec] = string(inChars, nLength);
         delete [] inChars;
      }
   }
} // end transferReceive

