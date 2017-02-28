proc gToolServer { args } {
   return [uplevel #0 ToolServer #auto $args]
}

itcl::class ToolServer {
   constructor { cmd } { set currentFtool $cmd }
   destructor {}

   public {
      method runTool { cmdLine }
      method readChannel { }
      method readErrorChannel { }
      method writeChannel { data }
      method flushChannel { }
      method closeObj { }
      method shutdownToolServer { } { itcl::delete object $this }
   }

   private {
      variable toolChannel
      variable errorChannel
      variable currentFtool
   }
}
########################################################################

itcl::body ToolServer::destructor {} {
   if { [info exists toolChannel] } {
      close $toolChannel
      close $errorChannel
   }
}

itcl::body ToolServer::runTool { cmdLine } {
   global env tcl_platform
   global oldHOME oldPFILES

   if { [info exists toolChannel] } {
      close $toolChannel
      close $errorChannel
      unset toolChannel
   }

   set oldHOME $env(HOME)
   set oldPFILES $env(PFILES)
   if { $tcl_platform(platform) == "windows" } {
      if { [file exists $env(PFILES)/${currentFtool}.par] && \
           $env(PFILES) != "$env(CYGWIN_HOME)/pfiles" } {
         file copy -force $env(PFILES)/${currentFtool}.par $env(CYGWIN_HOME)/pfiles/${currentFtool}.par
      }
      set ::env(HOME) $env(CYGWIN_BASH_HOME)
      set ::env(PFILES) $env(CYGWIN_BASH_HOME)/pfiles
      cd $env(ORIGINAL_PWD)
      set errorChannel [open "| $env(CYGWIN_ROOT)/bin/cat.exe" r+]
      set toolChannel [open "| $env(CYGWIN_ROOT)/bin/bash.exe -c {$env(CYGWIN_BASH_LHEASOFT)/bin/spawnToolCmds.csh $cmdLine} 2>@ $errorChannel" r+]
   } else {
      set errorChannel [open "| /bin/cat" r+]
      set toolChannel [open "| $env(LHEASOFT)/bin/spawnToolCmds.csh $cmdLine 2>@ $errorChannel" r+]
   }

   fileevent $errorChannel readable [itcl::code $this readErrorChannel]
   fconfigure $errorChannel -buffering none -blocking false
   fileevent $toolChannel readable [itcl::code $this readChannel]
   fconfigure $toolChannel -buffering none -blocking false
   return
}

itcl::body ToolServer::readChannel { } {
   # We aren't blocking the channel, so pause
   # just a little in case there is a big block being sent

   after 150
   set output [read $toolChannel]
   if { [eof $toolChannel] } {
      close $toolChannel
      close $errorChannel
      unset toolChannel
      [gNotifications default] postMessage $this "toolHasFinished NONE"
   }
   if { $output != "" } {
      [gNotifications default] postMessage $this "toolHasOutputAvailable $output"
   }
}

itcl::body ToolServer::readErrorChannel { } {
   # We aren't blocking the channel, so pause
   # just a little in case there is a big block being sent

   after 150
   set output [read $errorChannel]
   if { [eof $errorChannel] } {
      close $errorChannel
   }
   if { $output != "" } {
      [gNotifications default] postMessage $this "toolHasErrorAvailable $output"
   }
}

itcl::body ToolServer::writeChannel { data } {
   puts $toolChannel $data
}

itcl::body ToolServer::flushChannel {} {
   catch { flush $toolChannel } err
}

itcl::body ToolServer::closeObj { } {
   delete object $this
}
